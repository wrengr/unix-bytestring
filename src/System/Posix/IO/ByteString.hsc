{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -Wall -fwarn-tabs #-}
----------------------------------------------------------------
--                                                    2011.03.04
-- |
-- Module      :  System.Posix.IO.ByteString
-- Copyright   :  Copyright (c) 2010--2011 wren ng thornton
-- License     :  BSD
-- Maintainer  :  wren@community.haskell.org
-- Stability   :  provisional
-- Portability :  non-portable (requires POSIX)
--
-- Provides strict 'ByteString' versions of the "System.Posix.IO"
-- file-descriptor based I\/O API.
----------------------------------------------------------------
module System.Posix.IO.ByteString
    (
    -- * I\/O with file descriptors
      fdRead
    , fdWrite
    , fdWrites
    , fdWritev
    ) where

import           Data.Word                (Word8)
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Unsafe   as BSU

import           System.Posix.Types       (Fd, ByteCount)
import qualified System.Posix.IO          as Posix
import qualified System.IO.Error          as IOE

import           Foreign.Ptr              (Ptr)
import qualified Foreign.Ptr              as FFI (castPtr, plusPtr)
import qualified Foreign.ForeignPtr       as FFP
import qualified Foreign.Marshal.Array    as FMA
import           Foreign.C.Types          (CInt, CSize)
import qualified Foreign.C.Error          as FFI (throwErrnoIfMinus1Retry)
import           Foreign.Storable         (Storable(..))

----------------------------------------------------------------
-- | Read data from an 'Fd' and convert it to a 'BS.ByteString'.
-- Throws an exception if this is an invalid descriptor, or EOF has
-- been reached.
fdRead
    :: Fd
    -> ByteCount                     -- ^ How many bytes to try to read.
    -> IO (BS.ByteString, ByteCount) -- ^ The bytes read, and how many
                                     --   bytes were actually read.
fdRead _  0 = return (BS.empty, 0)
fdRead fd n = do
    s <- BSI.createAndTrim (fromIntegral n) $ \buf -> do
        rc <- Posix.fdReadBuf fd buf n
        if 0 == rc
            then IOE.ioError
                (IOE.ioeSetErrorString
                    (IOE.mkIOError IOE.eofErrorType "fdRead" Nothing Nothing)
                    "EOF")
            else return (fromIntegral rc)
    let rc = fromIntegral (BS.length s) in rc `seq` do
    return (s, rc)


----------------------------------------------------------------
-- | Write a 'BS.ByteString' to an 'Fd'. The return value is the
-- total number of bytes actually written. You can compare the total
-- bytes written to the length of the bytestring in order to determine
-- if the whole string was written or not.
fdWrite :: Fd -> BS.ByteString -> IO ByteCount
fdWrite fd s =
    -- N.B., BSU.unsafeUseAsCStringLen does zero copying. Use
    -- BS.useAsCStringLen if there's any chance Posix.fdWriteBuf
    -- might alter the buffer.
    BSU.unsafeUseAsCStringLen s $ \(buf,len) -> do
        Posix.fdWriteBuf fd (FFI.castPtr buf) (fromIntegral len)


----------------------------------------------------------------
-- | Write a sequence of 'BS.ByteString' to an 'Fd'. The return
-- value is a triple of: the total number of bytes written, the
-- number of bytes written from the head of the remaining input,
-- and the remaining (unwritten) input. We return this triple instead
-- of a pair adjusting the head of the remaining input (i.e.,
-- removing the bytes already written) in case there is some semantic
-- significance to the way the input is split into chunks.
--
-- This version consumes the list lazily and will call the @write@
-- system call once for each bytestring. This laziness allows the
-- early parts of the list to be garbage collected and prevents
-- needing to hold the whole list of bytestrings in memory at once.
-- Compare against 'fdWritev'.
fdWrites :: Fd -> [BS.ByteString] -> IO (ByteCount, ByteCount, [BS.ByteString])
fdWrites fd = go 0
    where
    -- We want to do a left fold in order to avoid stack overflows,
    -- but we need to have an early exit for incomplete writes
    -- (which normally requires a right fold). Hence this recursion.
    go acc []         = return (acc, 0, [])
    go acc ccs@(c:cs) = do
        rc <- fdWrite fd c
        let acc' = acc+rc in acc' `seq` do
        if rc == fromIntegral (BS.length c)
            then go acc' cs
            else return (acc', rc, ccs)


----------------------------------------------------------------
#include <sys/uio.h>

-- | Haskell type representing the C @struct iovec@ type. This is
-- like 'Foreign.C.String.CStringLen' except there's actually struct
-- definition on the C side.
data CIovec = CIovec
    { iov_base :: {-# UNPACK #-} !(Ptr Word8) -- char* or void*
    , iov_len  :: {-# UNPACK #-} !CSize       -- size_t
    }

#let alignment t = \
    "%lu", (unsigned long) offsetof(struct {char x__; t (y__); }, y__)

instance Storable CIovec where
    alignment _ = #{alignment struct iovec}
    
    sizeOf _    = #{size struct iovec}
    
    peek ptr = do
        base <- #{peek struct iovec, iov_base} ptr
        len  <- #{peek struct iovec, iov_len} ptr
        return (CIovec base len)
    
    poke ptr (CIovec base len) = do
        #{poke struct iovec, iov_base} ptr base
        #{poke struct iovec, iov_len}  ptr len


-- | Convert a bytestring to an @iovec@. Thus function is unsafe
-- because the @iovec@ contains a pointer into the bytestring, but
-- this pointer will not keep the bytestring alive! You must use
-- 'touchByteString' to keep the bytestring alive until you are
-- done with the @CIovec@. Also, it's unsafe because foreign code
-- could mutate the pointer's contents.
unsafeByteString2CIovec :: BS.ByteString -> CIovec
unsafeByteString2CIovec (BSI.PS fptr offset len) =
    CIovec
        (FFP.unsafeForeignPtrToPtr fptr `FFI.plusPtr` offset)
        (fromIntegral len)
{-# INLINE unsafeByteString2CIovec #-}


-- | Keep the bytestring alive.
touchByteString :: BS.ByteString -> IO ()
touchByteString (BSI.PS fptr _ _) = FFP.touchForeignPtr fptr
{-# INLINE touchByteString #-}


-- | Convert a bytestring into an @iovec@.
withByteString :: BS.ByteString -> (CIovec -> IO a) -> IO a
withByteString bs io = do
    a <- io (unsafeByteString2CIovec bs)
    touchByteString bs
    return a
{-# INLINE withByteString #-}


foreign import ccall safe "writev"
-- ssize_t writev(int fildes, const struct iovec *iov, int iovcnt);
    c_safe_writev :: CInt -> Ptr CIovec -> CInt -> IO CSize


-- | Write data from memory to an 'Fd'. This is exactly equivalent
-- to the POSIX @writev@ function.
fdWritevBuf :: Fd -> Ptr CIovec -> CInt -> IO ByteCount
fdWritevBuf _  _   0   = return 0
fdWritevBuf fd buf len =
    fmap fromIntegral $
        FFI.throwErrnoIfMinus1Retry "fdWritevBuf" $
            c_safe_writev (fromIntegral fd) buf len


-- | Write a sequence of 'BS.ByteString' to an 'Fd'. The return
-- value is the total number of bytes written. Unfortunately the
-- @writev@ system call does not provide enough information to
-- return the triple that 'fdWrites' does.
--
-- This version will force the spine of the list, convert each
-- bytestring into an @iovec@, and then call the @writev@ system
-- call. This means we only make one system call, which reduces the
-- overhead of performing context switches. But it also means that
-- we must store the whole list of bytestrings in memory at once,
-- and that we must perform some allocation and conversion. Compare
-- against 'fdWrites'.
fdWritev :: Fd -> [BS.ByteString] -> IO ByteCount
fdWritev fd cs = do
    rc <- FMA.withArrayLen (map unsafeByteString2CIovec cs) $ \len iovs ->
        fdWritevBuf fd iovs (fromIntegral len)
    -- BUG: is this enough to actually hold onto them?
    mapM_ touchByteString cs
    return rc

----------------------------------------------------------------
----------------------------------------------------------- fin.

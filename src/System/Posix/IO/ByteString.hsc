{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -Wall -fwarn-tabs -fno-warn-unused-binds #-}
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
-- Provides strict 'BS.ByteString' versions of the "System.Posix.IO"
-- file-descriptor based I\/O API.
----------------------------------------------------------------
module System.Posix.IO.ByteString
    (
    -- * I\/O with file descriptors
      fdRead
    , fdWrite
    , fdWrites
    , fdWritev
    -- , fdWritevBuf
    ) where

import qualified Data.ByteString          as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Unsafe   as BSU

import           System.Posix.Types.Iovec
import           System.Posix.Types       (Fd, ByteCount)
import qualified System.Posix.IO          as Posix
import qualified System.IO.Error          as IOE

import           Foreign.Ptr              (Ptr)
import qualified Foreign.Ptr              as FFI (castPtr)
import qualified Foreign.Marshal.Array    as FMA
import           Foreign.C.Types          (CInt, CSize)
import qualified Foreign.C.Error          as FFI (throwErrnoIfMinus1Retry)

----------------------------------------------------------------
-- | Read data from an 'Fd' and convert it to a 'BS.ByteString'.
-- Throws an exception if this is an invalid descriptor, or EOF has
-- been reached.
fdRead
    :: Fd
    -> ByteCount
        -- ^ How many bytes to try to read.
    -> IO (BS.ByteString, ByteCount)
        -- ^ The bytes read, and how many bytes were actually read.
fdRead _  0 = return (BS.empty, 0)
fdRead fd n = do
    s <- BSI.createAndTrim (fromIntegral n) $ \buf -> do
        rc <- Posix.fdReadBuf fd buf n
        if 0 == rc
            then IOE.ioError
                (IOE.ioeSetErrorString
                    (IOE.mkIOError
                        IOE.eofErrorType
                        "System.Posix.IO.ByteString.fdRead"
                        Nothing
                        Nothing)
                    "EOF")
            else return (fromIntegral rc)
    let rc = fromIntegral (BS.length s) in rc `seq` do
    return (s, rc)

-- TODO: since it's /O(1)/ to get the length from the bytestring,
-- and we do that anyways, would it be worthwhile to push that off
-- onto clients and just return the bytestring alone?


----------------------------------------------------------------
-- | Write a 'BS.ByteString' to an 'Fd'. The return value is the
-- total number of bytes actually written. This is exactly equivalent
-- to the POSIX @write(2)@ system call; we just convert the
-- @ByteString@ into its underlying @Ptr Word8@ and @ByteCount@
-- components for passing to 'Posix.fdWriteBuf'.
fdWrite
    :: Fd
    -> BS.ByteString -- ^ The string to write.
    -> IO ByteCount  -- ^ How many bytes were actually written.
fdWrite fd s =
    -- N.B., BSU.unsafeUseAsCStringLen does zero copying. Use
    -- BS.useAsCStringLen if there's any chance Posix.fdWriteBuf
    -- might alter the buffer.
    BSU.unsafeUseAsCStringLen s $ \(buf,len) -> do
        Posix.fdWriteBuf fd (FFI.castPtr buf) (fromIntegral len)

-- TODO: Should we have a version that returns the remaining unwritten
-- string as well (for convenience)?


----------------------------------------------------------------
-- | Write a sequence of 'BS.ByteString's to an 'Fd'. The return
-- value is a triple of: the total number of bytes written, the
-- number of bytes written from the first of the remaining strings,
-- and the remaining (unwritten) strings. We return this triple
-- instead of a pair adjusting the head of the remaining strings
-- (i.e., removing the bytes already written) in case there is some
-- semantic significance to the way the input is split into chunks.
--
-- This version consumes the list lazily and will call the @write(2)@
-- system call once for each @ByteString@. This laziness allows the
-- early parts of the list to be garbage collected and prevents
-- needing to hold the whole list of @ByteString@s in memory at
-- once. Compare against 'fdWritev'.
fdWrites
    :: Fd
    -> [BS.ByteString]
        -- ^ The strings to write.
    -> IO (ByteCount, ByteCount, [BS.ByteString])
        -- ^ The total number of bytes written, the number of bytes
        -- written from the first of the remaining strings, the
        -- remaining (unwritten) strings.
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
-- For struct iovec, writev, readv
#include <sys/uio.h>

foreign import ccall safe "writev"
-- ssize_t writev(int fildes, const struct iovec *iov, int iovcnt);
    c_safe_writev :: CInt -> Ptr CIovec -> CInt -> IO CSize


-- | Write data from memory to an 'Fd'. This is exactly equivalent
-- to the POSIX @writev(2)@ function.
fdWritevBuf :: Fd -> Ptr CIovec -> CInt -> IO ByteCount
fdWritevBuf _  _   0   = return 0
fdWritevBuf fd buf len =
    fmap fromIntegral $
        FFI.throwErrnoIfMinus1Retry "fdWritevBuf" $
            c_safe_writev (fromIntegral fd) buf len


-- | Write a sequence of 'BS.ByteString's to an 'Fd'. The return
-- value is the total number of bytes written. Unfortunately the
-- @writev(2)@ system call does not provide enough information to
-- return the triple that 'fdWrites' does.
--
-- This version will force the spine of the list, convert each
-- @ByteString@ into an @iovec@, and then call the @writev(2)@
-- system call. This means we only make one system call, which
-- reduces the overhead of performing context switches. But it also
-- means that we must store the whole list of @ByteString@s in
-- memory at once, and that we must perform some allocation and
-- conversion. Compare against 'fdWrites'.
fdWritev
    :: Fd
    -> [BS.ByteString] -- ^ The strings to write.
    -> IO ByteCount    -- ^ How many bytes were actually written.
fdWritev fd cs = do
    rc <- FMA.withArrayLen (map unsafeByteString2CIovec cs) $ \len iovs ->
        fdWritevBuf fd iovs (fromIntegral len)
    -- BUG: is this enough to actually hold onto them?
    mapM_ touchByteString cs
    return rc

----------------------------------------------------------------
----------------------------------------------------------- fin.

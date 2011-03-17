{-
/N.B./, There's a bug when trying to use Cabal-style MIN_VERSION_foo(1,2,3)
macros in combination with hsc2hs. We don't need full hsc2hs support
in this file, but if we use CPP instead we get a strange error on
OSX 10.5.8 about "architecture not supported" (even though the
headers work fine with hsc2hs). It turns out that we don't /need/
to combine Cabal-style macros and hsc2hs\/cpp since we can remove
our dependency on the @unix@ package. But this issue is worth making
a note of.
-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -Wall -fwarn-tabs #-}
----------------------------------------------------------------
--                                                    2011.03.17
-- |
-- Module      :  System.Posix.IO.ByteString
-- Copyright   :  Copyright (c) 2010--2011 wren ng thornton
-- License     :  BSD
-- Maintainer  :  wren@community.haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (POSIX.1, XPG4.2; hsc2hs, FFI)
--
-- Provides a strict-'BS.ByteString' file-descriptor based I\/O
-- API, designed loosely after the @String@ file-descriptor based
-- I\/O API in "System.Posix.IO". The functions here wrap standard
-- C implementations of the functions specified by the ISO\/IEC
-- 9945-1:1990 (``POSIX.1'') and X\/Open Portability Guide Issue
-- 4, Version 2 (``XPG4.2'') specifications.
----------------------------------------------------------------
module System.Posix.IO.ByteString
    (
    -- * I\/O with file descriptors
    -- ** Reading
    -- *** The POSIX.1 @read(2)@ syscall
      fdRead
    , fdReadBuf
    , fdReads
    -- *** The XPG4.2 @readv(2)@ syscall
    -- , fdReadv
    , fdReadvBuf
    -- *** The XPG4.2 @pread(2)@ syscall
    , fdPread
    , fdPreadBuf
    , fdPreads
    
    -- ** Writing
    -- *** The POSIX.1 @write(2)@ syscall
    , fdWrite
    , fdWriteBuf
    , fdWrites
    -- *** The XPG4.2 @writev(2)@ syscall
    , fdWritev
    , fdWritevBuf
    -- *** The XPG4.2 @pwrite(2)@ syscall
    , fdPwrite
    , fdPwriteBuf
    ) where

import           Data.Word                (Word8)
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Unsafe   as BSU

import qualified System.IO.Error          as IOE
{-
-- /N.B./, hsc2hs doesn't like this...
#if (MIN_VERSION_unix(2,4,0))
import           System.Posix.IO          (fdReadBuf, fdWriteBuf)
#endif
-}
import           System.Posix.Types.Iovec
import           System.Posix.Types       (Fd, ByteCount, FileOffset
                                          , CSsize, COff)
import           Foreign.C.Types          (CInt, CSize, CChar)
import qualified Foreign.C.Error          as FFI (throwErrnoIfMinus1Retry)
import           Foreign.Ptr              (Ptr)
import qualified Foreign.Ptr              as FFI (castPtr, plusPtr)
import qualified Foreign.Marshal.Array    as FMA (withArrayLen)

-- iovec, writev, and readv are in <sys/uio.h>, but we must include
-- <sys/types.h> and <unistd.h> for legacy reasons.
#include <sys/types.h>
#include <sys/uio.h>
#include <unistd.h>

----------------------------------------------------------------
-- | Throw an 'IOE.IOError' for EOF.
ioErrorEOF :: String -> IO a
ioErrorEOF fun =
    IOE.ioError
        (IOE.ioeSetErrorString
            (IOE.mkIOError IOE.eofErrorType fun Nothing Nothing)
            "EOF")


----------------------------------------------------------------
-- /N.B./, hsc2hs doesn't like this...
-- #if ! (MIN_VERSION_unix(2,4,0))
-- This version was copied from unix-2.4.2.0
-- | Read data from an 'Fd' into memory. This is exactly equivalent
-- to the POSIX.1 @read(2)@ system call.
--
-- TODO: better documentation.
fdReadBuf
    :: Fd
    -> Ptr Word8    -- ^ Memory in which to put the data.
    -> ByteCount    -- ^ How many bytes to try to read.
    -> IO ByteCount -- ^ How many bytes were actually read (zero for EOF).
fdReadBuf _  _   0      = return 0
fdReadBuf fd buf nbytes = 
    fmap fromIntegral
        $ FFI.throwErrnoIfMinus1Retry
            "System.Posix.IO.ByteString.fdReadBuf"
            $ c_safe_read
                (fromIntegral fd)
                (FFI.castPtr  buf)
                (fromIntegral nbytes)

foreign import ccall safe "read"
    -- ssize_t read(int fildes, void *buf, size_t nbyte);
    c_safe_read :: CInt -> Ptr CChar -> CSize -> IO CSsize
-- #endif


----------------------------------------------------------------
-- | Read data from an 'Fd' and convert it to a 'BS.ByteString'.
-- Throws an exception if this is an invalid descriptor, or EOF has
-- been reached.
--
-- This is essentially equivalent to the POSIX.1 @read(2)@ system
-- call; the differences are that we allocate a byte buffer for the
-- @ByteString@ (and then pass its underlying @Ptr Word8@ and
-- @ByteCount@ components to 'fdReadBuf'), and that we detect EOF
-- and throw an 'IOE.IOError'.
fdRead
    :: Fd
    -> ByteCount        -- ^ How many bytes to try to read.
    -> IO BS.ByteString -- ^ The bytes read.
fdRead _  0 = return BS.empty
fdRead fd n =
    BSI.createAndTrim (fromIntegral n) $ \buf -> do
        rc <- fdReadBuf fd buf n
        if 0 == rc
            then ioErrorEOF "System.Posix.IO.ByteString.fdRead"
            else return (fromIntegral rc)


----------------------------------------------------------------
-- | Read data from an 'Fd' and convert it to a 'BS.ByteString'.
-- Throws an exception if this is an invalid descriptor, or EOF has
-- been reached.
--
-- This version takes a kind of stateful predicate for whether and
-- how long to keep retrying. Assume the function is called as
-- @fdReads f z0 fd n0@. We will attempt to read @n0@ bytes from
-- @fd@. If we fall short, then we will call @f len z@ where @len@
-- is the total number of bytes read so far and @z@ is the current
-- state (initially @z0@). If it returns @Nothing@ then we will
-- give up and return the current buffer; otherwise we will retry
-- with the new state, continuing from where we left off.
--
-- For example, to define a function that tries up to @n@ times,
-- we can use:
--
-- > fdReadUptoNTimes :: Int -> Fd -> ByteCount -> IO ByteString
-- > fdReadUptoNTimes n0
-- >     | n0 <= 0   = \_ _ -> return empty
-- >     | otherwise = fdReads retry n0
-- >     where
-- >     retry _ 0 = Nothing
-- >     retry _ n = Just $! n-1
--
-- The benefit of doing this instead of the naive approach of calling
-- 'fdRead' repeatedly is that we only need to allocate one byte
-- buffer, and trim it once at the end--- whereas the naive approach
-- would allocate a buffer, trim it to the number of bytes read,
-- and then concatenate with the previous one (another allocation,
-- plus copying everything over) for each time around the loop.
fdReads
    :: (ByteCount -> a -> Maybe a) -- ^ A stateful predicate for retrying.
    -> a                           -- ^ An initial state for the predicate.
    -> Fd
    -> ByteCount                   -- ^ How many bytes to try to read.
    -> IO BS.ByteString            -- ^ The bytes read.
fdReads _ _  _  0  = return BS.empty
fdReads f z0 fd n0 = BSI.createAndTrim (fromIntegral n0) (go z0 0 n0)
    where
    go _ len n buf | len `seq` n `seq` buf `seq` False = undefined
    go z len n buf = do
        rc <- fdReadBuf fd buf n
        let len' = len + rc
        case rc of
          _ | rc == 0 -> ioErrorEOF "System.Posix.IO.ByteString.fdReads"
            | rc == n -> return (fromIntegral len') -- Finished.
            | otherwise ->
                case f len' z of
                Nothing -> return (fromIntegral len') -- Gave up.
                Just z' ->
                    go z' len' (n - rc) (buf `FFI.plusPtr` fromIntegral rc)


----------------------------------------------------------------
-- | Read data from an 'Fd' and scatter it into memory. This is
-- exactly equivalent to the XPG4.2 @readv(2)@ system call.
--
-- TODO: better documentation.
fdReadvBuf
    :: Fd
    -> Ptr CIovec   -- ^ A C-style array of buffers to fill.
    -> Int          -- ^ How many buffers there are.
    -> IO ByteCount -- ^ How many bytes were actually read (zero for EOF).
fdReadvBuf _  _    0   = return 0
fdReadvBuf fd bufs len =
    fmap fromIntegral
        $ FFI.throwErrnoIfMinus1Retry
            "System.Posix.IO.ByteString.fdReadvBuf"
            $ c_safe_readv (fromIntegral fd) bufs (fromIntegral len)

foreign import ccall safe "readv"
    -- ssize_t readv(int fildes, const struct iovec *iov, int iovcnt);
    c_safe_readv :: CInt -> Ptr CIovec -> CInt -> IO CSsize


-- TODO: What's a reasonable wrapper for fdReadvBuf to make it Haskellish?

----------------------------------------------------------------
-- | Read data from a specified position in the 'Fd' into memory,
-- without altering the position stored in the @Fd@. This is exactly
-- equivalent to the XPG4.2 @pread(2)@ system call.
--
-- TODO: better documentation.
fdPreadBuf
    :: Fd
    -> Ptr Word8    -- ^ Memory in which to put the data.
    -> ByteCount    -- ^ How many bytes to try to read.
    -> FileOffset   -- ^ Where to read the data from.
    -> IO ByteCount -- ^ How many bytes were actually read (zero for EOF).
fdPreadBuf _  _   0      _      = return 0
fdPreadBuf fd buf nbytes offset =
    fmap fromIntegral
        $ FFI.throwErrnoIfMinus1Retry
            "System.Posix.IO.ByteString.fdPreadBuf"
            $ c_safe_pread
                (fromIntegral fd)
                (FFI.castPtr  buf)
                (fromIntegral nbytes)
                (fromIntegral offset)

foreign import ccall safe "pread"
    -- ssize_t pread(int fildes, void *buf, size_t nbyte, off_t offset);
    c_safe_pread :: CInt -> Ptr Word8 -> CSize -> COff -> IO CSsize


----------------------------------------------------------------
-- | Read data from a specified position in the 'Fd' and convert
-- it to a 'BS.ByteString', without altering the position stored
-- in the @Fd@. Throws an exception if this is an invalid descriptor,
-- or EOF has been reached.
--
-- This is essentially equivalent to the XPG4.2 @pread(2)@ system
-- call; the differences are that we allocate a byte buffer for the
-- @ByteString@ (and then pass its underlying @Ptr Word8@ and
-- @ByteCount@ components to 'fdPreadBuf'), and that we detect EOF
-- and throw an 'IOE.IOError'.
fdPread
    :: Fd
    -> ByteCount        -- ^ How many bytes to try to read.
    -> FileOffset       -- ^ Where to read the data from.
    -> IO BS.ByteString -- ^ The bytes read.
fdPread _  0 _      = return BS.empty
fdPread fd n offset =
    BSI.createAndTrim (fromIntegral n) $ \buf -> do
        rc <- fdPreadBuf fd buf n offset
        if 0 == rc
            then ioErrorEOF "System.Posix.IO.ByteString.fdPread"
            else return (fromIntegral rc)

----------------------------------------------------------------
-- | Read data from a specified position in the 'Fd' and convert
-- it to a 'BS.ByteString', without altering the position stored
-- in the @Fd@. Throws an exception if this is an invalid descriptor,
-- or EOF has been reached. This is a @pread(2)@ based version of
-- 'fdReads'; see that function for more details.
fdPreads
    :: (ByteCount -> a -> Maybe a) -- ^ A stateful predicate for retrying.
    -> a                           -- ^ An initial state for the predicate.
    -> Fd
    -> ByteCount                   -- ^ How many bytes to try to read.
    -> FileOffset                  -- ^ Where to read the data from.
    -> IO BS.ByteString            -- ^ The bytes read.
fdPreads _ _  _  0  _      = return BS.empty
fdPreads f z0 fd n0 offset = BSI.createAndTrim (fromIntegral n0) (go z0 0 n0)
    where
    go _ len n buf | len `seq` n `seq` buf `seq` False = undefined
    go z len n buf = do
        rc <- fdPreadBuf fd buf n (offset + fromIntegral len)
        let len' = len + rc
        case rc of
          _ | rc == 0 -> ioErrorEOF "System.Posix.IO.ByteString.fdPreads"
            | rc == n -> return (fromIntegral len') -- Finished.
            | otherwise ->
                case f len' z of
                Nothing -> return (fromIntegral len') -- Gave up.
                Just z' ->
                    go z' len' (n - rc) (buf `FFI.plusPtr` fromIntegral rc)

----------------------------------------------------------------
----------------------------------------------------------------
-- /N.B./, hsc2hs doesn't like this...
-- #if ! (MIN_VERSION_unix(2,4,0))
-- This version was copied from unix-2.4.2.0
-- | Write data from memory to an 'Fd'. This is exactly equivalent
-- to the POSIX.1 @write(2)@ system call.
--
-- TODO: better documentation.
fdWriteBuf
    :: Fd
    -> Ptr Word8    -- ^ Memory containing the data to write.
    -> ByteCount    -- ^ How many bytes to try to write.
    -> IO ByteCount -- ^ How many bytes were actually written.
fdWriteBuf fd buf nbytes =
    fmap fromIntegral
        $ FFI.throwErrnoIfMinus1Retry
            "System.Posix.IO.ByteString.fdWriteBuf"
            $ c_safe_write
                (fromIntegral fd)
                (FFI.castPtr  buf)
                (fromIntegral nbytes)

foreign import ccall safe "write" 
    -- ssize_t write(int fildes, const void *buf, size_t nbyte);
    c_safe_write :: CInt -> Ptr CChar -> CSize -> IO CSsize
-- #endif


----------------------------------------------------------------
-- | Write a 'BS.ByteString' to an 'Fd'. The return value is the
-- total number of bytes actually written. This is exactly equivalent
-- to the POSIX.1 @write(2)@ system call; we just convert the
-- @ByteString@ into its underlying @Ptr Word8@ and @ByteCount@
-- components for passing to 'fdWriteBuf'.
fdWrite
    :: Fd
    -> BS.ByteString -- ^ The string to write.
    -> IO ByteCount  -- ^ How many bytes were actually written.
fdWrite fd s =
    -- N.B., BSU.unsafeUseAsCStringLen does zero copying. Use
    -- BS.useAsCStringLen if there's any chance fdWriteBuf might
    -- alter the buffer.
    BSU.unsafeUseAsCStringLen s $ \(buf,len) -> do
        fdWriteBuf fd (FFI.castPtr buf) (fromIntegral len)


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
-- | Write data from memory to an 'Fd'. This is exactly equivalent
-- to the XPG4.2 @writev(2)@ system call.
--
-- TODO: better documentation.
fdWritevBuf
    :: Fd
    -> Ptr CIovec   -- ^ A C-style array of buffers to write.
    -> Int          -- ^ How many buffers there are.
    -> IO ByteCount -- ^ How many bytes were actually written.
fdWritevBuf _  _    0   = return 0
fdWritevBuf fd bufs len =
    fmap fromIntegral
        $ FFI.throwErrnoIfMinus1Retry
            "System.Posix.IO.ByteString.fdWritevBuf"
            $ c_safe_writev (fromIntegral fd) bufs (fromIntegral len)

foreign import ccall safe "writev"
    -- ssize_t writev(int fildes, const struct iovec *iov, int iovcnt);
    c_safe_writev :: CInt -> Ptr CIovec -> CInt -> IO CSsize


----------------------------------------------------------------
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
-- | Write data from memory to a specified position in the 'Fd',
-- but without altering the position stored in the @Fd@. This is
-- exactly equivalent to the XPG4.2 @pwrite(2)@ system call.
--
-- TODO: better documentation.
fdPwriteBuf
    :: Fd
    -> Ptr Word8    -- ^ Memory containing the data to write.
    -> ByteCount    -- ^ How many bytes to try to write.
    -> FileOffset   -- ^ Where to write the data to.
    -> IO ByteCount -- ^ How many bytes were actually written.
fdPwriteBuf _  _   0      _      = return 0
fdPwriteBuf fd buf nbytes offset =
    fmap fromIntegral
        $ FFI.throwErrnoIfMinus1Retry
            "System.Posix.IO.ByteString.fdPwriteBuf"
            $ c_safe_pwrite
                (fromIntegral fd)
                (FFI.castPtr  buf)
                (fromIntegral nbytes)
                (fromIntegral offset)

foreign import ccall safe "pwrite"
    -- ssize_t pwrite(int fildes, const void *buf, size_t nbyte, off_t offset);
    c_safe_pwrite :: CInt -> Ptr Word8 -> CSize -> COff -> IO CSsize


----------------------------------------------------------------
-- | Write data from memory to a specified position in the 'Fd',
-- but without altering the position stored in the @Fd@. This is
-- exactly equivalent to the XPG4.2 @pwrite(2)@ system call; we
-- just convert the @ByteString@ into its underlying @Ptr Word8@
-- and @ByteCount@ components for passing to 'fdPwriteBuf'.
fdPwrite
    :: Fd
    -> BS.ByteString -- ^ The string to write.
    -> FileOffset    -- ^ Where to write the data to.
    -> IO ByteCount  -- ^ How many bytes were actually written.
fdPwrite fd s offset =
    -- N.B., BSU.unsafeUseAsCStringLen does zero copying. Use
    -- BS.useAsCStringLen if there's any chance fdPwriteBuf might
    -- alter the buffer.
    BSU.unsafeUseAsCStringLen s $ \(buf,len) -> do
        fdPwriteBuf fd (FFI.castPtr buf) (fromIntegral len) offset

----------------------------------------------------------------
----------------------------------------------------------- fin.

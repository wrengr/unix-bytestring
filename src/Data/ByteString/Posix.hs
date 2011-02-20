{-# OPTIONS_GHC -Wall -fwarn-tabs #-}
----------------------------------------------------------------
--                                                    2010.11.10
-- |
-- Module      :  Data.ByteString.Posix
-- Copyright   :  Copyright (c) 2010--2011 wren ng thornton
-- License     :  BSD
-- Maintainer  :  wren@community.haskell.org
-- Stability   :  provisional
-- Portability :  non-portable (Posix)
--
-- Provides strict 'ByteString' versions of the "System.Posix.IO"
-- file-descriptor based I/O API.
----------------------------------------------------------------
module Data.ByteString.Posix
    (
    -- * I/O with file descriptors
      fdRead
    , fdWrite
    ) where

import qualified Data.ByteString          as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Unsafe   as BSU
import           System.Posix.Types       (Fd, ByteCount)
import qualified System.Posix.IO          as Posix
import qualified System.IO.Error          as IOE
import qualified Foreign.Ptr              as FFI (castPtr)

----------------------------------------------------------------

-- | Read data from an 'Fd' and convert it to a 'BS.ByteString'.
-- Throws an exception if this is an invalid descriptor, or EOF has
-- been reached.
fdRead
    :: Fd
    -> ByteCount                     -- ^ How many bytes to try to read.
    -> IO (BS.ByteString, ByteCount) -- ^ The bytes read, how many
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
    
    {- -- This version is closer to the version for String.
    Foreign.Marshal.Alloc.allocaBytes (fromIntegral n) $ \buf -> do
        rc <- Posix.fdReadBuf fd buf n
        case fromIntegral rc of
            0 -> IOE.ioError
                (IOE.ioeSetErrorString
                    (IOE.mkIOError IOE.eofErrorType "fdRead" Nothing Nothing)
                    "EOF")
            n' -> do
                -- N.B., @buf@ will be freed on exit so we can't use
                -- BSU.unsafePackCStringLen to avoid copying (also that
                -- function would give a result with no finalizer and
                -- which wouldn't be GCed).
                s <- BS.packCStringLen (FFI.castPtr buf, fromIntegral n')
                return (s,n')
    -}


-- | Write a 'BS.ByteString' to an 'Fd'.
fdWrite :: Fd -> BS.ByteString -> IO ByteCount
fdWrite fd s =
    -- N.B., BSU.unsafeUseAsCStringLen does zero copying. Use
    -- B.useAsCStringLen if there's any chance Posix.fdWriteBuf
    -- might alter the buffer.
    BSU.unsafeUseAsCStringLen s $ \(buf,len) -> do
        Posix.fdWriteBuf fd (FFI.castPtr buf) (fromIntegral len)

----------------------------------------------------------------
----------------------------------------------------------- fin.

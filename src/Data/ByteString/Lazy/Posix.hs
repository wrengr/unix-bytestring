{-# OPTIONS_GHC -Wall -fwarn-tabs #-}
----------------------------------------------------------------
--                                                    2010.11.10
-- |
-- Module      :  Data.ByteString.Lazy.Posix
-- Copyright   :  Copyright (c) 2010 wren ng thornton
-- License     :  BSD
-- Maintainer  :  wren@community.haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Provides lazy 'ByteString' versions of the "System.Posix.IO"
-- file-descriptor based I/O API.
----------------------------------------------------------------
module Data.ByteString.Lazy.Posix
    (
    -- * I/O with file descriptors
      fdRead
    , fdWrite
    ) where

import qualified Data.ByteString          as BS
import qualified Data.ByteString.Lazy     as BL
import qualified Data.ByteString.Unsafe   as BU
import qualified Data.ByteString.Internal as BI
import           System.Posix.Types       (Fd, ByteCount)
import qualified System.Posix.IO          as Posix
import qualified System.IO.Error          as IOE
import qualified Foreign.Ptr              as FFI (castPtr)

----------------------------------------------------------------

-- | Read data from an 'Fd' and convert it to a 'BL.ByteString'.
-- Throws an exception if this is an invalid descriptor, or EOF has
-- been reached.
fdRead
    :: Fd
    -> ByteCount                     -- ^ How many bytes to try to read.
    -> IO (BL.ByteString, ByteCount) -- ^ The bytes read, how many
                                     -- bytes were actually read.
fdRead _  0 = return (BL.empty, 0)
fdRead fd n = do
    s <- BI.createAndTrim (fromIntegral n) $ \buf -> do
        rc <- Posix.fdReadBuf fd buf n
        if 0 == rc
            then IOE.ioError
                (IOE.ioeSetErrorString
                    (IOE.mkIOError IOE.eofErrorType "fdRead" Nothing Nothing)
                    "EOF")
            else return (fromIntegral rc)
    return (s, fromIntegral (BL.length s))


-- | Write a 'BL.ByteString' to an 'Fd'.
fdWrite :: Fd -> BL.ByteString -> IO ByteCount
fdWrite fd s =
    BU.unsafeUseAsCStringLen s $ \(buf,len) -> do
        rc <- Posix.fdWriteBuf fd (FFI.castPtr buf) (fromIntegral len)
        return (fromIntegral rc)

----------------------------------------------------------------
----------------------------------------------------------- fin.

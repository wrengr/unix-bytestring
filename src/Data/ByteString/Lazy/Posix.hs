{-# OPTIONS_GHC -Wall -fwarn-tabs #-}
----------------------------------------------------------------
--                                                    2011.02.20
-- |
-- Module      :  Data.ByteString.Lazy.Posix
-- Copyright   :  Copyright (c) 2010--2011 wren ng thornton
-- License     :  BSD
-- Maintainer  :  wren@community.haskell.org
-- Stability   :  provisional
-- Portability :  non-portable (Posix)
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

import qualified Data.ByteString               as BS
import qualified Data.ByteString.Posix         as BSP
import qualified Data.ByteString.Lazy          as BL
import qualified Data.ByteString.Lazy.Internal as BLI
import           System.Posix.Types            (Fd, ByteCount)

----------------------------------------------------------------

-- | Read data from an 'Fd' and convert it to a 'BL.ByteString'.
-- Throws an exception if this is an invalid descriptor, or EOF has
-- been reached.
fdRead
    :: Fd
    -> ByteCount                     -- ^ How many bytes to try to read.
    -> IO (BL.ByteString, ByteCount) -- ^ The bytes read, how many
                                     --   bytes were actually read.
fdRead _  0 = return (BL.empty, 0)
fdRead fd n = do
    (s,n') <- BSP.fdRead fd n
    return (BL.fromChunks [s], n')


-- | Write a 'BL.ByteString' to an 'Fd'.
fdWrite :: Fd -> BL.ByteString -> IO ByteCount
fdWrite fd = go 0
    where
    -- We want to do a left fold in order to avoid stack overflows,
    -- but we need to have an early exit for incomplete writes
    -- (which normally requires a right fold). Hence this recursion.
    go acc BLI.Empty        = return acc
    go acc (BLI.Chunk c cs) = do
        rc <- BSP.fdWrite fd c
        let acc' = acc+rc in acc' `seq` do
        if rc == fromIntegral (BS.length c)
            then go acc' cs
            else return acc'

----------------------------------------------------------------
----------------------------------------------------------- fin.

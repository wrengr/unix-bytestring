{-# OPTIONS_GHC -Wall -fwarn-tabs #-}
{-# LANGUAGE BangPatterns #-}
----------------------------------------------------------------
--                                                    2011.03.04
-- |
-- Module      :  System.Posix.IO.ByteString.Lazy
-- Copyright   :  Copyright (c) 2010--2011 wren ng thornton
-- License     :  BSD
-- Maintainer  :  wren@community.haskell.org
-- Stability   :  provisional
-- Portability :  non-portable (requires POSIX)
--
-- Provides lazy 'ByteString' versions of the "System.Posix.IO"
-- file-descriptor based I\/O API.
----------------------------------------------------------------
module System.Posix.IO.ByteString.Lazy
    (
    -- * I\/O with file descriptors
      fdRead
    , fdWrites
    -- TODO: fdWritev
    ) where

import qualified Data.ByteString               as BS
import qualified Data.ByteString.Unsafe        as BSU
import qualified System.Posix.IO.ByteString    as PosixBS
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
    -> IO (BL.ByteString, ByteCount) -- ^ The bytes read, and how many
                                     --   bytes were actually read.
fdRead _  0 = return (BL.empty, 0)
fdRead fd n = do
    (s,n') <- PosixBS.fdRead fd n
    return (BLI.chunk s BL.empty, n')


----------------------------------------------------------------
-- | Write a 'BL.ByteString' to an 'Fd'. The return value is a pair
-- of the total number of bytes written, and the remaining (unwritten)
-- input. This function makes one @write@ system call per chunk,
-- as per 'PosixBS.fdWrites'.
fdWrites :: Fd -> BL.ByteString -> IO (ByteCount, BL.ByteString)
fdWrites fd = go 0
    where
    -- We want to do a left fold in order to avoid stack overflows,
    -- but we need to have an early exit for incomplete writes
    -- (which normally requires a right fold). Hence this recursion.
    go acc BLI.Empty        = return (acc, BL.empty)
    go acc (BLI.Chunk c cs) = do
        rc <- PosixBS.fdWrite fd c
        let !acc'  = acc+rc
            !rcInt = fromIntegral rc
        if rcInt == BS.length c
            then go acc' cs
            else return (acc', BLI.Chunk (BSU.unsafeDrop rcInt c) cs)
{-
Using 'BSU.unsafeDrop' above is safe, assuming that
'System.Posix.IO.fdWriteBuf' never returns (rc < 0 || rc > BS.length c).
If we are paranoid about that then we should do the following instead:

    go acc ccs =
        case ccs of
        BLI.Empty      -> return (acc, ccs)
        BLI.Chunk c cs -> do
            rc <- PosixBS.fdWrite fd c
            let !acc'  = acc+rc
                !rcInt = fromIntegral rc
            case BS.length c of
                len | rcInt == len -> go acc' cs
                    | rcInt >  len -> error _impossibleByteCount
                    | rcInt <  0   -> error _negtiveByteCount
                    | rcInt == 0   -> return (acc', ccs) -- trivial optimizing
                    | otherwise    -> return (acc', BLI.Chunk (BSU.unsafeDrop rcInt c) cs)

_negtiveByteCount =
    "System.Posix.IO.fdWriteBuf: returned a negative byte count"
_impossibleByteCount =
    "System.Posix.IO.fdWriteBuf: returned a byte count greater than the length it was given"
-}

----------------------------------------------------------------
----------------------------------------------------------- fin.

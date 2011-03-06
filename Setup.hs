#!/usr/bin/env runhaskell
-- Cf. <http://www.mail-archive.com/haskell-cafe@haskell.org/msg59984.html>

{-# OPTIONS_GHC -Wall -fwarn-tabs -fno-warn-missing-signatures #-}
module Main (main) where
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo (withPrograms, buildDir)
import Distribution.Simple.Program        (userSpecifyArgs)
import System.Directory
import System.FilePath
import Control.Monad (forM_)
----------------------------------------------------------------

-- | The list of generated files to remove. /N.B./, watch your file
-- extensions.
--
-- TODO: this should be discovered dynamically.
generatedFiles :: [FilePath]
generatedFiles =
    [ "System/Posix/Types/Iovec.hs"
    ]


-- | Define __HADDOCK__ when building documentation.
main :: IO ()
main = defaultMainWithHooks
    $ simpleUserHooks `modify_haddockHook` \oldHH pkg lbi hooks flags -> do
            
        -- Horrible hack to force re-processing of the @.hsc@ files.
        -- Otherwise the __HADDOCK__ macro doesn't end up being defined
        -- because Cabal will reuse the processed file from before.
        forM_ generatedFiles $ \file ->
            removeFile (buildDir lbi </> file) `catch` \_ -> return ()
        
        -- Call the old haddockHook with a modified LocalBuildInfo
        (\lbi' -> oldHH pkg lbi' hooks flags)
            $ lbi `modify_withPrograms` \oldWP ->
                userSpecifyArgs "hsc2hs" ["-D__HADDOCK__"] oldWP


modify_haddockHook  hooks f = hooks { haddockHook  = f (haddockHook  hooks) }
modify_withPrograms lbi   f = lbi   { withPrograms = f (withPrograms lbi)   }

----------------------------------------------------------------
----------------------------------------------------------- fin.

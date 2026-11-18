-- Prints the unit id that Cabal gives a package's library when stack
-- configures it, so that bin/fast-run.sh can compile the library under the
-- same id. GHC prints this id in call stacks, so it shows up in test messages.
--
-- Usage: cabal-unit-id <name-version> <unit ids of all the package's dependencies, sorted by name>
--
-- This follows computeComponentId in Cabal's Distribution.Backpack.Id (a
-- hidden module), for a package with no flags and no explicit --cid or --ipid.
module Main (main) where

import Distribution.Types.ComponentId (mkComponentId)
import Distribution.Types.Flag (mkFlagAssignment)
import Distribution.Utils.Base62 (hashToBase62)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> error "cabal-unit-id expects a package id and the unit ids of its dependencies"
    (packageId : deps) ->
      putStrLn $ packageId ++ "-" ++ hashToBase62 (packageId ++ show (map mkComponentId deps) ++ show (mkFlagAssignment []))

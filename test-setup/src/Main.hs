{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
module Main (main) where
import Data.List (findIndex, isInfixOf)
import Data.IORef
import System.Environment (getArgs)
import Control.Arrow ((>>>))
import Control.Monad (when)
import System.Directory (copyFile)
import Data.Aeson.Lens (key, _Array)
import Control.Lens ((%~))
import Data.Yaml ( decodeFileThrow, encodeFile )
import qualified Data.Yaml as Yaml

main :: IO ()
main = do
  args <- getArgs
  case args of 
    [] -> error "setup-tests expects one argument - the project directory whose code should be modified"
    xs -> modifyTests (head xs)

hspecFormatterPath :: String
hspecFormatterPath = "pre-compiled/test/HspecFormatter.hs"

modifyTests :: String -> IO ()
modifyTests inputDir = do
  let testFile = inputDir ++ "/test/Tests.hs"
      packageFile = inputDir ++ "/package.yaml"

  testCodeRef <- readFile testFile >>= newIORef . lines

  readIORef testCodeRef >>=
    (updateHspecRunnerImport >>> updateMainFunc >>> writeIORef testCodeRef)

  -- Update the test/Tests.hs file with the new contents
  -- We use `when (length newTestFileData > 0)` as a trick to strictly evaluate the
  -- file data before trying to write to the file otherwise we get a "resouce busy (file is locked)" error
  newTestFileData <- readIORef testCodeRef
  {-# HLINT ignore "Use null" #-}
  when (length newTestFileData > 0) $ writeFile testFile (unlines newTestFileData)

  -- Add aeson, aeson-pretty, bytestring, hspec-core, stm and text packages to `tests` section of package.yaml.
  let missingDependencies = ["aeson", "aeson-pretty", "bytestring", "hspec-core", "stm", "text"]
      addDepependencies = key "tests" . key "test" . key "dependencies" . _Array %~ (<> missingDependencies)
  originalYaml <- decodeFileThrow packageFile :: IO Yaml.Value
  encodeFile packageFile (addDepependencies originalYaml)

  -- Copy our custom hspec formatter into the input code directory so it can be used
  copyFile hspecFormatterPath (inputDir ++ "/test/HspecFormatter.hs")

  where
    -- Update Test.Hspec.Runner import to add the `configFormat` import that we need
    -- and also add the import HspecFormatter line
    updateHspecRunnerImport = 
      updateLineOfCode 
        isHspecRunnerImport 
        "import Test.Hspec.Runner (configFailFast, defaultConfig, hspecWith, configFormat)\nimport HspecFormatter" 

    -- Update the main function to add the configFormat option to hspec to use our custom
    -- formatter that outputs results.json in the necessary format.
    -- It also removes the configFailFast option so that we run ALL tests rather than stopping
    -- at the first failing.
    updateMainFunc =
      updateLineOfCode 
        isMainFunc 
        "main = hspecWith defaultConfig {configFormat = Just formatter} specs"

updateLineOfCode :: (String -> Bool) -> String -> [String] -> [String]
updateLineOfCode isLineToUpdate newLine fileContents =
  case findIndex isLineToUpdate fileContents of
    Just idx -> replaceNth idx newLine fileContents
    Nothing -> fileContents

isHspecRunnerImport :: String -> Bool
isHspecRunnerImport = isInfixOf "import Test.Hspec.Runner"

isMainFunc :: String -> Bool
isMainFunc = isInfixOf "main = hspecWith"

replaceNth :: Int -> a -> [a] -> [a]
replaceNth idx newVal list = 
  let (first, second) = splitAt idx list
  in first <> (newVal : tail second)

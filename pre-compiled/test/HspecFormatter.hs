-- NOTE: This file is used by the setup-tests executable (built from the test-setup/ directory).
-- It's copied into the target project and is configured to be the hspec formatter using code injection.
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}

module HspecFormatter (formatter) where

import Data.Aeson (ToJSON, toJSON, object, encode, (.=))
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BS
import Control.Concurrent.STM
import Test.Hspec.Core.Formatters.V2
import Test.Hspec.Core.Format (Format, FormatConfig, Path, Event(ItemDone, Done))
import GHC.Generics (Generic)
import qualified Manifest
import qualified System.IO as IO

data TestResultStatus = Pass | Fail | Err deriving (Eq, Show)

instance ToJSON TestResultStatus where
  toJSON Pass = "pass"
  toJSON Fail = "fail"
  toJSON Err = "error"

data TestResult = TestResult {
  name :: String,
  status :: TestResultStatus,
  message :: Maybe String,
  taskId :: Maybe Int
} deriving (Generic, Show)

instance ToJSON TestResult where
  toJSON t = object [
      "name" .= t.name
      , "status" .= t.status
      , "message" .= t.message
      , "task_id" .= t.taskId
    ]

data TestResults = TestResults {
  resultsStatus :: TestResultStatus,
  tests :: [TestResult],
  resultsMessage :: Maybe String,
  version :: Int
} deriving (Generic, Show)

instance ToJSON TestResults where
  toJSON t = object [
      "version" .= t.version
      , "status" .= t.resultsStatus
      , "message" .= t.resultsMessage
      , "tests" .= t.tests
    ]

format :: TVar TestResults -> (String -> Maybe Int) -> Format
format results getTaskId event = case event of
  ItemDone path item -> handleItemDone path item
  Done _ -> handleDone
  _ -> return ()
  where
    handleItemDone :: Path -> Item -> IO ()
    handleItemDone (_, requirement) item = do
        let taskId = getTaskId requirement
        case itemResult item of
          Success -> addTestResult TestResult { name = requirement, status = Pass, message = Nothing, taskId }
          -- NOTE: We don't expect pending tests in Exercism exercises
          Pending _ _ -> return ()
          Failure _ failureReason ->
            let baseResult = TestResult { name = requirement, status = Fail, message = Just "", taskId }
                result = case failureReason of
                  NoReason -> baseResult { message = Just "No reason" }
                  Reason reason -> baseResult { message = Just reason }
                  ExpectedButGot _ expected got -> 
                    baseResult { 
                      message = Just $ "Expected '" ++ expected ++ "' but got '" ++ got ++ "'"
                    }
                  Error _ exception -> baseResult { message = Just $ show exception }
            in addTestResult result
      where
        addTestResult tr = atomically $ modifyTVar' results (\r -> r { tests = r.tests <> [tr] })

    handleDone :: IO ()
    handleDone = do
      resultsVal <- readTVarIO results
      let finalResults = if all (\t -> t.status == Pass) resultsVal.tests then resultsVal { resultsStatus = Pass } else resultsVal
      BS.writeFile "results.json" (encodePretty finalResults)
      return ()

formatter :: FormatConfig -> IO Format
formatter _config = do
  getTaskId <- Manifest.getManifest >>= \case
    Left e -> do
      IO.hPutStrLn IO.stderr $ "Could not decode manifest"
      IO.hPutStrLn IO.stderr e
      pure $ \_ -> Nothing
    Right m -> pure $ Manifest.getTaskId m
  format <$> newTVarIO (TestResults Fail [] Nothing 2) <*> pure getTaskId

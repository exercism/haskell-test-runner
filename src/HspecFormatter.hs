{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module HspecFormatter (formatter) where

import Data.Aeson (ToJSON, toJSON, object, encode, (.=))
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy as BS
import Data.Maybe (fromMaybe)
import Control.Monad (forM_)
import Control.Concurrent.STM
import GHC.IO.Unsafe (unsafePerformIO)
import Test.Hspec
import Test.Hspec.Core.Formatters.V2
import Test.Hspec.Core.Format (Format, FormatConfig, Path, Event(ItemDone, Done), FailureReason(..))
import GHC.Generics (Generic)

data TestResultStatus = Pass | Fail | Err deriving (Show)

instance ToJSON TestResultStatus where
  toJSON Pass = "pass"
  toJSON Fail = "fail"
  toJSON Err = "error"

data TestResult = TestResult {
  name :: String,
  status :: TestResultStatus,
  message :: String
} deriving (Generic, Show)

instance ToJSON TestResult where

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

results :: TVar TestResults
{-# NOINLINE results #-}
results = unsafePerformIO $ newTVarIO (TestResults Fail [] Nothing 2)

format :: Format
format event = case event of
  ItemDone path item -> handleItemDone path item
  Done _ -> handleDone
  _ -> return ()
  where
    handleItemDone :: Path -> Item -> IO ()
    handleItemDone (_, requirement) item =
        case itemResult item of
          Success ->
            addTestResult TestResult { name = requirement, status = Pass, message = "" }
          -- NOTE: We don't expect pending tests in Exercism exercises
          Pending _ _ -> return ()
          Failure _ failureReason ->
            let baseResult = TestResult { name = requirement, status = Fail, message = "" }
                result = case failureReason of
                  NoReason -> baseResult
                  Reason reason -> baseResult { message = reason }
                  ExpectedButGot _ expected got -> 
                    baseResult { 
                      message = "Expected '" ++ expected ++ "' but got '" ++ got ++ "'"
                    }
                  Error _ exception -> baseResult { message = show exception }
            in addTestResult result
      where
        addTestResult tr = atomically $ modifyTVar' results (\r -> r { tests = r.tests <> [tr] })

    handleDone :: IO ()
    handleDone = do
      resultsVal <- readTVarIO results
      BS.writeFile "results.json" (encodePretty resultsVal)
      return ()


formatter :: FormatConfig -> IO Format
formatter _config = return format

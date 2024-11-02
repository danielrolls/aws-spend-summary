{-|
Description : For printing AWS account costs to the terminal
Copyright   : (c) Daniel Rolls, 2024
License     : GPL-2 only

This module is for printing AWS account costs to the terminal.
Costs are all unblended and shown per day.
-}
module AwsSpendSummary (Options(Options), numberOfDays, printCosts) where

import Prelude hiding (concat)
import Amazonka.Data.Body (_ResponseBody)
import Amazonka.S3.GetObject (newGetObject, getObjectResponse_body)
import Amazonka.S3.Internal (BucketName(..), ObjectKey(..))
import Amazonka (_ServiceError, discover, newEnv, serviceError_status, sendEither)
import Codec.Compression.GZip (decompress)
import Conduit ((.|), foldC, liftIO, runConduit, runResourceT)
import Control.Lens ((^.), (^?), over, set)
import qualified Data.ByteString.Lazy as LBS (ByteString, fromStrict, writeFile)
import Data.Csv (FromField, FromNamedRecord, (.:), decodeByName, parseField, parseNamedRecord)
import Data.Default (Default, def)
import Data.Map as Map (Map(), insertWith, filterWithKey, toAscList)
import qualified Data.Map as Map (empty)
import qualified Data.Set as Set (fromList, toList)
import Data.Text.Encoding (decodeUtf8)
import Data.Text (Text, pack, unpack)
import Data.Time.Calendar (fromGregorian, toGregorian)
import Data.Time.Clock (UTCTime, NominalDiffTime, addUTCTime, getCurrentTime, nominalDay, secondsToDiffTime, utctDay)
import Data.Time.Format (defaultTimeLocale, formatTime, parseTimeOrError)
import qualified Data.Time.Timelens as TL (utctDay, utctDayTime)
import Data.Vector (Vector, concat, empty)
import Network.HTTP.Types.Status (statusCode)
import Numeric (showFFloat)
import System.Console.ANSI (Color(Green, Red), ColorIntensity(Dull, Vivid), ConsoleLayer(Foreground), SGR(Reset, SetColor), setSGR)
import System.IO (hPutStrLn, stderr)


-- | Optional arguments to pass to @printCosts@
data Options = Options {
  numberOfDays :: Integer -- ^ Number of days to show results for
}

instance Default Options where
    def = Options 15

instance FromField UTCTime where
    parseField = return
               . parseTimeOrError False defaultTimeLocale "%Y-%m-%dT%H:%M:%S.000Z"
               . unpack
               . decodeUtf8

dropTime :: UTCTime -> UTCTime
dropTime = set TL.utctDayTime $ secondsToDiffTime 0

updateDailyCost :: Map UTCTime Double -> Cost -> Map UTCTime Double
updateDailyCost db cost = insertWith (+)
                                     (dropTime $ usageStartDate cost)
                                     (unblendedCost cost)
                                     db

data Cost = Cost {
  usageStartDate  :: UTCTime
, usageEndDate  :: UTCTime
, unblendedCost :: Double
} deriving (Show, Eq)


instance FromNamedRecord Cost where
    parseNamedRecord m = Cost <$> m .: "line_item_usage_start_date"
                              <*> m .: "line_item_usage_end_date"
                              <*> m .: "line_item_unblended_cost"

extractCostData :: LBS.ByteString -> Vector Cost
extractCostData = either (error . ("could not parse csv: " <>))
                         snd
                  . decodeByName

startOfMonth :: UTCTime -> UTCTime
startOfMonth = dropTime
               . over TL.utctDay
                      ((\(y,m,_) -> fromGregorian y m 1) . toGregorian)

-- | Print the costs to the terminal
printCosts :: Options -> Text -> Text -> Text -> IO ()
printCosts options bucketName pathPrefix costReportName =
    do startDate <- xDaysAgo (fromInteger (numberOfDays options))
       today <- getCurrentTime
       awsResults <- mapM (getCostsFromAWS bucketName
                                           pathPrefix
                                           costReportName)
                          $ dropDuplicates $ startOfMonth <$> [startDate, today]
       layoutTable $ filterWithKey (\k _ -> k > startDate)
                                   $ foldl updateDailyCost Map.empty $ concat awsResults
    where dropDuplicates = Set.toList . Set.fromList

xDaysAgo :: NominalDiffTime -> IO UTCTime
xDaysAgo days = addUTCTime (-days * nominalDay) <$> getCurrentTime

layoutRow :: (UTCTime, Double) -> IO ()
layoutRow (a,b) = do putStr $ layoutDate a <> ": "
                     layoutCost b
                     putStrLn ""
                  where layoutDate = formatTime defaultTimeLocale "%d %B"

layoutCost :: Double -> IO ()
layoutCost cost =
  do setSGR $ pure $ if cost > 0.02 then
                       SetColor Foreground Vivid Red
                     else
                       SetColor Foreground Dull Green
     putStr $ showFFloat (Just 4) cost ""
     setSGR [Reset]

layoutTable :: Map.Map UTCTime Double -> IO ()
layoutTable = foldMap layoutRow . toAscList

debugOutput :: FilePath
debugOutput = "processed-csv-debug.csv"

getCostsFromAWS :: Text -> Text -> Text -> UTCTime -> IO (Vector Cost)
getCostsFromAWS bucketName pathPrefix costReportName startTime =
    do runResourceT (
        do res' <- newEnv discover
                   >>= (`sendEither` newGetObject (BucketName bucketName)
                                                  (ObjectKey fullPath))
           case res' of
             Left e -> do if (statusCode <$> (e ^? _ServiceError . serviceError_status)) == Just 404 then
                            liftIO $ hPutStrLn stderr $ "Got 404 when trying to retrieve results for month number " <> unpack mnthStr <> " which might be because no report exists for this month yet."
                          else
                            liftIO $ hPutStrLn stderr $
                              "Could not retrieve: " <> show fullPath <> "\n" <> show e
                          return Data.Vector.empty
             Right res -> do r <- runConduit $
                               res ^. getObjectResponse_body . _ResponseBody .| foldC
                             let rawCsv = decompress . LBS.fromStrict $ r
                             liftIO $ LBS.writeFile debugOutput rawCsv
                             return . extractCostData $ rawCsv)
    where (yr, mnth, _) = toGregorian $ utctDay startTime
          mnthStr = pack $ twoDigitPad $ show mnth
          fullPath = pathPrefix
                   <> "/" <> costReportName
                   <> "/data"
                   <> "/BILLING_PERIOD=" <> pack (show yr) <> "-" <> mnthStr
                   <> "/" <> costReportName <> "-00001.csv.gz"
          twoDigitPad s = replicate (2 - length s) '0' ++ s


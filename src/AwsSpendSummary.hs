{-|
Description : For printing AWS account costs to the terminal
Copyright   : (c) Daniel Rolls, 2024
License     : GPL-2 only

This module is for printing AWS account costs to the terminal.
Costs are all unblended and shown per day.
-}
module AwsSpendSummary (Options(Options), numberOfDays, printCosts, threshold) where

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
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (decodeUtf8)
import Data.Time.Calendar (addGregorianMonthsClip, fromGregorian, toGregorian)
import Data.Time.Clock (UTCTime(UTCTime), addUTCTime, getCurrentTime, nominalDay, secondsToDiffTime, utctDay)
import Data.Time.Format (defaultTimeLocale, formatTime, parseTimeOrError)
import qualified Data.Time.Timelens as TL (utctDay, utctDayTime)
import Data.Vector (Vector, concat, empty)
import Network.HTTP.Types.Status (status404)
import Numeric (showFFloat)
import System.Console.ANSI (Color(Green, Red), ColorIntensity(Dull, Vivid), ConsoleLayer(Foreground), SGR(Reset, SetColor), setSGR)
import System.IO (hPutStrLn, stderr)


-- | Optional arguments to pass to @printCosts@
data Options = Options {
  numberOfDays :: Integer -- ^ Number of days to show results for
, threshold :: Double -- ^ Threshold to determine red/green colouring on console
, csvOutputFile :: Maybe Text -- ^ Optional file to dump the csv response into
}

instance Default Options where
    def = Options 15 1.0 Nothing

data Cost = Cost {
  usageStartDate  :: UTCTime
, usageEndDate  :: UTCTime
, unblendedCost :: Double
} deriving (Show, Eq)


instance FromNamedRecord Cost where
    parseNamedRecord m = Cost <$> m .: "line_item_usage_start_date"
                              <*> m .: "line_item_usage_end_date"
                              <*> m .: "line_item_unblended_cost"

instance FromField UTCTime where
    parseField = return
               . parseTimeOrError False defaultTimeLocale "%Y-%m-%dT%H:%M:%S.000Z"
               . unpack
               . decodeUtf8

-- | Print the costs to the terminal
printCosts :: Options -> Text -> Text -> Text -> IO ()
printCosts options bucketName pathPrefix costReportName =
    do dateToday <- getCurrentTime
       let startTime = xDaysAgo (fromInteger (numberOfDays options)) dateToday
       mapM (getCostsFromAWS (csvOutputFile options)
                              bucketName
                              pathPrefix
                              costReportName)
            (firstOfMonthBetween startTime dateToday)
         >>= layoutTable (threshold options) . filterWithKey (\k _ -> k > startTime)
                                             . foldl updateDailyCost Map.empty
                                             . concat
    where xDaysAgo = addUTCTime . (* (-nominalDay))

layoutTable :: Double -> Map.Map UTCTime Double -> IO ()
layoutTable threshold = foldMap (layoutRow threshold) . toAscList

layoutRow :: Double -> (UTCTime, Double) -> IO ()
layoutRow threshold (a,b) =
    do putStr $ layoutDate a <> ": "
       layoutCost threshold b
       putStrLn ""
    where layoutDate = formatTime defaultTimeLocale "%d %B"

layoutCost :: Double -> Double -> IO ()
layoutCost threshold cost =
  do setSGR $ pure $ if cost > threshold then
                       SetColor Foreground Vivid Red
                     else
                       SetColor Foreground Dull Green
     putStr $ showFFloat (Just 4) cost ""
     setSGR [Reset]

getCostsFromAWS :: Maybe Text -> Text -> Text -> Text -> UTCTime -> IO (Vector Cost)
getCostsFromAWS debugFile bucketName pathPrefix costReportName startTime = runResourceT (
     newEnv discover
     >>= (`sendEither` newGetObject (BucketName bucketName)
                                    (ObjectKey fullPath))
     >>= \case
         Left e -> do
             liftIO $ case e ^? _ServiceError . serviceError_status of
                 Just status | status == status404 ->
                     hPutStrLn stderr $ "Got 404 when trying to retrieve "
                                     <> "results for " <> mnthStr
                                     <> " which might be because "
                                     <> "no report exists for this month yet."
                 _ -> error $
                         "Could not retrieve results from: " <> show fullPath
                      <> "\n" <> show e
             return Data.Vector.empty
         Right res -> do
             rawCsv <- decompress . LBS.fromStrict <$> runConduit (
                 res ^. getObjectResponse_body . _ResponseBody .| foldC)
             handleJust
                 (liftIO . (`LBS.writeFile` rawCsv) . unpack)
                 debugFile
             return . extractCostData $ rawCsv
    )
    where mnthStr = formatTime defaultTimeLocale "%B" startTime
          billingPeriod = formatTime defaultTimeLocale "%_Y-%m" startTime
          fullPath = pathPrefix
                  <> "/" <> costReportName
                  <> "/data"
                  <> "/BILLING_PERIOD=" <> pack billingPeriod
                  <> "/" <> costReportName <> "-00001.csv.gz"
          handleJust = maybe (return ())

extractCostData :: LBS.ByteString -> Vector Cost
extractCostData = either (error . ("could not parse csv: " <>))
                         snd
                  . decodeByName

-- | Given a start and end date, returns the first date of each month
-- spanned by the two dates.
firstOfMonthBetween :: UTCTime -> UTCTime -> [UTCTime]
firstOfMonthBetween start end = takeWhile (<= end) $ iterate nextMonth firstOfMonth
  where
    (y, m, _) = toGregorian (utctDay start)
    firstOfMonth = UTCTime (fromGregorian y m 1) (secondsToDiffTime 0)
    nextMonth = over TL.utctDay (addGregorianMonthsClip 1)

updateDailyCost :: Map UTCTime Double -> Cost -> Map UTCTime Double
updateDailyCost db cost = insertWith (+)
                                     (dropTime $ usageStartDate cost)
                                     (unblendedCost cost)
                                     db

dropTime :: UTCTime -> UTCTime
dropTime = set TL.utctDayTime $ secondsToDiffTime 0

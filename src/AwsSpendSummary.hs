module AwsSpendSummary (printCosts) where

import Amazonka.Data.Body (_ResponseBody)
import Amazonka.S3.GetObject (newGetObject, getObjectResponse_body, getObjectResponse_httpStatus)
import Amazonka.S3.Internal (BucketName(..), ObjectKey(..))
import Amazonka (send, newEnv, discover)
import Codec.Compression.GZip (decompress)
import Conduit (foldlC, runConduit, runResourceT, (.|), liftIO)
import Control.Lens ((^.))
import qualified Data.ByteString.Lazy as LBS (ByteString, fromStrict, writeFile)
import Data.Csv (decodeByName, FromNamedRecord, parseNamedRecord, (.:), FromField, parseField)
import Data.Map (Map(), insertWith, empty, filterWithKey, toAscList)
import qualified Data.Set as Set (fromList, toList)
import Data.Text.Encoding (decodeUtf8)
import Data.Text (Text, pack, unpack)
import Data.Time.Calendar (fromGregorian, toGregorian)
import Data.Time.Clock (UTCTime, NominalDiffTime, addUTCTime, getCurrentTime, nominalDay, secondsToDiffTime, utctDay, utctDayTime)
import Data.Time.Format (defaultTimeLocale, formatTime, parseTimeOrError)
import qualified Data.Time.Timelens as TL (utctDay)
import Data.Vector (Vector)
import Numeric (showFFloat)
import System.Console.ANSI (Color(Green, Red), ColorIntensity(Dull, Vivid), ConsoleLayer(Foreground), SGR(Reset, SetColor), setSGR)


instance FromField UTCTime where
    parseField = return
               . parseTimeOrError False defaultTimeLocale  "%Y-%m-%dT%H:%M:%S.000Z"
               . unpack
               . decodeUtf8

dropTime :: UTCTime -> UTCTime
dropTime dt = dt{utctDayTime = secondsToDiffTime 0}

updateDailyCost :: Map UTCTime Double -> Cost -> Map UTCTime Double
updateDailyCost db cost = insertWith (+)
                                     (dropTime $ usageStartDate cost)
                                     (unblendedCost cost)
                                     db

costsPerDay :: Foldable a => Map UTCTime Double -> a Cost -> Map UTCTime Double
costsPerDay = foldl updateDailyCost

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
startOfMonth startDate = dropTime $ startDate{
   utctDay = (\(y,m,_) -> fromGregorian y m 1) . toGregorian . utctDay $ startDate
}

printCosts :: Integer -> Text -> Text -> Text -> IO ()
printCosts offset bucketName pathPrefix costReportName =
       do startDate <- xDaysAgo (fromInteger offset)
          today <- getCurrentTime
          awsResults <- mapM (getCostsFromAWS bucketName
                                              pathPrefix
                                              costReportName)
                             $ mkUniq $ startOfMonth <$> [startDate, today]

          layoutTable $ filterWithKey (\k _ -> k > startDate)
                                      $ foldl costsPerDay Data.Map.empty
                                      $ extractCostData
                                     <$> awsResults

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

layoutTable :: Map UTCTime Double -> IO ()
layoutTable = layoutTableWorker . toAscList
  where layoutTableWorker = foldl (\b a -> b >> layoutRow a)
                                  (putStr "")

debugOutput :: FilePath
debugOutput = "processed-csv-debug.csv"

getCostsFromAWS :: Text -> Text -> Text -> UTCTime -> IO LBS.ByteString
getCostsFromAWS bucketName pathPrefix costReportName startTime =
    do rawCsv <- decompress . LBS.fromStrict <$> runResourceT (
           do res <- newEnv discover
                     >>= (`send` newGetObject (BucketName bucketName)
                                              (ObjectKey fullPath))
              liftIO $ print $ res ^. getObjectResponse_httpStatus
              let resBody  = res ^. getObjectResponse_body . _ResponseBody
              runConduit $ resBody .| foldlC (<>) "")
       LBS.writeFile debugOutput rawCsv
       return rawCsv
    where (yr, mnth, _) = toGregorian $ utctDay startTime
          mnthStr = pack $ twoDigitPad $ show mnth
          fullPath = pathPrefix
                   <> "/" <> costReportName
                   <> "/data"
                   <> "/BILLING_PERIOD=" <> pack (show yr) <> "-" <> mnthStr
                   <> "/" <> costReportName <> "-00001.csv.gz"
          twoDigitPad s = replicate (2 - length s) '0' ++ s

mkUniq :: Ord a => [a] -> [a]
mkUniq = Set.toList . Set.fromList


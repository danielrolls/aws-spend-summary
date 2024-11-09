module Main (main) where

import AwsSpendSummary (Options(Options), threshold, numberOfDays, printCosts)
import Data.Default (def)
import Data.Text (Text)
import Options.Applicative ((<**>), Parser, ParserInfo, argument, auto, execParser, fullDesc, header, help, helper, info, long, metavar, option, optional, progDesc, short, showDefault, str, strOption, value)


data CostQuery = CostQuery {
  bucketName :: Text
, bucketPath :: Text
, costReportName :: Text
, daysToQuery :: Integer
, requestThreshold :: Double
, requestCsvOutputFile :: Maybe Text
}

costArgParser :: Parser CostQuery
costArgParser = CostQuery
    <$> argument str (
            metavar "BUCKET_NAME"
         <> help "The name of the S3 bucket that the cost report was exported to")
    <*> argument str (
            metavar "BUCKET_PATH"
         <> help ( "This is the prefix to the bucket key AWS asks "
                <> "you specify when setting up a cost report")
        )
    <*> argument str (
            metavar "COST_REPORT_NAME"
         <> help "The name given when creating the cost report")
    <*> option auto (
           long "days"
        <> short 'd'
        <> metavar "DAYS"
        <> value (numberOfDays def)
        <> showDefault
        <> help "Number of days to print"
        )
    <*> option auto (
           long "threshold"
        <> short 't'
        <> metavar "DOLLARS"
        <> value (threshold def)
        <> showDefault
        <> help "Threshold to detrmine red/green colouring on thhe console"
        )
    <*> optional ( strOption $
           long "csv-output"
        <> help "Specify a file to dump the CSV output into. Useful for testing debugging or to go and explore further!"
        <> metavar "FILENAME"
        )

opts :: ParserInfo CostQuery
opts = info
  (costArgParser <**> helper)
  (    fullDesc
    <> progDesc "Print out the latest daily unblended costs for an AWS account"
    <> header "AWS Cost Summary"
  )

main :: IO ()
main = do costQuery <- execParser opts
          printCosts (Options (daysToQuery costQuery)
                              (requestThreshold costQuery)
                              (requestCsvOutputFile costQuery))
                     (bucketName costQuery)
                     (bucketPath costQuery)
                     (costReportName  costQuery)

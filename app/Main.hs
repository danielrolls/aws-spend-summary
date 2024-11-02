module Main (main) where

import AwsSpendSummary (Options(Options), numberOfDays, printCosts)
import Data.Default (def)
import Data.Text (Text)
import Options.Applicative ((<**>), Parser, ParserInfo, argument, auto, execParser, fullDesc, header, help, helper, hidden, info, long, metavar, option, progDesc, short, showDefault, str, value)


data CostQuery = CostQuery {
  bucketName :: Text
, bucketPath :: Text
, costReportName :: Text
, daysToQuery :: Integer
}

costArgParser :: Parser CostQuery
costArgParser = CostQuery
    <$> argument str (metavar "BUCKET_NAME")
    <*> argument str (
            metavar "BUCKET_PATH"
         <> help ( "This is the prefix to the bucket key AWS asks "
                <> "you specify when setting up a cost report")
        )
    <*> argument str (metavar "COST_REPORT_NAME")
    <*> option auto (
           long "days"
        <> short 'd'
        <> hidden
        <> metavar "DAYS"
        <> value (numberOfDays def)
        <> showDefault
        <> help "Number of days to print"
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
          printCosts (Options (daysToQuery costQuery))
                     (bucketName costQuery)
                     (bucketPath costQuery)
                     (costReportName  costQuery)

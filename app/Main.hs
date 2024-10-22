module Main (main) where

import AwsSpendSummary (printCosts)
import Data.Text(Text)
import Options.Applicative ((<**>), Parser, ParserInfo, argument, execParser, fullDesc, header, helper, info, metavar, progDesc, str)


data CostQuery = CostQuery {
  bucketName :: Text
, bucketPath :: Text
, costReportName :: Text
}

costArgParser :: Parser CostQuery
costArgParser = CostQuery <$> argument str (metavar "BUCKET_NAME")
                          <*> argument str (metavar "BUCKET_PATH")
                          <*> argument str (metavar "COST_REPORT_NAME")

opts :: ParserInfo CostQuery
opts = info (costArgParser <**> helper)
            (  fullDesc
            <> progDesc "Print out the latest daily unblended costs for an AWS account"
            <> header "AWS Cost Summary"
            )

main :: IO ()
main = do costQuery <- execParser opts
          printCosts 15
                     (bucketName costQuery)
                     (bucketPath costQuery)
                     (costReportName  costQuery)

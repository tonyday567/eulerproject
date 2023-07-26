{-# LANGUAGE OverloadedStrings #-}

-- | basic measurement and callibration
module Main where

import Control.Monad
import Data.List (intercalate)
import Euler
import Options.Applicative
import Perf
import Prelude

data RunType = RunDefault deriving (Eq, Show)

data Options = Options
  { optionN :: Int,
    optionValue :: Int,
    optionStatDType :: StatDType,
    optionRunType :: RunType,
    optionMeasureType :: MeasureType,
    optionGolden :: Golden,
    optionReportConfig :: ReportConfig,
    optionRawStats :: Bool
  }
  deriving (Eq, Show)

parseRun :: Parser RunType
parseRun =
  flag' RunDefault (long "default" <> help "default test")
    <|> pure RunDefault

options :: Parser Options
options =
  Options
    <$> option auto (value 1 <> long "runs" <> short 'n' <> help "number of runs to perform")
    <*> option auto (value 1000 <> long "value" <> short 'v' <> help "value")
    <*> parseStatD
    <*> parseRun
    <*> parseMeasure
    <*> parseGolden "golden"
    <*> parseReportConfig defaultReportConfig
    <*> switch (long "raw" <> short 'w' <> help "write raw statistics to file")

opts :: ParserInfo Options
opts =
  info
    (options <**> helper)
    (fullDesc <> progDesc "perf benchmarking" <> header "basic perf callibration")

main :: IO ()
main = do
  o <- execParser opts
  let !n = optionN o
  let !v = optionValue o
  let s = optionStatDType o
  let r = optionRunType o
  let mt = optionMeasureType o
  let gold = goldenFromOptions [show r, show n, show v, show mt] (optionGolden o)
  let w = optionRawStats o
  let raw =
        "other/"
          <> intercalate "-" [show r, show n, show v, show mt]
          <> ".map"
  let cfg = optionReportConfig o

  case r of
    RunDefault -> do
      m <- execPerfT (measureDs mt n) $ do
        _ <- ffap "euler14" euler14 v
        _ <- ffap "euler14Array" euler14Array v
        pure ()
      when w (writeFile raw (show m))
      report cfg gold (measureLabels mt) (statify s m)

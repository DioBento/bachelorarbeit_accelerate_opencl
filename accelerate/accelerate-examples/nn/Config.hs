{-# LANGUAGE TemplateHaskell #-}

module Config where

import Data.Label
import System.Console.GetOpt


data Config = Config
  { _configFilePath :: Maybe FilePath
  , _configLat      :: Float
  , _configLong     :: Float
  , _configQuiet    :: Bool
  } deriving Show

$(mkLabels [''Config])


defaults :: Config
defaults = Config
  { _configFilePath = Nothing
  , _configLat      = 0
  , _configLong     = 0
  , _configQuiet    = False
  }


options :: [OptDescr (Config -> Config)]
options = [ Option ['f']["file"] (ReqArg (set configFilePath . Just . read) "FILE") "read input from file"
          , Option []["lat"] (ReqArg (set configLat . read) "FLOAT") "Sets Latitude"
          , Option []["lon"] (ReqArg (set configLong . read) "FLOAT") "Sets Longitude"
          , Option ['q'][] (NoArg (set configQuiet True)) "Set"
          ]


header :: [String]
header =
  [ "accelerate-nn -f <file> [OPTIONS]"
  , ""
  , "set flag --benchmark to measure performance"
  , ""
  ]

footer :: [String]
footer = [ "" ]

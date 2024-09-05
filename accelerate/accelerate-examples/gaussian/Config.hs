{-# LANGUAGE TemplateHaskell #-}

module Config where

import Data.Label
import System.Console.GetOpt


data Config = Config
  { _configSize     :: Int
  , _configVerbose  :: Bool
  , _configFilePath :: Maybe FilePath
  }

$(mkLabels [''Config])


defaults :: Config
defaults = Config
  { _configSize     = 0
  , _configVerbose  = False
  , _configFilePath = Nothing
  }


options :: [OptDescr (Config -> Config)]
options = [ Option ['s'] ["size"] (ReqArg (set configSize . read) "INT") "size of matrix"
          , Option ['v'] ["verbose"] (NoArg (set configVerbose True)) "print result"
          , Option ['f'] ["file"] (ReqArg (set configFilePath . Just . read) "FILE") "read from file"
          ]


header :: [String]
header =
  [ "accelerate-gaussian (c) [2024] Benedikt Drmic"
  , ""
  , "usage: accelerate-gaussian -v -s n"
  , " where n is a number, denoting the size of the matrix"
  , "Use either '--size n' or '--file <file_path>'"
  , "Optional flag --benchmark"
  , ""
  ]

footer :: [String]
footer = [ "" ]

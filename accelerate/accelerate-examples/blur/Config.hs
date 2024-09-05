{-# LANGUAGE TemplateHaskell #-}

module Config where

import Data.Label
import System.Console.GetOpt


data Config = Config
  deriving Show

$(mkLabels [''Config])


defaults :: Config
defaults = Config


options :: [OptDescr (Config -> Config)]
options = []


header :: [String]
header =
  [ "accelerate-blur <file.bmp>"
  , ""
  , "usage: accelerate-blur <file.bmp> [OPTIONS]"
  , ""
  ]

footer :: [String]
footer = [ "" ]

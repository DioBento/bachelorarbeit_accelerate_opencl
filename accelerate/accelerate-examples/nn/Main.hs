module Main
  ( main
  ) where

import Prelude as P
import Data.Char (isSpace)
import Control.Monad (unless)
import Data.Label (get)

import Config
import NearestNeighbour

import qualified Data.Array.Accelerate             as A
import Data.Array.Accelerate.Examples.Internal     as A


main :: IO ()
main = do
  beginMonitoring
  (conf, opts, rest) <- parseArgs options defaults header footer

  let neighbor = nearestNeighbor (get optBackend opts)
      filepath = _configFilePath conf :: Maybe FilePath
      lat      = get configLat conf
      lon      = get configLong conf
      quiet    = get configQuiet conf

  -- (names, latLngs, size) <- case filepath of
  --                        Just fpath -> readLocations fpath
  --                        Nothing    -> error "usage: accelerate-nn -f <FILE> [OPTIONS]"
  (names, latLngs, size) <- readLocations "./database.txt"
  let latLongs = A.fromList (A.Z A.:. size) latLngs :: A.Vector LatLong

  runBenchmarks opts rest
    [ bench "nearest-neighbour" $ nf (neighbor latLongs lat) lon ]

  unless quiet
   (print $ (\(idx, dist) -> (show $ names !! idx) ++ " " ++ (show $ latLngs !! idx) ++ " -> " ++ (show dist))
          $ (\idxDist -> (fst $ head $ A.toList idxDist, snd $ head $ A.toList idxDist))
          $ neighbor latLongs lat lon
   )


readLocations :: FilePath -> IO ([String], [LatLong], Int)
readLocations filename = do
  contents <- readFile filename
  let fileLines = lines contents
      locations = map parseLine fileLines
      names = map fst locations
      locs  = map snd locations
  return (names, locs, length fileLines)


parseLine :: String -> (String, LatLong)
parseLine line =
  let name    = trim $ take 25 line
      latLong = ( read $ take 5 $ drop 25 line
                , read $ take 5 $ drop 30 line
                ) :: LatLong
  in
    (name, latLong)


trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace

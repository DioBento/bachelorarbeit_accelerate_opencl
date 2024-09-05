module Main
  ( main
  ) where

import Prelude as P
import Data.Char (isSpace)
import Config
import Control.Monad (unless)
import NearestNeighbour

import qualified Data.Array.Accelerate             as A
import qualified Data.Array.Accelerate.LLVM.PTX    as PTX
import Data.Array.Accelerate.Examples.Internal     as A


type Location = (String, LatLong)


main :: IO ()
main = do
  beginMonitoring
  (conf, opts, rest) <- parseArgs options defaults header footer

  let neighbor = nearestNeighbor (get optBackend opts)
      filepath = get configFilePath conf
      lat      = get configLat conf
      lon      = get configLong conf
      quiet    = get configQuiet conf

  (names, latLngs, size) <- case filepath of
                         Just fpath -> readLocations fpath
                         Nothing    -> error "usage: opencl -f <FILE> [OPTIONS]"

  let latLongs = A.fromList (A.Z A.:. size) latLngs :: A.Vector LatLong

  runBenchmarks opts rest
    [ bench "nearest-neighbour" $ whnf (neighbor latLongs lat) lon ]

  unless quiet
    (print $ \(idx, dist) -> (show $ names !! id) ++ " " ++ (show $ latLngs !! id) ++ " -> " ++ dist
           $ \idxDist -> (fst $ head $ A.toList idxDist, snd $ head $ A.toList idxDist) -- idk how to accomplish: Scalar (Int, Float) -> (Int, Float)
           $ neighbor latLongs lat lon
    )


parseLine :: String -> (String, LatLong)
parseLine line =
  let name    = trim $ take 25 line
      latLong = ( read $ take 5 $ drop 25 line
                , read $ take 5 $ drop 30 line
                ) :: LatLong
  in ( name
     , latLong
     )


trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace


readLocations :: FilePath -> IO ([String], [LatLong], Int)
readLocations filename = do
  contents <- readFile filename
  let fileLines = lines contents
      locations = map parseLine fileLines
      names = map fst locations
      locs  = map snd locations
  return (names, locs, length fileLines)

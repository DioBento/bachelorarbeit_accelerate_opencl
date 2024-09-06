module Main
  ( main
  ) where

import Config
import Elimination
import Control.Monad (when, unless)
import System.IO (readFile)

import Prelude                                 as P
import Data.Array.Accelerate                   as A
import Data.Array.Accelerate.Examples.Internal as A


main :: IO ()
main = do
  beginMonitoring
  (conf, opts, rest) <- parseArgs options defaults header footer

  let gaussian = gaussianElim (_optBackend opts)
      size     = _configSize conf
      verbose  = _configVerbose conf
      filepath = _configFilePath conf

  (mat, vec) <- getMatVec filepath size

  runBenchmarks opts rest
    [ bench "gauss-jordan" $ whnf (gaussian mat) vec ]

  when verbose
    (print $ gaussian mat vec)


getMatVec :: Maybe FilePath -> Int -> IO (FMat, FVec)
getMatVec mFilename size =
    case mFilename of
      Just filename -> do
                     (mat, vec, n) <- readInputFile filename
                     return ( fromList (Z :. n :. n) mat
                            , fromList (Z :. n) vec
                            )
      Nothing -> do
        unless (size P.> 0)
                   (error "usage: accelerate-gaussian -v -s n")
        let mat = createMatrix size
            vec = P.take size $ repeat 1
        return ( fromList (Z :. size :. size) mat
               , fromList (Z :. size) vec
               )


createMatrix :: Int -> [Float]
createMatrix size = concat [[coe P.!! (size - 1 - i + j) | j <- [0 .. size - 1]] | i <- [0 .. size - 1]]
  where
    lamda   = -0.01 :: Float
    coe     = [coe_i i | i <- [-(size - 1) .. size - 1]]
    coe_i i = 10 * exp (lamda * P.fromIntegral (abs i))


readInputFile :: FilePath -> IO ([Float], [Float], Int)
readInputFile filename = do
  contents <- readFile filename
  let (size:rest) = lines contents
      n = read size :: Int
      matrix = P.map read $ concatMap words (P.take n rest)
      vector = P.map read $ words (rest P.!! n)
  return (matrix, vector, n)

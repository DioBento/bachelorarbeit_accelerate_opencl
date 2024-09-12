module Main
  ( main
  ) where

import Config
import Elimination
import Control.Monad (when, unless)
import Data.Label (get)

import Prelude                                 as P
import Data.Array.Accelerate                   as A
import Data.Array.Accelerate.Examples.Internal as A


main :: IO ()
main = do
  beginMonitoring
  (conf, opts, rest) <- parseArgs options defaults header footer

  let gaussian = gaussianElim (get optBackend opts)
      size_    = get configSize conf
      verbose  = get configVerbose conf
      filepath = get configFilePath conf

  (mat, vec) <- getMatVec filepath size_

  runBenchmarks opts rest
    [ bench "gauss-jordan" $ nf (gaussian mat) vec ]

  when verbose
    (print $ gaussian mat vec)


getMatVec :: Maybe FilePath -> Int -> IO (FMat, FVec)
getMatVec mFilename size_ =
    case mFilename of
      Just filename -> do
                     (mat, vec, n) <- readInputFile filename
                     return ( fromList (Z :. n :. n) mat
                            , fromList (Z :. n) vec
                            )
      Nothing -> do
        unless (size_ P.> 0)
                   (error "usage: accelerate-gaussian -v -s n")
        let mat = createMatrix size_
            vec = P.take size_ $ repeat 1
        return ( fromList (Z :. size_ :. size_) mat
               , fromList (Z :. size_) vec
               )


createMatrix :: Int -> [Float]
createMatrix size_ = concat [[coe P.!! (size_ - 1 - i + j) | j <- [0 .. size_ - 1]] | i <- [0 .. size_ - 1]]
  where
    lamda   = -0.01 :: Float
    coe     = [coe_i i | i <- [-(size_ - 1) .. size_ - 1]]
    coe_i i = 10 * exp (lamda * P.fromIntegral (abs i))


readInputFile :: FilePath -> IO ([Float], [Float], Int)
readInputFile filename = do
  contents <- readFile filename
  let (size_:rest) = lines contents
      n = read size_ :: Int
      matrix = P.map read $ concatMap words (P.take n rest)
      vector = P.map read $ words (rest P.!! n)
  return (matrix, vector, n)

{-# LANGUAGE NoImplicitPrelude #-}

module NearestNeighbour
  ( nearestNeighbor
  , LatLong
  ) where

import Data.Array.Accelerate                   as A
import Data.Array.Accelerate.Examples.Internal as A


type LatLong = (Float, Float)


nearestNeighbor :: Backend -> Vector LatLong -> Float -> Float -> Scalar (Int, Float)
nearestNeighbor backend ll la lo = run backend nn
  where nn = nearestNeighbor' (use ll) (constant la) (constant lo)


nearestNeighbor' :: Acc (Vector LatLong)
                 -> Exp Float
                 -> Exp Float
                 -> Acc (Scalar (Int, Float))
nearestNeighbor' latLngs lat long = map convert $ fold closestNeighbor neutral idxDistances
  where
    idxDistances = indexed $ map pitagora latLngs
    infinity = constant (1/0) :: Exp Float
    neutral  = (T2 (I1 0) infinity)

    pitagora :: Exp LatLong -> Exp Float
    pitagora latLong = sqrt $ (lat - fst latLong) ** 2 + (long - snd latLong) ** 2

    closestNeighbor it1@(T2 _ v) it2@(T2 _ w) = v <= w ? (it1, it2)
    convert (T2 id x) = T2 (unindex1 id) x

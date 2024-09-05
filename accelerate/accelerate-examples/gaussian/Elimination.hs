{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Elimination
  ( gaussianElim
  , FMat
  , FVec
  ) where

import Data.Array.Accelerate                   as A
import Data.Array.Accelerate.Examples.Internal as A


type FMat = Matrix Float
type FVec = Vector Float


gaussianElim :: Backend -> FMat -> FVec -> FMat
gaussianElim backend a x = run backend (gaussianElim' (use a) (use x))


gaussianElim' :: Acc FMat -> Acc FVec -> Acc FMat
gaussianElim' a x = afst $ awhile condition function_ matrix
  where
    Z :. dim :. _ = unlift $ shape a :: Z :. Exp Int :. Exp Int
    condition = (\(T2 _ iter) -> unit $ the iter < dim)
    function_ = normalize . findDiagonal
    matrix    = T2 (join a x) (unit $ constant 0)


join :: Acc FMat -> Acc FVec -> Acc FMat
join m v = m ++ v'
  where Z :. rows :. _ = unlift $ shape m :: Z :. Exp Int :. Exp Int
        v' = reshape (I2 rows 1) v


findDiagonal :: Acc (FMat, Scalar Int) -> Acc (FMat, Scalar Int)
findDiagonal orig@(T2 mat (the -> iter)) =
  let
    -- first row index with non-zero value in iter column
    rowIdx = the
           $ sum
           $ take 1
           $ map (unindex1 . fst)
           $ afst
           $ filter (\(T2 _ v) -> v /= 0 ? (True_, False_))
           $ indexed
           $ slice mat (lift (Z :. All :. iter))

    swapWith :: Exp Int -> Acc FMat
    swapWith other =
      permute const mat (\ix@(I2 row col) ->
                            Just_ $ cond (row == other) (I2 iter col)
                                  $ cond (row == iter) (I2 other col)
                                  $ ix
                        ) mat
  in
    mat ! I2 iter iter /= 0 ?| (orig, (T2 (swapWith rowIdx) (unit iter)))


normalize :: Acc (FMat, Scalar Int) -> Acc (FMat, Scalar Int)
normalize (T2 mat (the -> iter)) =
  let
    pivot = mat ! I2 iter iter :: Exp Float
    mat'  = imap (\(I2 i _) x -> i == iter ? (x / pivot, x)) mat

    -- elemOp :: Shape sh => Exp sh -> Exp Float -> Exp Float
    elemOp (unlift -> I2 row col) el = row == iter ? (el, el - mul * pEl)
      where
        mul = mat' ! I2 row iter :: Exp Float
        pEl = mat' ! I2 iter col :: Exp Float

  in
    T2 (imap elemOp mat') (unit $ iter + 1)

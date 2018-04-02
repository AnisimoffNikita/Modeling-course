{-# LANGUAGE BangPatterns #-}
module Task3.Math where

import Data.List (scanl')

tridiagonalSolve :: [Double] -> [Double] -> [Double] -> [Double] -> [Double]
tridiagonalSolve a b c f = x
  where
    ab = f0 (0:a) b c f [(0, 0)]

    f0 (a:as) (b:bs) (c:cs) (f:fs) r@((!alpha, !beta):rs) =
      f0 as bs cs fs (((-b) / (a * alpha + c), (f - a * beta) / (a * alpha + c)):r)
    f0 _ _ _ _ r = r

    (alpha', beta') = head ab
    a' = last a
    x' = (last f - a' * beta')/(last c + a' * alpha')

    x = tail.reverse $ scanl' (\x (alpha, beta) -> alpha*x + beta) x' ab


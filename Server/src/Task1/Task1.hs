module Task1.Task1 where

import Data.List (foldl', scanl', transpose)
import Data.Map (toList, fromListWith)

f :: Double -> Double -> Double
f x u = x ^ 2 + u ^ 2

x0 :: Double
x0 = 0.0

u0 :: Double
u0 = 0.0

euler :: Double -> Double -> Double -> Double
euler x y h = y + h * f x y

picard :: Double -> Double -> Double -> Double -> Double
picard n x _ _ = u0 + polynom x (p n)
  where
    integrate = map (\(a, b) -> (a + 1, b / (a + 1)))
    sqr p = toList . fromListWith (+) $ [(a + c, b * d) | (a, b) <- p, (c, d) <- p]
    polynom x = foldl' (\acc (a, b) -> acc + (x ** a) * b) 0
    p n = foldl' (\acc x -> integrate $ (2, 1) : sqr acc) [] [0 .. n - 1]

runge_kutta2 :: Double -> Double -> Double -> Double
runge_kutta2 x y h = y + h * f (x + 0.5 * h) (y + 0.5 * h * f x y)

runge_kutta4 :: Double -> Double -> Double -> Double
runge_kutta4 x y h = y + (h/6) * (k1 + 2*k2 + 2*k3 + k4)
  where
    k1 = f x y
    k2 = f (x + h/2) (y + h*k1/2)
    k3 = f (x + h/2) (y + h*k2/2)
    k4 = f (x + h) (y + h*k3)


find :: ([Double] -> [Double]) -> (Double -> Double -> Double -> Double) -> Double -> Double -> [Double]
find part method h xmax = part (scanl' (\y x -> method x y h) u0 [x0, h .. xmax])

task1 :: Double -> Double -> Double -> [[Double]]
task1 n h xmax = transpose [ [x0, h .. xmax]
                           , find init euler h xmax
                           , find tail (picard 3) h xmax
                           , find tail (picard 5) h xmax
                           , find tail (picard n) h xmax
                           , find init runge_kutta2 h xmax
                           , find init runge_kutta4 h xmax ]

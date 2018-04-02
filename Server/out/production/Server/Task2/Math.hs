module Task2.Math where

import Data.List (foldl', transpose)

import Debug.Trace


interpolate1d :: [[Double]] -> Int -> Double -> Double
interpolate1d table yCol x = y
  where
    loop0 (a:b:xs)
      | x < head a && x < head b = ((head a, a !! yCol), (head b, b !! yCol))
      | otherwise = loop (a:b:xs)

    loop (a:b:xs)
      | x >= head a && x > head b && null xs = ((head a, a !! yCol), (head b, b !! yCol))
      | x >= head a && x > head b = loop (b:xs)
      | x >= head a && x < head b = ((head a, a !! yCol), (head b, b !! yCol))
      | otherwise = ((head a, a !! yCol), (head b, b !! yCol))


    ((xp, yp),(xn, yn)) = loop0 table

    y = yp + ((yn - yp) / (xn - xp)) * (x - xp);

interpolate2d' :: [[Double]] -> Double -> Double -> Double
interpolate2d' table x y = res
  where
    cols = length . head $ table
    vec = foldr f [] [1 .. cols - 1]
    f col acc = interpolate1d (tail table) col x : acc

    table' = transpose [tail.head $ table, vec]

    res = interpolate1d table' 1 y

interpolate1d' :: [[Double]] -> Int -> Int -> Double -> Double
interpolate1d' table n yCol x = undefined
  where
    (lower, higher) = span (\r -> head r < x) table
    l = length table
    ll = length lower
    lh = length higher
    nl' = n `quot` 2 + n `rem` 2
    nh' = n `quot` 2
    (nl, nh)
      | nl' > ll = (ll, nh' + nl' - ll)
      | nh' > lh = (nl' + nh' - lh, lh)
      | otherwise = (nl, nh)
    using = drop (l - nl) lower ++ take nh higher

    xs = map head table
    ys = map (!! yCol) table






zipList :: [Double] -> [Double] -> [[Double]]
zipList _ [] = []
zipList [] _ = []
zipList (x:xs) (y:ys) = [x,y] : zipList xs ys

interpolate2d :: [[Double]] -> Double -> Double -> Double
interpolate2d table x y = res
  where
    ttable = tail table
    row' = tail (head table)
    col' = map (\(x:_) -> x) ttable

    vec = map f ttable

    f row = interpolate1d (zipList row' (tail row)) 1 y

    table' = zipList col' vec
    res = interpolate1d table' 1 x



integrate :: Double -> (Double -> Double) -> Double -> Double -> Double
integrate n f low high = step * sum / 3
  where
    step = (high - low) / (2*n)

    accf acc x = acc + 2 * f x + 4 * f (x - step)

    fsum = foldl' accf 0 [low + i*2*step | i <- [1 .. n - 1]]
    sum = f low + f high + 4 * f (high - step) + fsum


rungeKutta4 :: Double -> Double -> Double -> (Double -> Double -> Double) -> Double
rungeKutta4 x y h f = y + (h/6) * (k1 + 2*k2 + 2*k3 + k4)
  where
    k1 = f x y
    k2 = f (x + h/2) (y + h*k1/2)
    k3 = f (x + h/2) (y + h*k2/2)
    k4 = f (x + h) (y + h*k3)


halfDivision :: (Double -> Double) -> Double -> Double -> Double
halfDivision f l r = y
  where
    yl = f l
    yr = f r
    m = (l + r) / 2
    ym = f m
    eps = 1e-4
    y
      | abs (yl - yr) < eps * (1 + abs yl) = m
      | yl * ym > 0 = halfDivision f m r
      | otherwise = halfDivision f l m

getCol table i = transpose table !! i
getRow table i = table !! i
module Task4.Collocation where 

import Task4.Basic
import Control.Monad.Reader
import Data.Matrix
import Control.Monad
import Debug.Trace

params = Parameters 
  { getT0 = 8000
  , getTw = 2000
  , getPower = 2
  , getK0 = 0.04
  , getNu = 0.6
  , getR = 0.35
  , getDelta = 0.2
  , getM = 0.786
  , getH = 0.001
  , getPoints = 10
  }

uk :: Double -> Double -> Double
uk k x = (x ** (2*k-2))*(1 - x * x) + x ** 5 - x ** 3

uk' :: Double -> Double -> Double
uk' 1 x = -2*x + 5*(x ** 4) - 3*(x ** 2)
uk' k x = (2*k-2)*(x ** (2*k-3)) - 2*k*(x ** (2*k-1)) + 5*(x ** 4) - 3*(x ** 2)

uk'' :: Double -> Double -> Double
uk'' 1 x = -2 + 20*(x ** 3) - 6*x;
uk'' k x = (2*k-2)*(2*k-3)*(x ** (2*k-4)) - 2*k*(2*k-1)*(x ** (2*k-2)) + 20*(x ** 3) - 6*x;

cq :: Double -> Double'
cq x = do 
  p <- pTilde x
  r <- asks $ getR.getParams
  kx <- k x 
  return $ p * p / (3 * r * r * kx)

cp :: Double -> Double'
cp x = do 
  zx <- z x
  kx <- k x
  px <- pTilde x
  tzx <- t zx
  a <- asks getA
  yMax <- asks getYMax
  power <- asks $ getPower . getParams
  k0 <- asks $ getK0 . getParams
  tw <- asks $ getTw . getParams
  t0 <- asks $ getT0 . getParams
  r <- asks $ getR . getParams
  let 
    c1 = (1 + zx * 2 * a * yMax * x)*kx
    c2 = (zx ** power) * power * (2 * k0 / 2000 / 2000) * tzx * (tw - t0)
  return $ px * (c1 - c2) / (3 * r * r * zx * kx * kx)

cg :: Double -> Double'
cg x = negate <$> k x

cf :: Double -> Double'
cf x = (*) <$> k x <*> up x

l :: Double -> Double -> Double'
l k x = do
  q <- cq x
  p <- cp x
  g <- cg x
  return $ q * uk'' k x + p * uk' k x + g * uk k x

gauss :: [[Double]] -> [Double] -> [Double]
gauss left right = result 
  where 
    left' = fromLists left
    rows = length left'
    right' = fromList rows 1 right
    m = left' <|> right'
    result' = rref m
    result = case result' of 
      Right m -> map last . toLists $ m
      Left _ -> []

collacation' :: Double'
collacation' = do  
  params <- ask
  h <- asks $ getH . getParams
  points <- asks $ getPoints . getParams
  let 
    xs = [0,h .. 1]
    l' k x = runReader $ l k x 
    hi = 1.0 / fromIntegral points
    left = [ [ l' (fromIntegral k) x params
             | k <- [1 .. points] ]
             | x <- [hi, 2*hi .. 1] ]
  right <- mapM cf [hi, 2*hi .. 1]
  let 
    result = gauss left right

  traceShow result $ traceShow "\n"  $ traceShow left $  return 0

a = pi / 2 - getDelta params
yMax = tan a
expPower = 4.799e4 * getNu params
numerator = 6.1679e-19 * (getNu params ** 3)
internal = InternalParams a yMax expPower numerator params

collacation params = result 
  where 
    a = pi / 2 - getDelta params
    yMax = tan a
    expPower = 4.799e4 * getNu params
    numerator = 6.1679e-19 * (getNu params ** 3)
    internal = InternalParams a yMax expPower numerator params
    result = runReader collacation' internal
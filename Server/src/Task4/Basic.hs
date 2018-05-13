module Task4.Basic where

import Control.Monad.Reader

data Parameters = Parameters
  { getT0      :: Double
  , getTw      :: Double
  , getPower   :: Double
  , getK0      :: Double
  , getNu      :: Double
  , getR       :: Double
  , getDelta   :: Double
  , getM       :: Double
  , getH       :: Double
  , getPoints  :: Int
  } deriving (Show)

type Func = Double -> Double

data InternalParams = InternalParams
  { getA :: Double
  , getYMax :: Double
  , getExpPower :: Double
  , getNumerator :: Double
  , getParams :: Parameters
  } deriving (Show)

type Double' = Reader InternalParams Double

pTilde :: Double -> Double'
pTilde x = do
  a <- asks getA
  yMax <- asks getYMax
  return $ (a / yMax) * (1 + x * x * yMax * yMax)

k :: Double -> Double'
k x = do
  k0 <- asks $ getK0 . getParams
  t' <- (/2000) <$> (z x >>= t)
  return $ k0 * t' * t'


t :: Double -> Double'
t z = do
  t0 <- asks $ getT0 . getParams
  tw <- asks $ getTw . getParams
  power <- asks $ getPower . getParams
  return $ t0 + (tw - t0) * (z ** power)

z :: Double -> Double'
z x = do
  a <- asks getA
  yMax <- asks getYMax
  return $ (1.0 / a) * atan (x * yMax)

up :: Double -> Double'
up x = do 
  numerator <- asks $ getNumerator
  expPower <- asks $ getExpPower
  tzx <- z x >>= t
  return $ numerator / (exp (expPower / tzx) - 1)
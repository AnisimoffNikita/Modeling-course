module Task3.Task3(
    task3
  , Parameters(..)
  ) where

import Debug.Trace
import Control.Monad.Reader
import Task3.Math
import Data.List(transpose)
import Diagrams.Solve.Tridiagonal

data Parameters = Parameters {
    getTheta   :: Double
  , getLambda0 :: Double
  , getR       :: Double
  , getF0      :: Double
  , getL       :: Double
  , getAlpha0  :: Double
  , getAlphaN  :: Double
  , getToc     :: Double
  , getH       :: Double
  , getPower   :: Double
} deriving (Show)

type Func = Double -> Double

data InternalParams = InternalParams {
    getAlpha  :: Func
  , getLambda :: Func
  , getF      :: Func
  , getP      :: Func
  , getParams :: Parameters
}

params = Parameters{
    getTheta = 293,
    getLambda0 = 0.1,
    getR = 0.5,
    getF0 = -10,
    getL = 10,
    getAlpha0 = 0.01,
    getAlphaN = 0.005,
    getToc = 293,
    getH = 0.001,
    getPower = 2
}

chi :: Double -> Double -> Double
chi l1 l2 = 2*l1*l2 / (l1 + l2)

alpha :: Double -> Double -> Double -> Double
alpha a b x = a/(x - b)

abAlpha :: Double -> Double -> Double -> (Double, Double)
abAlpha alpha0 alphan l = (a, b)
  where
    b = (alphan*l)/(alphan - alpha0)
    a = (-alpha0) * b

lambda :: Double -> Double -> Double -> Double -> Double
lambda lambda0 theta p t = lambda0 * (t / theta) ** p

p :: Double -> Double -> Double -> Double -> Double
p a b r x = (2.0/r) * alpha a b x

f :: Double -> Double -> Double -> Double -> Double -> Double
f toc a b r x = (-2.0*toc/r) * alpha a b x

converge :: [Double] -> [Double] -> Bool
converge last current = traceShow (maximum (zipWith (\a b -> abs ((a - b) / a)) last current)) $ maximum (zipWith (\a b -> abs ((a - b) / a)) last current) < 0.5

chiList :: [Double] -> [Double]
chiList (l1:rest@(l2:lambdas)) = chi l1 l2 : chiList rest
chiList _ = []

bList :: [Double] -> [Double] -> Func -> Double -> [Double]
bList (chi1:restChi@(chi2:_)) (x:restX) p h = (-chi2 - chi1 - p x * h * h) : bList restChi restX p h
bList _ _ _ _ = []

leftCondition :: Double -> Reader InternalParams (Double, Double, Double)
leftCondition chi = do
  f0 <- asks (getF0.getParams)
  h <- asks (getH.getParams)
  f <- asks getF
  p <- asks getP
  let
    h2 = h / 2
    b0 = chi + (h * h * p h2) / 8 + (h * h * p 0) / 4
    c0 = (h * h * p h2) / 8 - chi
    d0 = h * f0 - h * h * (f h2 + f 0) / 4
  return (b0, c0, d0)


rightCondition :: Double -> Double -> Reader InternalParams (Double, Double, Double)
rightCondition chi t = do
  f0 <- asks (getF0.getParams)
  h <- asks (getH.getParams)
  l <- asks (getL.getParams)
  toc <- asks (getToc.getParams)
  alpha <- asks getAlpha
  f <- asks getF
  p <- asks getP
  let
    h2 = h / 2
    an = chi - (h * h * p (l - h2)) / 8
    bn = -(chi + (h * h * p (l - h2)) / 8 + (h * h * p l) / 4)
    dn = h * (alpha l * (t - toc)) + h * h * (f (l - h2) + f l)/4
  return (an, bn, dn)

getCoeffs :: [Double] -> [Double] -> Reader InternalParams ([Double], [Double], [Double], [Double])
getCoeffs x t = do
  lambda <- asks getLambda
  f <- asks getF
  p <- asks getP
  h <- asks (getH.getParams)
  let
    chi = chiList $ map lambda t

  (b0, c0, d0) <- leftCondition (head chi)
  (an, bn, dn) <- rightCondition (head chi) (last t)

  let
    a = init chi ++ [an]
    b = (b0 : bList chi (tail . init $ x) p h) ++ [bn]
    c = c0 : tail chi
    d = d0 : map (\x -> f x * h * h) (tail . init $ x) ++ [dn]

  traceShow (length a) $ traceShow (length b) $ traceShow (length c) $ traceShow (length d) $ return (a, b, c, d)

solve :: [Double] -> [Double] -> Reader InternalParams [(Double, Double)]
solve x t = do
  (a,b,c,d) <- getCoeffs x t

  let
    newt = tridiagonalSolve a c b d

  traceShow newt $ if converge t newt
     then return $ zip x newt
     else solve x newt

tableResult :: Reader Parameters [(Double, Double)]
tableResult = do
  toc <- asks getToc
  h <- asks getH
  l <- asks getL
  alpha0 <- asks getAlpha0
  alphaN <- asks getAlphaN
  r <- asks getR
  lambda0 <- asks getLambda0
  power <- asks getPower
  theta <- asks getTheta
  params <- ask
  let (a, b) = abAlpha alpha0 alphaN l
      alpha' = alpha a b
      lambda' = lambda lambda0 theta power
      f' = f toc a b r
      p' = p a b r
  return $ runReader (solve [0,h .. l] [toc | x <- [0,h .. l]]) (InternalParams alpha' lambda' f' p' params)



task3 :: Parameters -> [[Double]]
task3 params  = raw
  where
    raw = transpose [[x, t] | (x, t) <- runReader tableResult params]
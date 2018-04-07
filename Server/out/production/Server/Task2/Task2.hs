module Task2.Task2 where

import Task2.Math
import Task2.Data

import Debug.Trace

import Data.List(scanl', transpose)

data Parameters = Parameters {
  rk  :: Double,
  lk  :: Double,
  ck  :: Double,
  uc0 :: Double,
  i0  :: Double,
  radius :: Double,
  p0  :: Double,
  tn  :: Double,
  l   :: Double
}deriving (Show)


params = Parameters { rk = 0.2
                           , lk = 60e-6
                           , ck = 150e-6
                           , uc0 = 3000
                           , i0 = 0
                           , radius = 0.35
                           , l = 12
                           , p0 = 0.5
                           , tn = 300
                           }

interpolateSigma15 = interpolate1d table2 2
interpolateT0 = interpolate1d table3 1
interpolateN = interpolate1d table3 2

t :: Double -> Double -> (Double -> Double)
t i r = \z -> t0 + (tw - t0)*((z/r)**n)
  where
    tw = 2000
    t0 = interpolateT0 (abs i)
    n = interpolateN (abs i)

p :: Parameters -> Double -> Double
p params i = res
  where
    p0' = p0 params
    tn' = tn params

    left = p0' * 7242 / tn'

    r' = radius params

    f p z = z * interpolate2d table1 (t' z) p

    t' = t i r'

    g p = (2/(r'*r')) * integrate 20 (f p) 0 r' - left

    res = halfDivision g 3 30

rp :: Parameters -> Double -> Double
rp params i = res
  where
    r' = radius params  --берешь радиус из заданных параметров
    l' = l params --берешь длину из заданных параметров
    p' = p params i --находишь р, зная ток


    t' = t i r' --это нижняя функция, которая позволяет получить температуру
    f z = z*interpolate2d table2 (t' z) p' -- это подинтегральное выражение которое в знаменателе
    integral = integrate 20 f 0 r' --тут ты его вычисляешь, 20 это число шагов, 0 и r' пределы интегрирования

    t0 = interpolateT0 (abs i) --получаешь температуру из 3 таблицы
    res = l' / (2*pi*integral) -- ну а тут уже тупо вычисление самого rp

current :: Parameters -> Double -> Double -> Double -> Double
current params u _ i = (u - (rk params + rp params i) * i) / lk params

voltage :: Parameters -> Double -> Double -> Double -> Double
voltage params i _ u = (-i) / ck params


task2 :: Parameters -> Double -> Double -> [[Double]]
task2 params dt tmax = raw
  where
    raw = transpose [[t, i,v,r] | (t, i,v,r) <- tableResultRK params dt tmax]

tableResultRK :: Parameters -> Double -> Double -> [(Double, Double, Double, Double)]
tableResultRK params dt tmax = scanl' f (0, i0 params, uc0 params, rp params (i0 params)) [dt, 2*dt..tmax]
  where
    f (_, i, u, _) t =
      let
        i' = rungeKutta4 t i dt (current params u)
      in
      ( t
      , i'
      , rungeKutta4 t u dt (voltage params i')
      , rp params i'
      )
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Monoid(mconcat)
import Web.Scotty
import Network.HTTP.Types
import Data.Text (pack)

import Formatter
import qualified Task1.Task1 as T1
import qualified Task2.Task2 as T2
import qualified Task3.Task3 as T3

main :: IO ()
main = scotty 8081 $ do

  get "/task1/:n/:points/:max" $ do
    (n :: Double) <- param "n"
    (points :: Double) <- param "points"
    (max :: Double) <- param "max"
    let result = T1.task1 n points max
    setHeader "Access-Control-Allow-Origin" "*"
    html $ tableJsonLazy result

  get "/task2/:rk/:lk/:ck/:u0/:i0/:radius/:p0/:tn/:le/:dt/:tmax" $ do
    (rk :: Double) <- param "rk"
    (lk :: Double) <- param "lk"
    (ck :: Double) <- param "ck"
    (u0 :: Double) <- param "u0"
    (i0 :: Double) <- param "i0"
    (radius :: Double) <- param "radius"
    (p0 :: Double) <- param "p0"
    (tn :: Double) <- param "tn"
    (l :: Double) <- param "le"
    (dt :: Double) <- param "dt"
    (tmax :: Double) <- param "tmax"
    let params = T2.Parameters {
        T2.rk = rk
      , T2.lk = lk
      , T2.ck = ck
      , T2.uc0 = u0
      , T2.i0 = i0
      , T2.radius = radius
      , T2.l = l
      , T2.p0 = p0
      , T2.tn = tn
    }

    let result = T2.task2 params dt tmax
    setHeader "Access-Control-Allow-Origin" "*"
    html $ tableJsonLazy result

  post "/task3" $ do
    (theta :: Double) <- param "theta"
    (lambda0 :: Double) <- param "lambda0"
    (r :: Double) <- param "r"
    (f0 :: Double) <- param "f0"
    (l :: Double) <- param "l"
    (alpha0 :: Double) <- param "alpha0"
    (alphaN :: Double) <- param "alphaN"
    (toc :: Double) <- param "toc"
    (h :: Double) <- param "h"
    (power :: Double) <- param "power"
    let params = T3.Parameters {
        T3.getTheta = theta
      , T3.getLambda0 = lambda0
      , T3.getR = r
      , T3.getF0 = f0
      , T3.getL = l
      , T3.getAlpha0 = alpha0
      , T3.getAlphaN = alphaN
      , T3.getToc = toc
      , T3.getH = h
      , T3.getPower = power
    }
    let result = T3.task3 params
    setHeader "Access-Control-Allow-Origin" "*"
    html $ tableJsonLazy result


  notFound $ do
    status notFound404
    html " <h1>Not found</h1> "

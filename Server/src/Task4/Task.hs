module Task4.Task
  ( task
  , Parameters(..)
  )
where

import           Debug.Trace
import           Control.Monad.Reader
import           Data.List                      ( transpose )
import           Diagrams.Solve.Tridiagonal

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
  , getPoints  :: Double
  } deriving (Show)

type Func = Double -> Double

tableResult :: Reader Parameters [(Double, Double)]
tableResult = undefined
 

task :: Parameters -> [[Double]]
task params = raw
  where raw = transpose [ [x, t] | (x, t) <- runReader tableResult params ]

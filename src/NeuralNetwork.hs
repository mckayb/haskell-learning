module NeuralNetwork where

import Prelude
import Data.Vector (Vector)
import Data.Matrix (Matrix)
import qualified Data.Vector as V
import qualified Data.Matrix as M
import qualified System.Random as R

-- TODO: Cleanup the types here. They're... not useful

sigmoid :: (Num a, Fractional a, Floating a) => a -> a
sigmoid x = 1 / (1 + exp (-x))

sigmoid' :: Num a => a -> a
sigmoid' x = x * (1 - x)

train :: Integer -> Matrix Double -> Matrix Double -> IO (Matrix Double)
train iterations inputs outputs = do
  initialWeights' <- V.generateM (M.nrows inputs - 1) (const (R.randomRIO (-1, 1) :: IO Double))
  pure $ foldr (loop inputs outputs) (M.colVector initialWeights') [1..iterations]

train' :: Integer -> Matrix Double -> Matrix Double -> Matrix Double -> Matrix Double
train' iterations initialWeights inputs outputs = foldr (loop inputs outputs) initialWeights [1..iterations]

loop :: Matrix Double -> Matrix Double -> Integer -> Matrix Double -> Matrix Double
loop inputs outputs _ weights = weights + adjustments
  where
    newOutput = think weights inputs
    error = outputs - newOutput
    changes = sigmoid' <$> newOutput
    adjustments = M.multStd (M.transpose inputs) (M.multStd error (M.transpose changes))

think :: Matrix Double -> Matrix Double -> Matrix Double
think weights inputs = sigmoid <$> M.multStd inputs weights

predict :: Matrix Double -> Matrix Double -> Matrix Double
predict = think

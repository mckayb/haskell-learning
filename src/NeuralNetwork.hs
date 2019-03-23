module NeuralNetwork where

import Prelude
import Data.Vector (Vector)
import Data.Matrix (Matrix)
import qualified Data.Vector as V
import qualified Data.Matrix as M

sigmoid :: (Num a, Fractional a, Floating a) => a -> a
sigmoid x = 1 / (1 + exp (-x))

sigmoid' :: Num a => a -> a
sigmoid' x = x * (1 - x)

train :: Matrix Double -> Matrix Double -> Integer -> Matrix Double
train inputs outputs iterations = foldr go initialWeights [1..iterations]
  where
    go :: Integer -> Matrix Double -> Matrix Double
    go _ weights =
      let newOutput = think inputs weights
          error = outputs - newOutput
          changes = sigmoid' <$> newOutput
          adjustments = M.multStd (M.transpose inputs) (M.multStd error (M.transpose changes))
       in weights + adjustments

think :: Matrix Double -> Matrix Double -> Matrix Double
think inputs weights = sigmoid <$> M.multStd inputs weights

inputs :: Matrix Double
inputs = M.fromLists [ [0, 0, 1]
                     , [1, 1, 1]
                     , [1, 0, 1]
                     , [0, 1, 1]
                     ]

outputs :: Matrix Double
outputs = M.colVector $ V.fromList [0, 1, 1, 0]

neuralNetwork :: Matrix Double
neuralNetwork = think input trained
  where
    input = M.rowVector $ V.fromList [1, 0, 0]
    trained = train inputs outputs 15000

initialWeights :: Matrix Double
initialWeights = M.fromLists [ [-0.16595599]
                             , [0.44064899]
                             , [-0.99977125]
                             ]




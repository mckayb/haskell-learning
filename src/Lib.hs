module Lib where

import qualified SimpleLinearRegression
import qualified NeuralNetwork
import qualified Data.Vector as V
import qualified Data.Matrix as M

slr :: IO ()
slr = do
  putStrLn "Starting Simple Linear Regression: \n"
  let inputs = V.fromList [1100, 1200, 1300, 1400, 1500, 1600, 1700, 1800, 1900, 2000]
  let outputs = V.fromList [119000, 126000, 133000, 150000, 161000, 163000, 169000, 182000, 201000, 209000]
  let coefficients = SimpleLinearRegression.fit inputs outputs
  let predictedValue = SimpleLinearRegression.predict coefficients $ V.fromList [1750, 1000]
  putStrLn "Predicted Value: \n"
  print predictedValue
  putStrLn "End of Simple Linear Regression: \n"

nn :: IO ()
nn = do
  putStrLn "Starting NeuralNetwork: \n"
  let inputs = M.fromLists [ [0, 0, 1]
                           , [1, 1, 1]
                           , [1, 0, 1]
                           , [0, 1, 1]
                           ]
  let outputs = M.colVector $ V.fromList [0, 1, 1, 0]
  trainedWeights <- NeuralNetwork.train 15000 inputs outputs
  let predictedValue = NeuralNetwork.predict trainedWeights (M.rowVector $ V.fromList [1, 0, 0])
  putStrLn "Result: \n"
  print predictedValue
  putStrLn "End of NeuralNetwork: \n"
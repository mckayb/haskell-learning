module Lib where

import SimpleLinearRegression
import NeuralNetwork
import qualified Data.Vector as V

slr :: IO ()
slr = do
  putStrLn "Starting Simple Linear Regression: \n"
  let inputs = V.fromList [1100, 1200, 1300, 1400, 1500, 1600, 1700, 1800, 1900, 2000]
  let outputs = V.fromList [119000, 126000, 133000, 150000, 161000, 163000, 169000, 182000, 201000, 209000]
  let coefficients = fit inputs outputs
  let predictedValue = predict coefficients $ V.fromList [1750, 1000]
  putStrLn "Predicted Value: \n"
  print predictedValue
  putStrLn "End of Simple Linear Regression: \n"

nn :: IO ()
nn = do
  putStrLn "Starting NeuralNetwork: \n"
  let res = neuralNetwork
  putStrLn "Result: \n"
  print res
  putStrLn "End of NeuralNetwork: \n"
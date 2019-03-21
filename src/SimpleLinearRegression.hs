module SimpleLinearRegression where

import Prelude hiding (zipWith)
import Data.Vector (Vector, zipWith)

newtype Intercept a = Intercept a
  deriving (Show, Eq)

newtype Slope a = Slope a
  deriving (Show, Eq)

fit :: Vector Double -> Vector Double -> (Intercept Double, Slope Double)
fit xs ys = (Intercept intercept, Slope slope)
  where
    sumXs = sum xs
    sumYs = sum ys
    sumXYs = sum $ zipWith (*) xs ys
    sumXX = sum $ zipWith (*) xs xs
    n = fromIntegral $ length xs
    slope = ((n * sumXYs) - (sumXs * sumYs)) / ((n * sumXX) - (sumXs ** 2))
    intercept = (sumYs - (slope * sumXs)) / n

predict :: (Intercept Double, Slope Double) -> Vector Double -> Vector Double
predict (Intercept b, Slope m) = fmap (\x -> m * x + b)
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Examples.Iris where

import Control.Monad(mzero)
import Data.ByteString(ByteString)
import Data.Csv(HasHeader(..), FromField(..), FromRecord(..), (.:))
import Data.Either (Either(Left, Right))
import Data.Vector (Vector)
import GHC.Generics(Generic)

import qualified Data.ByteString.Lazy.UTF8 as BL
import qualified Data.Csv as Csv

data IrisClass = Setosa | Versicolor | Virginica
  deriving (Eq, Show)

instance FromField IrisClass where
  parseField s
    | s == "Iris-setosa" = pure Setosa
    | s == "Iris-versicolor" = pure Versicolor
    | s == "Iris-virginica" = pure Virginica
    | otherwise = error "Couldn't parse this!"

data Row = Row { sepalLengthCm :: Double
               , sepalWidthCm :: Double
               , petalLengthCm :: Double
               , petalWidthCm :: Double
               , irisClass :: IrisClass
               } deriving (Eq, Show, Generic, FromRecord)

-- Run this file using stack runghc -- src/Examples/Iris.hs from the root
main :: IO ()
main = do
  contents <- readFile "./datasets/iris.csv"
  case (Csv.decode NoHeader (BL.fromString contents) :: Either String (Vector Row)) of
    Left e -> do
      print e
      error "Couldn't decode"
    Right v -> print v


  
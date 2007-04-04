{-# OPTIONS_GHC -fglasgow-exts #-}

module Operator where

import DataTypes

data OperatorDef = Unary   { opName :: String, opUnaryFunc :: (Value -> Evaluate Value) }
              | Binary  { opName :: String, opBinaryFunc :: (Value -> Value -> Evaluate Value) }

operatorsTable :: [OperatorDef]

numericUnaryOp :: (Double -> Double) -> Value -> Evaluate Value

numericBinaryOp :: (Double -> Double -> Double) -> Value -> Value -> Evaluate Value

bitwiseBinaryOp :: (Int -> Int -> Int) -> Value -> Value -> Evaluate Value

(.+.) :: Value -> Value -> Evaluate Value

(.%.) :: Value -> Value -> Evaluate Value

(.~.) :: Value -> Evaluate Value

(.>>.) :: Value -> Value -> Evaluate Value
(.<<.) :: Value -> Value -> Evaluate Value
(.>>>.) :: Value -> Value -> Evaluate Value

(.==.) :: Value -> Value -> Evaluate Value
(.!=.) :: Value -> Value -> Evaluate Value

(.===.) :: Value -> Value -> Evaluate Value
(.!==.) :: Value -> Value -> Evaluate Value

instanceofOperator :: Value -> Value -> Evaluate Value

inOperator :: Value -> Value -> Evaluate Value

comparisonOp :: (forall a. (Ord a) => a -> a -> Bool) -> Value -> Value -> Evaluate Value

applyNumericOp :: (Double -> Double -> a) -> Number -> Number -> Maybe a

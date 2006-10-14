{-# OPTIONS_GHC -fglasgow-exts #-}

module Operator where
import Data.Bits

import DataTypes
import Eval
import Context
import Internal

data Operator = Unary   { opName :: String, opUnaryFunc :: (Value -> Evaluate Value) }
              | Binary  { opName :: String, opBinaryFunc :: (Value -> Value -> Evaluate Value) }
              | Ternary { opName :: String, opTernaryFunc :: (Value -> Value -> Value -> Evaluate Value) }

operatorsTable :: [Operator]
operatorsTable = [
        Unary  "+"  $ numericUnaryOp id,
        Unary  "-"  $ numericUnaryOp negate,
        Binary "*"  $ numericBinaryOp (*),
        Binary "/"  $ numericBinaryOp (/),
        Binary "%"  $ rem',
        Binary "+"  $ (+~),
        Binary "-"  $ numericBinaryOp (-),
        Binary "<"  $ comparisonOp (<),
        Binary ">"  $ comparisonOp (>),
        Binary ">=" $ comparisonOp (>=),
        Binary "<=" $ comparisonOp (<=),
        Binary "==" $ comparisonOp (==),
        Binary "!=" $ comparisonOp (/=),
        Binary "in" $ inOperator
    ]

numericUnaryOp :: (Double -> Double) -> Value -> Evaluate Value
numericUnaryOp op x = 
    case x of
        (Number n) -> return $ Number $ Double $ op $ toDouble n
        _ -> do n <- toNumber x
                numericUnaryOp op (Number n)

numericBinaryOp :: (Double -> Double -> Double) -> Value -> Value -> Evaluate Value
numericBinaryOp op x y = 
    case (x, y) of
        (Number n, Number m) -> return $ Number $ maybe NaN Double (applyNumericOp op n m)
        _ -> do n <- toNumber x
                m <- toNumber y
                numericBinaryOp op (Number n) (Number m)

(+~) :: Value -> Value -> Evaluate Value
x +~ y =
    case (x, y) of
         (Number n, Number m) -> numericBinaryOp (+) x y
         _ -> do s <- toString x
                 t <- toString y
                 return $ String $ s ++ t

rem' :: Value -> Value -> Evaluate Value
rem' (Number (Integer n)) (Number (Integer m)) =
    return $ Number $ Integer $ rem n m

rem' (Number n) (Number m) =
    let n' = toDouble n
        m' = toDouble m
        in return $ Number $ Double $ n' - m' * fromIntegral (floor (n' / m'))

rem' _ _ = return $ Number NaN

inOperator :: Value -> Value -> Evaluate Value
inOperator name object =
    if isPrimitive object
       then do objStr <- toString object
               throw $ TypeError $ "invalid 'in' operand " ++ objStr
       else do name <- toString name
               hasProp <- hasProperty object name
               return $ Boolean hasProp

-- http://www2u.biglobe.ne.jp/~oz-07ams/prog/ecma262r3/11_Expressions.html#section-11.8.5
comparisonOp :: (forall a. (Ord a) => a -> a -> Bool) -> Value -> Value -> Evaluate Value
comparisonOp op x y =
    do x <- toPrimitive x "Number"
       y <- toPrimitive y "Number"
       case (x, y) of
            (String s, String t) -> return $ Boolean $ s `op` t
            _ -> do n <- toNumber x
                    m <- toNumber y
                    return $ Boolean $ maybe False id (applyNumericOp op n m)

applyNumericOp :: (Double -> Double -> a) -> Number -> Number -> Maybe a
applyNumericOp _ NaN _ = Nothing
applyNumericOp _ _ NaN = Nothing
applyNumericOp op n m = Just $ (toDouble n) `op` (toDouble m)

toDouble :: Number -> Double
toDouble (Integer n) = fromIntegral n
toDouble (Double n)  = n

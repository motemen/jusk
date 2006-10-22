{-# OPTIONS_GHC -fglasgow-exts #-}

module Operator (Operator (..), operatorsTable, comparisonOp) where
import Monad
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
        Unary  "+"   $ numericUnaryOp id,
        Unary  "-"   $ numericUnaryOp negate,

        Unary  "~"   $ (.~.),
        Unary  "!"   $ (.!.),

        Binary "*"   $ numericBinaryOp (*),
        Binary "/"   $ numericBinaryOp (/),
        Binary "%"   $ (.%.),

        Binary "+"   $ (.+.),
        Binary "-"   $ numericBinaryOp (-),

        Binary ">>"  $ (.>>.),
        Binary "<<"  $ (.<<.),
        Binary ">>>" $ (.>>>.),

        Binary "<"   $ comparisonOp (<),
        Binary ">"   $ comparisonOp (>),
        Binary ">="  $ comparisonOp (>=),
        Binary "<="  $ comparisonOp (<=),
--      Binary "instanceof"
--                   $ instanceofOperator,
        Binary "in"  $ inOperator,

        Binary "=="  $ comparisonOp (==),
        Binary "!="  $ comparisonOp (/=),

        Binary "&"   $ bitwiseBinaryOp (.&.),
        Binary "|"   $ bitwiseBinaryOp (.|.),
        Binary "^"   $ bitwiseBinaryOp (xor)
    ]

numericUnaryOp :: (Double -> Double) -> Value -> Evaluate Value
numericUnaryOp op x = 
    case x of
        (Number NaN) -> return x
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

bitwiseBinaryOp :: (Int -> Int -> Int) -> Value -> Value -> Evaluate Value
bitwiseBinaryOp op n m =
    do n <- toInt n
       m <- toInt m
       return $ toValue $ n `op` m

(.+.) :: Value -> Value -> Evaluate Value
(.+.) x y =
    do x <- toPrimitive x "Number"
       y <- toPrimitive y "Number"
       if isString x || isString y
          then do s <- toString x
                  t <- toString y
                  return $ String $ s ++ t
          else do n <- toNumber x
                  m <- toNumber y
                  numericBinaryOp (+) (Number n) (Number m)

(.%.) :: Value -> Value -> Evaluate Value
(.%.) (Number (Integer n)) (Number (Integer m)) =
    return $ Number $ Integer $ rem n m

(.%.) (Number n) (Number m) =
    let n' = toDouble n
        m' = toDouble m
        in return $ Number $ Double $ n' - m' * fromIntegral (floor (n' / m'))

(.%.) _ _ = return $ Number NaN

(.~.) :: Value -> Evaluate Value
(.~.) = liftM (toValue . complement) . toInt

(.!.) :: Value -> Evaluate Value
(.!.) = liftM (toValue . not) . toBoolean

(.>>.) :: Value -> Value -> Evaluate Value
(.>>.) n m =
    do n <- toInt n
       m <- liftM (0x1F .&.) (toUInt m)
       return $ toValue $ n `shiftR` m

(.<<.) :: Value -> Value -> Evaluate Value
(.<<.) n m =
    do n <- toInt n
       m <- liftM (0x1F .&.) (toUInt m)
       return $ toValue $ n `shiftL` m

(.>>>.) :: Value -> Value -> Evaluate Value
(.>>>.) n m =
    do n <- toUInt n
       m <- liftM (0x1F .&.) (toUInt m)
       return $ toValue $ foldl clearBit (n `shiftR` m) [31,30..(31-m+1)]

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
toDouble NaN = undefined

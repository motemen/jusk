{-# OPTIONS_GHC -fglasgow-exts #-}

module Operator where
import Monad
import Data.Bits

import DataTypes
import Eval
import Internal

data OperatorDef = Unary   { opName :: String, opUnaryFunc :: Value -> Evaluate Value }
                 | Binary  { opName :: String, opBinaryFunc :: Value -> Value -> Evaluate Value }

-- hmm
opUnaryFunc' = opUnaryFunc
opBinaryFunc' = opBinaryFunc

operatorsTable :: [OperatorDef]
operatorsTable = [
        Unary "typeof" $ typeOf,

        Unary  "+"   $ numericUnaryOp id,
        Unary  "-"   $ numericUnaryOp negate,

        Unary  "~"   $ (.~.),
        Unary  "!"   $ (.!.),

        Binary "*"   $ numericBinaryOp (*),
        Binary "/"   $ numericBinaryOp (/),
        Binary "%"   $ withNoRef2 (.%.),

        Binary "+"   $ (.+.),
        Binary "-"   $ numericBinaryOp (-),

        Binary ">>"  $ (.>>.),
        Binary "<<"  $ (.<<.),
        Binary ">>>" $ (.>>>.),

        Binary "<"   $ comparisonOp (<),
        Binary ">"   $ comparisonOp (>),
        Binary ">="  $ comparisonOp (>=),
        Binary "<="  $ comparisonOp (<=),
        Binary "instanceof" $ instanceofOperator,
        Binary "in"  $ inOperator,

        Binary "=="  $ (.==.),
        Binary "!="  $ (.!=.),

        Binary "===" $ (.===.),
        Binary "!==" $ (.!==.),

        Binary "&"   $ bitwiseBinaryOp (.&.),
        Binary "|"   $ bitwiseBinaryOp (.|.),
        Binary "^"   $ bitwiseBinaryOp (xor)
    ]

typeOf :: Value -> Evaluate Value
typeOf ref@(Reference object name) =
    do klass <- classOf object
       if klass == "Global" || klass == "Activation"
          then ifM (isBound name)
                   (getVar name >>= readRef >>= typeOf)
                   (return $ String "undefined")
          else liftM (toValue . typeString) $ readRef =<< getValue ref

typeOf object =
    liftM (toValue . typeString) $ readRef =<< getValue object

numericUnaryOp :: (Double -> Double) -> Value -> Evaluate Value
numericUnaryOp op x = 
    do x <- readRef =<< getValue x
       case x of
           (Number NaN) -> return x
           (Number n) -> return $ Number $ Double $ op $ toDouble n
           _ -> do n <- toNumber x
                   numericUnaryOp op (Number n)

numericBinaryOp :: (Double -> Double -> Double) -> Value -> Value -> Evaluate Value
numericBinaryOp op x y = 
    do n <- toNumber x
       m <- toNumber y
       return $ Number $ maybe NaN Double (applyNumericOp op n m)

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

(.==.) :: Value -> Value -> Evaluate Value
(.==.) xRef yRef =
    do xRef <- getValue xRef; yRef <- getValue yRef
       x <- readRef xRef; y <- readRef yRef
       case (x, y) of
            (Undefined, Null) -> return $ Boolean True
            (Null, Undefined) -> return $ Boolean True
            _ | isPrimitive x && isPrimitive y ->
                return $ Boolean $ x == y
            (Object { }, y) | isNumber y || isString y ->
                (.==. y) =<< toPrimitive x ""
            (x, Object { }) | isNumber x || isString x ->
                (x .==.) =<< toPrimitive y ""
            (Object { }, Object { }) ->
                return $ Boolean $ xRef == yRef
            _ -> return $ Boolean False

(.!=.) :: Value -> Value -> Evaluate Value
(.!=.) x y = liftM not' $ x .==. y
    where not' (Boolean b) = Boolean (not b)

(.===.) :: Value -> Value -> Evaluate Value
(.===.) x y =
    do x <- getValue x; y <- getValue y
       x' <- readRef x; y' <- readRef y
       if isPrimitive x' || isPrimitive y'
          then return $ toValue $ x' == y'
          else return $ toValue $ x == y

(.!==.) :: Value -> Value -> Evaluate Value
(.!==.) x y = liftM not' $ x .===. y
    where not' (Boolean b) = Boolean (not b)

instanceofOperator :: Value -> Value -> Evaluate Value
instanceofOperator value klass =
    do value <- getValue value
       klass <- getValue klass
       klassProto <- getProp klass "prototype"
       liftM toValue $ liftM2 (||) (isProtoEq value klass) (isProtoEq value klassProto)
    where isProtoEq value obj =
              do proto <- prototypeOf value
                 case proto of
                      Null -> return False
                      _ | proto == obj -> return True
                      _ -> isProtoEq proto obj

inOperator :: Value -> Value -> Evaluate Value
inOperator name object =
    if isPrimitive object
       then do objStr <- toString object
               throw "TypeError" $ "invalid 'in' operand " ++ objStr
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

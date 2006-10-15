module JSType where

import DataTypes

toPrimitive :: Value -> String -> Evaluate Value

toBoolean :: Value -> Evaluate Bool

toNumber :: Value -> Evaluate Number

toInteger :: Value -> Evaluate Integer
toInt :: Value -> Evaluate Int
toUInt :: Value -> Evaluate Int
toUInt16 :: Value -> Evaluate Int

toString :: Value -> Evaluate String


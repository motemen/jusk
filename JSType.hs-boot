module JSType where

import DataTypes

toString :: Value -> Evaluate String

toNumber :: Value -> Evaluate Number

toInt :: Value -> Evaluate Int
toUInt :: Value -> Evaluate Int
toUInt16 :: Value -> Evaluate Int

toPrimitive :: Value -> String -> Evaluate Value

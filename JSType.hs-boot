module JSType where

import DataTypes

toString :: Value -> Evaluate String

toNumber :: Value -> Evaluate Number

toPrimitive :: Value -> String -> Evaluate Value

--
-- Types.hs
-- 型変換
--

module Types (module Types, JSBoolean.toBoolean) where
import Monad
import Data.IORef

import DataTypes
import Internal
import JSBoolean (toBoolean, new)
import Parser
import Eval

toInteger :: Value -> Evaluate Number
toInteger object =
    do maybeNum <- toNumber object
       case maybeNum of
            NaN -> return $ Integer 0
            Double num | num == 1/0 or num == -1/0 -> return $ Double num
                       | otherwise -> return $ Integer $ truncate num

toInt32 :: Value -> Evaluate Integer
toInt32 object =
    do num <- toNumber object
       case num of
            NaN -> return 0
            Double num | num == 1/0 or num == -1/0 -> return 0
                       | otherwise -> let n = (truncate num) `rem` (floor $ 2**32)
                                          in if n >= 2**31 then return n - (floor $ 2*32)
                                                           else return n

isUndefined :: Value -> Bool
isUndefined Undefined = True
isUndefined _         = False

isNull :: Value -> Bool
isNull Null = True
isNull _    = False

isNaN :: Value -> Bool
isNaN (Number NaN) = True
isNaN _            = False

isZero :: Value -> Bool
isZero (Number (Integer num)) = num == 0
isZero (Number (Double num))  = num == 0
isZero _ = False

isNullString :: Value -> Bool
isNullString (String "") = True
isNullString _ = False

toObject :: Value -> Evaluate Value
-- TODO: Throw error for Undefined, Null
toObject object@(Object { }) = return object
toObject bool@(Boolean _)    = JSBoolean.new [bool]

{-
    JSType.hs
    型変換
-}

module JSType where
import Data.IORef

import DataTypes
import Parser (numericLiteral)
import ParserUtil (runLex)
import Internal
import Eval

toString :: Value -> Evaluate String
toString Void      = return ""
toString Undefined = return "undefined"
toString Null      = return "null"

toString (Boolean False) = return "false"
toString (Boolean True)  = return "true"

toString (String string) = return string

toString (Number (Integer n)) = return $ show n
toString (Number (Double n))  = return $ show n
toString (Number NaN)         = return "NaN"

toString (NativeFunction _) =
    return "[native function]"

toString ref@(Reference _) =
    do object <- getValue ref
       toString object

toString (Ref objRef) =
    do object <- liftAll $ readIORef objRef
       toString object

toString (Exception e) =
    return $ show e

toString object = 
    do String s <- callMethod object "toString" []
       return s

toNumber :: Value -> Evaluate Number
toNumber Undefined =
    return NaN

toNumber Null =
    return $ Integer 0

toNumber (Boolean bool) =
    return $ if bool then Integer 1
                     else Double 0.0

toNumber (Number num) =
    return num

toNumber (String string) =
    case runLex numericLiteral string of
         Left _ -> return NaN
         Right (Literal (Number n)) -> return n

toNumber object@(Object { }) =
    toPrimitive object "Number" >>= toNumber

toPrimitive :: Value -> String -> Evaluate Value
toPrimitive Undefined _ =
    return Undefined

toPrimitive Null _ =
    return Null

toPrimitive bool@(Boolean _) _ =
    return bool

toPrimitive num@(Number _) _ =
    return num

toPrimitive string@(String _) _ =
    return string

toPrimitive object preferredType =
    defaultValue object preferredType


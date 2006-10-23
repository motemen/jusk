{-
    JSFunction.hs
    Functionオブジェクト
    http://www2u.biglobe.ne.jp/~oz-07ams/prog/ecma262r3/15-3_Function_Objects.html
-}

module JSFunction where
import Monad
import qualified Data.Map as Map
import Maybe

import DataTypes
import Context
import Eval hiding(callMethod)
import Internal
import PrettyShow
import JSType

-- Function.prototype
prototypeObject :: Value
prototypeObject =
    nullObject {
        objPropMap = nativeFuncPropMap [("constructor", constructor,    1),
                                        ("toString",    toStringMethod, 0),
                                        ("call",        callMethod,     1),
                                        ("apply",       apply,          2)]
    }

toStringMethod :: NativeCode
toStringMethod _ =
    do this <- readRef =<< getThis
       if isFunction this || isNativeFunction this
          then return $ toValue $ prettyShow this
          else throw "TypeError" $ "Function.prototype.toString: " ++ show this ++ " is not a function"

-- Function
function :: NativeCode
function args = constructor args

-- new Function
constructor :: NativeCode
constructor _ = throw "NotImplemented" "Function.prototype.constructor"

-- Function.prototype.call
callMethod :: NativeCode
callMethod (thisArg:args) =
    do func <- getThis
       call thisArg func args

-- Function.prototype.apply
apply :: NativeCode
apply (thisArg:argArray:_) =
    do func <- getThis
       argArray <- readRef argArray
       case argArray of
            Array args -> call thisArg func args
            _ | isArguments argArray ->
                do length <- toUInt $ argArray ! "length"
                   args <- mapM (getProp argArray . show) [0..length-1]
                   call thisArg func args
            _ -> throw "TypeError" "second argument to Function.prototype.apply must be an array"

isArguments :: Value -> Bool
isArguments (Object { objPropMap = propMap }) =
    fromMaybe False (Map.lookup "callee" propMap >>= return . (elem DontEnum) . propAttr)

isArguments _ = False

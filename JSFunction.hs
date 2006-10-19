{-
    JSFunction.hs
    Functionオブジェクト
    http://www2u.biglobe.ne.jp/~oz-07ams/prog/ecma262r3/15-3_Function_Objects.html
-}

module JSFunction where

import DataTypes
import PrettyShow
import Context

-- Function.prototype
prototypeObject :: Value
prototypeObject =
    nullObject {
        objPropMap = mkPropMap [("constructor", NativeFunction constructor, []),
                                ("toString", NativeFunction toStringMethod, [])]
    }

toStringMethod :: NativeFunction
toStringMethod _ =
    do this <- readRef =<< getThis
       case this of
            Function { } -> return $ toValue $ prettyShow this
            Object { objValue = func@Function { } } -> return $ toValue $ prettyShow func
            _ -> throw $ TypeError ""

-- Function()
function :: NativeFunction
function args = constructor args

-- new Function()
constructor :: NativeFunction
constructor _ = throw $ NotImplemented $ "Function.prototype.constructor"

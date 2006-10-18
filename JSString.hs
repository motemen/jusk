{-
    JSString.hs
    Stringオブジェクト
    http://www2u.biglobe.ne.jp/~oz-07ams/prog/ecma262r3/15-5_String_Objects.html
-}

module JSString where

import DataTypes
import Context

-- String.prototype
prototypeObject :: Value
prototypeObject =
    nullObject {
        objPropMap = mkPropMap [("toString", NativeFunction toStringMethod, []),
                                ("valueOf",  NativeFunction valueOfMethod, [])]
    }

toStringMethod :: NativeFunction
toStringMethod _ =
    do this <- readRef =<< getThis
       case this of
            String _ -> return this
            Object { objValue = String _ } -> return this
            _ -> do throw $ ReferenceError $ "String.prototype.toString called on incompatible value"

valueOfMethod :: NativeFunction
valueOfMethod _ =
    do this <- readRef =<< getThis
       case this of
            String _ -> return this
            Object { objValue = String _ } -> return this
            _ -> do throw $ ReferenceError $ "String.prototype.valueOf called on incompatible value"

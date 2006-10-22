{-
    JSString.hs
    Stringオブジェクト
    http://www2u.biglobe.ne.jp/~oz-07ams/prog/ecma262r3/15-5_String_Objects.html
-}

module JSString where
import Prelude hiding(toInteger)
import Monad
import Data.Char

import DataTypes
import Context
import JSType

-- String.prototype
prototypeObject :: Value
prototypeObject =
    nullObject {
        objPropMap = nativeFuncPropMap [("toString",   toStringMethod, 0),
                                        ("valueOf",    valueOfMethod, 0),
                                        ("charAt",     charAt, 1),
                                        ("charCodeAt", charCodeAt, 1)]
    }

-- String()
function :: NativeCode
function [] = return $ String ""

function (x:_) = liftM String $ toString x

-- new String()
constructor :: NativeCode
constructor args =
    do string <- function args
       return $ nullObject {
           objPrototype = prototypeObject,
           objClass     = "String",
           objValue     = string
       }

toStringMethod :: NativeCode
toStringMethod _ =
    do this <- readRef =<< getThis
       case this of
            String _ -> return this
            Object { objValue = String _ } -> return this
            _ -> do throw $ ReferenceError $ "String.prototype.toString called on incompatible value"

valueOfMethod :: NativeCode
valueOfMethod _ =
    do this <- readRef =<< getThis
       case this of
            String _ -> return this
            Object { objValue = String _ } -> return this
            _ -> do throw $ ReferenceError $ "String.prototype.valueOf called on incompatible value"

charAt :: NativeCode
charAt [] = charAt [Undefined]

charAt (pos:_) =
    do pos <- toInt pos
       this <- readRef =<< getThis
       return $ case this of
                     _ | pos < 0 -> String ""
                     String string -> String $ if pos >= length string then "" else [string !! pos]

charCodeAt :: NativeCode
charCodeAt [] = charCodeAt [Undefined]

charCodeAt (pos:_) =
    do pos <- toInt pos
       this <- readRef =<< getThis
       return $ case this of
                     _ | pos < 0 -> Number NaN
                     String string -> if pos >= length string then Number NaN else toValue $ ord (string !! pos)

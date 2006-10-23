{-
    JSRegExp.hs
    正規表現のサポート
    http://www2u.biglobe.ne.jp/~oz-07ams/prog/ecma262r3/15-10_RegExp_Objects.html
-}

module JSRegExp where
import Text.Regex

import DataTypes
import Eval
import Context
import Internal

-- RegExp.prototype
prototypeObject :: Value
prototypeObject =
    nullObject {
        objPropMap = nativeFuncPropMap [("constructor", constructor,    1),
                                        ("toString",    toStringMethod, 0)]
    }

-- RegExp
function :: NativeCode
function [x] = function [x, Undefined]

function [regexp@Object { objObject = RegExp { } }, Undefined] =
    return regexp

function (r:f:_) =
    constructor [r, f]

-- new RegExp
constructor :: NativeCode
constructor [pattern, flags] =
    do pattern <- toString pattern
       flags <- toString flags
       return $ nullObject { objObject = RegExp { regexpRegex = mkRegex pattern, regexpPattern = pattern, regexpFlags = flags } }

-- RegExp.prototype.toString
toStringMethod :: NativeCode
toStringMethod _ =
    do this <- readRef =<< getThis
       if isRegExp this
          then return $ toValue $ "/" ++ (regexpPattern $ objObject this) ++ "/" ++ (regexpFlags $ objObject this)
          else throw "TypeError" $ getName this ++ " is not a regexp"

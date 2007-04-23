{-
    JSRegExp.hs
    正規表現のサポート
    http://www2u.biglobe.ne.jp/~oz-07ams/prog/ecma262r3/15-10_RegExp_Objects.html
-}

module JSRegExp where
import Maybe
import Text.Regex

import DataTypes
import Eval
import Internal

-- RegExp.prototype
prototypeObject :: Value
prototypeObject =
    nullObject {
        objPropMap = nativeFuncPropMap [
                ("constructor", constructor,    1),
                ("toString",    toStringMethod, 0)
            ]
    }

-- RegExp
function :: NativeCode
function _ [x] = function undefined [x, Undefined]

function _ [regexp@Object { objObject = RegExp { } }, Undefined] =
    return regexp

function _ (r:f:_) =
    constructor undefined [r, f]

-- new RegExp
constructor :: NativeCode
constructor _ [pattern] =
    constructor undefined [pattern, String ""]
constructor _ (pattern:flags:_) =
    do pattern <- toString pattern
       flags <- toString flags
       return $ nullObject { objObject = RegExp { regexpRegex = mkRegex pattern, regexpPattern = pattern, regexpFlags = flags }, objClass = "RegExp" }

-- RegExp.prototype.toString
toStringMethod :: NativeCode
toStringMethod this _ =
    do this <- readRef this
       if isRegExp this
          then return $ toValue $ "/" ++ (regexpPattern $ objObject this) ++ "/" ++ (regexpFlags $ objObject this)
          else throw "TypeError" $ getName this ++ " is not a regexp"

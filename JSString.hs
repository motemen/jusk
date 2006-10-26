{-
    JSString.hs
    Stringオブジェクト
    http://www2u.biglobe.ne.jp/~oz-07ams/prog/ecma262r3/15-5_String_Objects.html
-}

module JSString where
import Prelude hiding(toInteger)
import Monad
import Maybe
import Data.Char
import Text.Regex

import DataTypes
import Internal
import Eval
import JSType

-- String.prototype
prototypeObject :: Value
prototypeObject =
    nullObject {
        objPropMap = nativeFuncPropMap [("toString",   toStringMethod, 0),
                                        ("valueOf",    valueOfMethod,  0),
                                        ("charAt",     charAt,         1),
                                        ("charCodeAt", charCodeAt,     1),
                                        ("replace",    replace,        2)]
    }

-- String()
function :: NativeCode
function _ [] = return $ String ""

function _ (x:_) = liftM String $ toString x

-- new String()
constructor :: NativeCode
constructor _ args =
    do string <- function undefined args
       return $ nullObject {
           objClass = "String",
           objValue = string
       }

-- String.prototype.toString
toStringMethod :: NativeCode
toStringMethod this _ =
    do this <- readRef this
       case this of
            String _ -> return this
            Object { objValue = String _ } -> return this
            _ -> throw "ReferenceError" "String.prototype.toString called on incompatible value"

-- String.prototype.valueOf
valueOfMethod :: NativeCode
valueOfMethod this _ =
    do this <- readRef this
       case this of
            String _ -> return this
            Object { objValue = String _ } -> return this
            _ -> do throw "ReferenceError" "String.prototype.valueOf called on incompatible value"

-- String.prototype.charAt
charAt :: NativeCode
charAt this [] = charAt this [Undefined]

charAt this (pos:_) =
    do pos <- toInt pos
       this <- readRef this
       return $ case this of
                     _ | pos < 0 -> String ""
                     String string -> String $ if pos >= length string then "" else [string !! pos]

-- String.prototype.charCodeAt
charCodeAt :: NativeCode
charCodeAt this [] = charCodeAt this [Undefined]

charCodeAt this (pos:_) =
    do pos <- toInt pos
       this <- readRef this
       return $ case this of
                     _ | pos < 0 -> Number NaN
                     String string -> if pos >= length string then Number NaN else toValue $ ord (string !! pos)

-- String.prototype.replace
replace :: NativeCode
replace this (searchValue:replaceValue:_) =
    do string <- toString this
       searchValue <- readRef searchValue
       case searchValue of
            Object { objObject = RegExp { regexpRegex = regex, regexpFlags = flags } } ->
                liftM String $ doReplace string regex $ 'g' `elem` flags
    where doReplace string regex global =
              maybe (return string)
                    (\matchInfo@(before, matched, after, _) ->
                        do replaced <- getReplaceStr matchInfo
                           after <- if global && not (null matched)
                                       then doReplace after regex global
                                       else return after
                           return $ before ++ replaced ++ after)
                    (matchRegexAll regex string)
          getReplaceStr matchInfo@(before, matched, _, submatch) =
              do replaceValue' <- readRef replaceValue
                 if typeString replaceValue' == "function"
                    then (call Null replaceValue
                               $ [String matched] ++ map String submatch ++ [toValue $ length before, this]
                               ) >>= toString
                    else liftM (makeReplaceString matchInfo) $ toString replaceValue
          makeReplaceString (before, matched, after, submatch) string =
              loop string ""
              where loop ""       s = s
                    loop ('$':cs) s = inDoller cs s
                    loop (c:cs)   s = loop cs (s ++ [c])

                    inDoller ('$':cs)  s = loop cs (s ++ "$")
                    inDoller ('&':cs)  s = loop cs (s ++ matched)
                    inDoller ('`':cs)  s = loop cs (s ++ before)
                    inDoller ('\'':cs) s = loop cs (s ++ after)
                    inDoller (d:e:cs)  s
                        | isDigit d && isDigit e = loop cs     (s ++ submatch .!!. (read [d, e] - 1))
                        | isDigit d              = loop (e:cs) (s ++ submatch .!!. (read [d] - 1))
                        | otherwise              = loop (e:cs) (s ++ [d])
                    inDoller (d:cs)    s
                        | isDigit d = loop cs (s ++ submatch .!!. (read [d] - 1))
                        | otherwise = loop cs (s ++ [d])
                    xs .!!. n | n < 0 || length xs <= n = ""
                              | otherwise               = xs !! n

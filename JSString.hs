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
import JSArray (makeArray)
import JSType

-- String.prototype
prototypeObject :: Value
prototypeObject =
    nullObject {
        objPropMap = nativeFuncPropMap [("toString",    toStringMethod, 0),
                                        ("valueOf",     valueOfMethod,  0),
                                        ("charAt",      charAt,         1),
                                        ("charCodeAt",  charCodeAt,     1),
                                        ("replace",     replace,        2),
                                        ("split",       split,          2),
                                        ("substring",   substring,      2),
                                        ("toLowerCase", toLowerCase,    0),
                                        ("toUpperCase", toUpperCase,    0)]
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

charAt this args@(pos:_) =
    do pos <- toInt pos
       this <- readRef this
       case this of
            _ | pos < 0   -> return $ String ""
            String string -> return $ String $ if pos >= length string then "" else [string !! pos]
            object -> do string <- toString object
                         charCodeAt (String string) args

-- String.prototype.charCodeAt
charCodeAt :: NativeCode
charCodeAt this [] = charCodeAt this [Undefined]

charCodeAt this args@(pos:_) =
    do pos <- toInt pos
       this <- readRef this
       case this of
            _ | pos < 0   -> return $ Number NaN
            String string -> return $ if pos >= length string then Number NaN else toValue $ ord (string !! pos)
            object -> do string <- toString object
                         charCodeAt (String string) args

-- String.prototype.replace
replace :: NativeCode
replace this (searchValue:replaceValue:_) =
    do string <- toString this
       searchValue <- readRef searchValue
       case searchValue of
            Object { objObject = RegExp { regexpRegex = regex, regexpFlags = flags } } ->
                liftM String $ doReplace string regex $ 'g' `elem` flags
            _ -> do regexp <- new "RegExp" [searchValue]
                    replace this [regexp, replaceValue]
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
                    then (call replaceValue
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
                    inDoller []        s = s ++ "$"
                    xs .!!. n | n < 0 || length xs <= n = ""
                              | otherwise               = xs !! n

replace this [searchValue] =
    replace this [searchValue, Undefined]

replace this [] =
    return this

-- String.prototype.split
split :: NativeCode
split this [] =
    liftM String $ toString this

split this (separator:_) =
    do string <- toString this
       separator <- readRef separator
       case separator of
            Object { objObject = RegExp { regexpRegex = regex } } ->
                return $ makeArray $ map String $ splitRegex regex string
            _ -> do regexp <- new "RegExp" [separator]
                    split this [regexp]
                    

-- String.prototype.substring
substring :: NativeCode
substring this [] =
    substring this [Number $ Integer 0, Undefined]

substring this [s] =
    substring this [s, Undefined]

substring this (s:e:_) =
    do string <- toString this
       s <- toInt s
       e <- if isUndefined e then return $ length string else toInt e
       let (start, end) = (min s e, max s e)
       return $ String $ drop start $ take end string

-- String.prototype.toLowerCase
toLowerCase :: NativeCode
toLowerCase this _ =
    do string <- toString this
       return $ String $ map toLower string

-- String.prototype.toUpperCase
toUpperCase :: NativeCode
toUpperCase this _ =
    do string <- toString this
       return $ String $ map toUpper string

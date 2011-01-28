{-
    JSError.hs
    Error オブジェクト
    http://www2u.biglobe.ne.jp/~oz-07ams/prog/ecma262r3/15-11_Error_Objects.html
-}

module JSError where
import qualified Data.Map as Map

import DataTypes
import Eval
import Internal

-- Error.prototype
prototypeObject :: Value
prototypeObject =
    nullObject {
        objClass   = "Error",
        objPropMap = Map.union (nativeFuncPropMap [("constructor", constructor,    1),
                                                   ("toString",    toStringMethod, 0)])
                               (mkPropMap [("name", String "Error", []), ("message", String "", [])])
    }

prototypeObjectOfName :: String -> Value
prototypeObjectOfName name =
    nullObject {
        objClass   = "Error",
        objPropMap = Map.union (nativeFuncPropMap [("constructor", constructor,    1),
                                                   ("toString",    toStringMethod, 0)])
                               (mkPropMap [("name", String name, []), ("message", String "", [])])
    }

-- Error
function :: NativeCode
function = constructor

-- new Error
constructor _ args =
    do message <- if null args then return "" else toString $ head args
       return $ nullObject {
           objClass   = "Error",
           objPropMap = mkPropMap [("message", String message, [])]
       }

-- Error.prototype.toString
toStringMethod :: NativeCode
toStringMethod this _ =
    do name <- toString =<< getProp this "name"
       message <- toString =<< getProp this "message"
       return $ toValue $ name ++ ": " ++ message

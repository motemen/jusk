{-
    JSError.hs
    Error オブジェクト
    http://www2u.biglobe.ne.jp/~oz-07ams/prog/ecma262r3/15-11_Error_Objects.html
-}

module JSError where
import qualified Data.Map as Map

import DataTypes
import Eval
import Context
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

-- Error
function :: NativeCode
function = constructor

-- new Error
constructor :: NativeCode
constructor [] = constructor [(String "")]

constructor (message:_) =
    do message <- toString message
       return $ nullObject {
           objClass   = "Error",
           objPropMap = mkPropMap [("message", String message, [])]
       }

-- Error.prototype.toString
toStringMethod :: NativeCode
toStringMethod _ =
    do this <- getThis
       name <- toString =<< getProp this "name"
       message <- toString =<< getProp this "message"
       return $ toValue $ name ++ ": " ++ message

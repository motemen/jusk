{-
    JSObject.hs
    Objectオブジェクト
    http://www2u.biglobe.ne.jp/~oz-07ams/prog/ecma262r3/15-2_Object_Objects.html
-}

module JSObject where

import DataTypes
import Context
import Internal

-- Object.prototype
prototypeObject :: Value
prototypeObject =
    nullObject {
        objProperties = [("constructor", NativeFunction make),
                         ("toString",    NativeFunction toString)],
        objAttributes = [("constructor", []),
                         ("toString",    [])],
        objClass = "Object"
    }

-- Object()
function :: NativeFunction
function [] = create []

-- new Object()
new :: NativeFunction
new _ = create []

-- Object.prototype.toString
toString :: NativeFunction
toString _ =
    do this <- getThis
       String klass <- classOf this
       return $ String $ "[object " ++ klass ++ "]"

create :: [(String, Value)] -> Evaluate Value
create props = 
    return $ nullObject {
        objProperties = props,
        objAttributes = zip (map fst props) (repeat []),
        objPrototype = prototypeObject,
        objClass = "Object"
    }

make :: NativeFunction
make [] =
    return $ nullObject {
        objPrototype = prototypeObject,
        objClass = "Object"
    }

make (v:_) =
    return $ nullObject {
        objPrototype    = prototypeObject,
        objClass    = "Object",
        objDefault = v
    }

{-
    JSObject.hs
    Objectオブジェクト
    http://www2u.biglobe.ne.jp/~oz-07ams/prog/ecma262r3/15-2_Object_Objects.html
-}

module JSObject where
import Data.Map hiding(map)

import DataTypes
import Context
import Internal

-- Object.prototype
prototypeObject :: Value
prototypeObject =
    nullObject {
        objProperties = fromList [("constructor", NativeFunction make),
                                  ("toString",    NativeFunction toString)],
        objAttributes = fromList [("constructor", []),
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
        objProperties = fromList props,
        objAttributes = fromList $ zip (map fst props) (repeat []),
        objPrototype = prototypeObject,
        objClass = "Object"
    }

make :: NativeFunction
make [] =
    return $ nullObject {
        objPrototype = prototypeObject,
        objClass = "Object"
    }

make (value:_) =
    return $ nullObject {
        objPrototype = prototypeObject,
        objClass     = "Object",
        objDefault   = value
    }

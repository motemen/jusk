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
        properties = [("constructor", NativeFunction make),
                      ("toString",    NativeFunction toString)],
        attributes = [("constructor", []),
                      ("toString",    [])],
        className = "Object"
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
        properties = props,
        attributes = zip (map fst props) (repeat []),
        prototype = prototypeObject,
        className = "Object"
    }

make :: NativeFunction
make [] =
    return $ nullObject {
        prototype = prototypeObject,
        className = "Object"
    }

make (v:_) =
    return $ nullObject {
        prototype = prototypeObject,
        className = "Object",
        delegate = v
    }

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
        objPropMap = mkPropMap [("constructor", NativeFunction make, []),
                                ("toString",    NativeFunction toString, [])] ,
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
        objPropMap = fromList $ map withNoAttr props,
        objPrototype = prototypeObject,
        objClass = "Object"
    }
    where withNoAttr (name, value) = (name, mkProp value [])

make :: NativeFunction
make [] =
    return $ nullObject {
        objPrototype = prototypeObject,
        objClass     = "Object"
    }

make (value:_) =
    return $ nullObject {
        objPrototype = prototypeObject,
        objClass     = "Object",
        objValue     = value
    }

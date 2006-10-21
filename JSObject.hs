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
        objPropMap = nativeFuncPropMap [("toString", toStringMethod, 0),
                                        ("valueOf",  valueOf, 0)],
        objClass = "Object"
    }

-- Object()
function :: NativeFunction
function [] = create []

-- Object.prototype.toString
toStringMethod :: NativeFunction
toStringMethod _ =
    do this <- getThis
       klass <- classOf this
       return $ String $ "[object " ++ klass ++ "]"

-- Object.prototype.valueOf
valueOf :: NativeFunction
valueOf _ =
    do this <- getThis
       return this

create :: [(String, Value)] -> Evaluate Value
create props = 
    return $ nullObject {
        objPropMap = fromList $ map withNoAttr props,
        objPrototype = prototypeObject,
        objClass = "Object"
    }
    where withNoAttr (name, value) = (name, mkProp value [])

constructor :: NativeFunction
constructor [] =
    return $ nullObject {
        objPrototype = prototypeObject,
        objClass     = "Object"
    }

constructor (value:_) =
    return $ nullObject {
        objPrototype = prototypeObject,
        objClass     = "Object",
        objValue     = value
    }

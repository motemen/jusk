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
        objPropMap = mkPropMap [("toString", toStringMethod, [])] ,
        objClass = "Object"
    }

-- Object()
function :: NativeFunction
function [] = create []

-- Object.prototype.toString
toStringMethod :: Value
toStringMethod =
    nullNativeFunc {
        funcName = "toString",
        funcArity = 0,
        funcNatCode = nativeCode
    }
    where nativeCode _ =
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

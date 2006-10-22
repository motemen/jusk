{-
    JSObject.hs
    Objectオブジェクト
    http://www2u.biglobe.ne.jp/~oz-07ams/prog/ecma262r3/15-2_Object_Objects.html
-}

module JSObject where
import qualified Data.Map as Map hiding(map)
import List
import Monad

import DataTypes
import Context
import Internal
import Eval
import JSType

-- Object.prototype
prototypeObject :: Value
prototypeObject =
    nullObject {
        objPropMap = nativeFuncPropMap [("toString", toStringMethod, 0),
                                        ("valueOf",  valueOf,        0),
                                        ("toSource", toSourceMethod, 0)],
        objClass = "Object"
    }

-- Object
function :: NativeCode
function [] = create []

-- Object.prototype.toString
toStringMethod :: NativeCode
toStringMethod _ =
    do this <- getThis
       klass <- classOf this
       return $ String $ "[object " ++ klass ++ "]"

-- Object.prototype.valueOf
valueOf :: NativeCode
valueOf _ = getThis

-- Object.prototype.toSource
toSourceMethod :: NativeCode
toSourceMethod _ =
    do this <- toObject =<< readRef =<< getThis
       strings <- mapM pairToString $ Map.assocs $ Map.filter (notElem DontEnum . propAttr) $ objPropMap this
       return $ toValue $ "({" ++ strings `joinBy` ", " ++ "})"
    where pairToString (k, p) =
              do value <- readRef $ propValue p
                 if isPrimitive value
                    then return $ k ++ ": " ++ show value
                    else liftM ((k ++ ": ") ++) $ toString =<< callMethod value "toSource" []
          joinBy strings sep = concat $ intersperse sep strings

create :: [(String, Value)] -> Evaluate Value
create props = 
    return $ nullObject {
        objPropMap = Map.fromList $ map withNoAttr props,
        objPrototype = prototypeObject,
        objClass = "Object"
    }
    where withNoAttr (name, value) = (name, mkProp value [])

constructor :: NativeCode
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

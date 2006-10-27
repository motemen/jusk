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
        objClass = "Object",
        objObject = ULObject
    }

-- Object
function :: NativeCode
function _ [] = create []

-- Object.prototype.toString
toStringMethod :: NativeCode
toStringMethod this _ =
    do klass <- classOf this
       return $ String $ "[object " ++ klass ++ "]"

-- Object.prototype.valueOf
valueOf :: NativeCode
valueOf this _ = return this

-- Object.prototype.toSource
toSourceMethod :: NativeCode
toSourceMethod this _ =
    do this <- toObject =<< readRef this
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
constructor _ [] =
    return $ nullObject {
        objPrototype = prototypeObject,
        objClass     = "Object"
    }

constructor _ (value:_) =
    return $ nullObject {
        objPrototype = prototypeObject,
        objClass     = "Object",
        objValue     = value
    }

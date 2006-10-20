{-
    JSFunction.hs
    Functionオブジェクト
    http://www2u.biglobe.ne.jp/~oz-07ams/prog/ecma262r3/15-3_Function_Objects.html
-}

module JSFunction where
import Monad

import DataTypes
import PrettyShow
import Context
import JSType

-- Function.prototype
prototypeObject :: Value
prototypeObject =
    nullObject {
        objPropMap = nativeFuncPropMap [("constructor", constructor, 1),
                                        ("toString",    toStringMethod, 0)]
    }

toStringMethod :: NativeFunction
toStringMethod _ =
    do this <- readRef =<< getThis
       showFunc this
    where showFunc func@Function { } =
              return $ toValue $ prettyShow func
          showFunc func@(NativeFunction { }) =
              liftM toValue $ toString func
          showFunc (Object { objValue = func }) =
              showFunc func
          showFunc x =
              throw $ TypeError $ "Function.prototype.toString: " ++ show x ++ " is not a function"

-- Function()
function :: NativeFunction
function args = constructor args

-- new Function()
constructor :: NativeFunction
constructor _ = throw $ NotImplemented $ "Function.prototype.constructor"

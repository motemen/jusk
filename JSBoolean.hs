{-
    JSBoolean.hs
    Booleanオブジェクト
    http://www2u.biglobe.ne.jp/~oz-07ams/prog/ecma262r3/15-6_Boolean_Objects.html
-}

module JSBoolean where
import Monad

import DataTypes
import Internal

-- Boolean.prototype
prototype :: Evaluate Value
prototype = return $ Object { properties = [("constructor", NativeFunction JSBoolean.new)] }

-- Boolean()
function :: [Value] -> Evaluate Value
function []    = return $ Boolean False
function (x:_) = liftM Boolean $ toBoolean x

-- new Boolean()
new :: [Value] -> Evaluate Value
new args =
    do prototype <- JSBoolean.prototype
       value <- function args
       return $ Object {
                    properties = [],
                    DataTypes.prototype = prototype,
                    className = "Boolean",
                    delegate = value
                }

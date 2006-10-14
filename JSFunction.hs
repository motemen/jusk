{-
    JSFunction.hs
    Functionオブジェクト
    http://www2u.biglobe.ne.jp/~oz-07ams/prog/ecma262r3/15-3_Function_Objects.html
-}

module JSFunction where
import Data.IORef

import Internal

-- Function.prototype
prototype :: Evaluate Value
prototype = return $ Object [] []

-- Function()
--function :: [Value] -> Evaluate Value

-- new Function()
--new :: [Value] -> Evaluate Value

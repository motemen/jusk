{-
    JSMath.hs
    Mathオブジェクト
    http://www2u.biglobe.ne.jp/~oz-07ams/prog/ecma262r3/15-8_The_Math_Object.html
-}
module JSMath(createMathObject) where
import Monad

import DataTypes
import Eval
import Internal

createMathObject :: Evaluate Value
createMathObject =
    do math <- new "Object" []
       math ! "sin" <~ makeNativeFunc "sin" (unary sin) 1
       math ! "cos" <~ makeNativeFunc "cos" (unary cos) 1
       math ! "min" <~ makeNativeFunc "min" (multiary min) 2
       math ! "max" <~ makeNativeFunc "max" (multiary max) 2
       return math

makeNativeFunc name code arity =
    nativeFunc name arity code

unary :: (Double -> Double) -> NativeCode
unary _ _ [] =
    return $ Number NaN

unary _ _ (Number NaN:_) =
    return $ Number NaN

unary func _ (x:_) =
    liftM (tidyNumber . toValue . func . toDouble) (toNumber x)

multiary :: (Double -> Double -> Double) -> NativeCode
multiary _ _ [] =
    return Undefined

multiary func _ xs =
    do xs <- mapM (liftM toDouble . toNumber) xs
       return $ tidyNumber $ toValue $ foldl1 func xs

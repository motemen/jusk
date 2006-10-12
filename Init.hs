{-
    Init.hs
    環境をセットアップする
-}

module Init where
import IO
import Control.Monad.Cont
import Control.Monad.State

import DataTypes
import Context
import qualified JSObject as Object
import qualified JSArray as Array
import Eval

setupEnv :: Evaluate ()
setupEnv =
    do defineVar "Object"
                 nullObject {
                     properties = [("prototype", Object.prototypeObject)],
                     attributes = [("prototype", [DontEnum, DontDelete, ReadOnly])],
                     delegate   = NativeFunction Object.function,
                     prototype  = Object.prototypeObject,
                     construct  = NativeFunction Object.make
                 }

       defineVar "Array"
                 nullObject {
                     properties = [("prototype", Array.prototypeObject)],
                     attributes = [("prototype", [DontEnum, DontDelete, ReadOnly])],
                     delegate   = NativeFunction Array.function,
                     prototype  = Array.prototypeObject,
                     construct  = NativeFunction Array.make
                 }

       defineVar "NaN" (Number NaN)
       defineVar "Infinity" (Number $ Double $ 1 / 0)

       defineVar "undefined" Undefined

       defineVar "print" (NativeFunction print')
       defineVar "__print__" (NativeFunction printNative)
       defineVar "__printEnv__" (NativeFunction printEnv)
       
       return ()

       where print' :: NativeFunction
             print' [] = print' [Undefined]
             print' (x:_) =
                 do string <- toString x
                    liftAll $ putStrLn string
                    return Undefined

             printNative :: NativeFunction
             printNative (x:_) =
                 do liftAll $ print x
                    liftAll $ hFlush stdout
                    return Undefined

             printEnv :: NativeFunction
             printEnv _ =
                do env <- get
                   liftAll $ print env
                   return Undefined

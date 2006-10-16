{-
    Init.hs
    環境をセットアップする
-}

module Init where
import IO
import Control.Monad.Cont
import Control.Monad.State
import Data.Map

import DataTypes
import Context
import qualified JSObject as Object
import qualified JSArray as Array
import Internal
import Eval

setupEnv :: Evaluate ()
setupEnv =
    do defineVar "Object"
                 nullObject {
                     objProperties = fromList [("prototype", Object.prototypeObject)],
                     objAttributes = fromList [("prototype", [DontEnum, DontDelete, ReadOnly])],
                     objDefault    = NativeFunction Object.function,
                     objPrototype  = Object.prototypeObject,
                     objConstruct  = NativeFunction Object.make
                 }

       defineVar "Array"
                 nullObject {
                     objProperties = fromList [("prototype", Array.prototypeObject)],
                     objAttributes = fromList [("prototype", [DontEnum, DontDelete, ReadOnly])],
                     objDefault    = NativeFunction Array.function,
                     objPrototype  = Array.prototypeObject,
                     objConstruct  = NativeFunction Array.make
                 }

       defineVar "NaN" (Number NaN)
       defineVar "Infinity" (Number $ Double $ 1 / 0)

       defineVar "undefined" Undefined

       defineVar "print" (NativeFunction print')
       defineVar "__print__" (NativeFunction printNative)
       defineVar "__env__" (NativeFunction printEnv)
       defineVar "exit" (NativeFunction exit)
       
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
                do env <- getEnv
                   liftAll $ print env
                   return Undefined

             exit :: NativeFunction
             exit _ =
                throw SysExit

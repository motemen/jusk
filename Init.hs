{-
    Init.hs
    環境をセットアップする
-}

module Init where
import IO
import Control.Monad.State
import Data.IORef

import DataTypes
import Context
import qualified JSObject as Object
import qualified JSArray as Array
import qualified JSString as String
import qualified JSFunction as Function
import Internal
import Eval

nullEnv :: [Flag] -> IO Env
nullEnv flags =
    do global <- liftM Ref $ newIORef $ nullObject { objClass = "Global", objPrototype = Object.prototypeObject }
       return $ Env { envFrames = [GlobalFrame global global], envContStack = [], envFlags = flags }

setupEnv :: Evaluate ()
setupEnv =
    do defineConstructor "Object"   Object.prototypeObject   Object.function   Object.constructor
       defineConstructor "Array"    Array.prototypeObject    Array.function    Array.constructor
       defineConstructor "String"   String.prototypeObject   String.function   String.constructor
       defineConstructor "Function" Function.prototypeObject Function.function Function.constructor

       defineVar "NaN" (Number NaN)
       defineVar "Infinity" (Number $ Double $ 1 / 0)

       defineVar "undefined" Undefined

       defineVar "print" (NativeFunction print')
       defineVar "p" (NativeFunction printNative)
       defineVar "__env__" (NativeFunction printEnv)
       defineVar "exit" (NativeFunction exit)
       
       return ()

       where defineConstructor name prototypeObject function construct =
                 do proto <- makeRef prototypeObject
                    constructor <- makeRef nullObject {
                        objPropMap   = mkPropMap [("prototype", proto, [DontEnum, DontDelete, ReadOnly])],
                        objPrototype = Function.prototypeObject,
                        objValue     = NativeFunction function,
                        objConstruct = NativeFunction construct,
                        objName      = name
                    }
                    constructor ! "prototype" ! "constructor" <~ constructor
                    defineVar name constructor

             print' :: NativeFunction
             print' [] = print' [Undefined]
             print' (x:_) =
                 do string <- toString x
                    liftAll $ putStrLn string
                    return Undefined

             printNative :: NativeFunction
             printNative (x:_) =
                 do liftAll $ print x
                    return Undefined

             printEnv :: NativeFunction
             printEnv _ =
                do env <- getEnv
                   liftAll $ print $ env { envFrames = tail $ envFrames env }
                   return Undefined

             exit :: NativeFunction
             exit _ =
                throw SysExit

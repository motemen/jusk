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
    do pushCont (\e -> liftAll $ print e >> return e) CThrow

       defineConstructor "Object"   Object.prototypeObject   Object.function   Object.constructor
       defineConstructor "Array"    Array.prototypeObject    Array.function    Array.constructor
       defineConstructor "String"   String.prototypeObject   String.function   String.constructor
       defineConstructor "Function" Function.prototypeObject Function.function Function.constructor

       defineVar "NaN" (Number NaN)
       defineVar "Infinity" (Number $ Double $ 1 / 0)

       defineVar "undefined" Undefined

       defineVar "print"     (nativeFunc "print"     1 print')
       defineVar "p"         (nativeFunc "p"         1 printNative)
       defineVar "__env__"   (nativeFunc "__env__"   0 printEnv)
       defineVar "__proto__" (nativeFunc "__proto__" 1 getProto)
       defineVar "exit"      (nativeFunc "exit"      0 exit)
       
       popCont

       return ()

       where defineConstructor name prototypeObject function construct =
                 do proto <- makeRef prototypeObject
                    constructor <- makeRef nullNativeFunc {
                        funcName      = name,
                        funcArity     = 1,
                        funcNatCode   = function,
                        funcConstruct = Just construct,
                        objPropMap    = mkPropMap [("prototype", proto, [DontEnum, DontDelete, ReadOnly])]
                    }
                    constructor ! "prototype" ! "constructor" <~ constructor
                    defineVar name constructor

             print' [] = print' [Undefined]
             print' (x:_) =
                 do string <- toString x
                    liftAll $ putStrLn string
                    return Undefined

             printNative (x:_) =
                 do liftAll $ print x
                    return Undefined

             printEnv _ =
                do env <- getEnv
                   liftAll $ print $ env { envFrames = tail $ envFrames env }
                   return Undefined

             getProto (Object { objPrototype = proto }:_) =
                 return proto

             getProto (ref@(Ref _):_) =
                 do obj <- readRef ref
                    getProto [obj]

             getProto _ =
                 return Null

             exit _ =
                throw SysExit

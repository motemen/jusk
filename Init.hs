{-
    Init.hs
    環境をセットアップする
-}

module Init where
import Prelude hiding(break)
import IO
import Control.Monad.State
import Data.IORef

import DataTypes
import Context
import qualified JSObject as Object
import qualified JSArray as Array
import qualified JSString as String
import qualified JSFunction as Function
import qualified JSDate as Date
import qualified JSRegExp as RegExp
import qualified JSError as Error
import Internal
import Eval
import Parser
import ParserUtil
import Repl

nullEnv :: [Flag] -> IO Env
nullEnv flags =
    do global <- liftM Ref $ newIORef $ nullObject { objClass = "Global", objPrototype = Object.prototypeObject }
       return $ Env { envFrames = [GlobalFrame global global], envContStack = [], envFlags = flags }

setupEnv :: Evaluate ()
setupEnv =
    do pushCont (\e -> liftIO $ print e >> return e) CThrow

       defineConstructor "Object"   Object.prototypeObject   Object.function   Object.constructor
       defineConstructor "Array"    Array.prototypeObject    Array.function    Array.constructor
       defineConstructor "String"   String.prototypeObject   String.function   String.constructor
       defineConstructor "Function" Function.prototypeObject Function.function Function.constructor
       defineConstructor "Date"     Date.prototypeObject     Date.function     Date.constructor
       defineConstructor "RegExp"   RegExp.prototypeObject   RegExp.function   RegExp.constructor
       defineConstructor "Error"    Error.prototypeObject    Error.function    Error.constructor

       defineVar "NaN" (Number NaN)
       defineVar "Infinity" (Number $ Double $ 1 / 0)

       defineVar "undefined" Undefined

       defineBuiltInFuncs

       popCont

       return ()

       where defineConstructor name prototypeObject function construct =
                 do proto <- makeRef
                             =<< if name == "Object"
                                    then return prototypeObject { objName = name ++ ".prototype" }
                                    else do objectProto <- prototypeOfVar "Object"
                                            return $ prototypeObject { objName = name ++ ".prototype", objPrototype = objectProto }
                    constructor <- makeRef nullObject {
                        objName      = name,
                        objPropMap   = mkPropMap [("prototype", proto, [DontEnum, DontDelete, ReadOnly])],
                        objConstruct = Just construct,
                        objObject    = nullNativeFunc {
                            funcArity     = 1,
                            funcNatCode   = function
                        }
                    }
                    constructor ! "prototype" ! "constructor" <~ constructor
                    defineVar name constructor

defineBuiltInFuncs =
    do defineVar "load"      (nativeFunc "load"      1 load)
       defineVar "print"     (nativeFunc "print"     1 printLn)
       defineVar "p"         (nativeFunc "p"         1 printNative)
       defineVar "__env__"   (nativeFunc "__env__"   0 env)
       defineVar "__break__" (nativeFunc "__break__" 0 break)
       defineVar "__proto__" (nativeFunc "__proto__" 1 getProto)
       defineVar "exit"      (nativeFunc "exit"      0 exit)
       
load args =
    do popFrame
       value <- liftM last $ mapM loadFile args
       pushNullScope
       return value
    where loadFile file =
              do file <- toString file
                 content <- liftIO $ readFile file
                 case runLex program content of
                      Left err -> throw "SyntaxError" $ showError content err
                      Right program -> liftM last $ mapM eval program

printLn (x:_) =
    do string <- toString x
       liftIO $ putStrLn string
       return Undefined

printNative (x:_) =
    do liftIO $ print x
       return Undefined

env _ =
    do env <- getEnv
       proto <- prototypeOfVar "Object"
       object <- makeRef $ nullObject { objPrototype = proto }
       (object ! "frames" <~) =<< makeRef =<< Array.makeArray =<< liftIO (mapM (setProto proto . frObject) $ tail $ envFrames env)
       (object ! "stack" <~) =<< makeRef =<< Array.makeArray (map (String . show) $ envContStack env)
       return object
    where setProto proto object@Object { } =
              return $ object { objPrototype = proto }
          setProto proto ref@(Ref objRef) =
              do modifyIORef objRef $ \object -> object { objPrototype = proto }
                 return ref

getProto (Object { objPrototype = proto }:_) =
    return proto

getProto (ref@(Ref _):_) =
    do obj <- readRef ref
       getProto [obj]

getProto _ =
    return Null

exit [] =
   returnCont CExit Void

exit (x:_) =
   returnCont CExit x

break _ =
    withCC (CContinue Nothing) (runReplWithTry >> return Void)

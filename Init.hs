{-
    Init.hs
    環境をセットアップする
-}

module Init where
import Prelude hiding(break)
import IO
import Control.Monad.State
import Data.IORef
import qualified Data.Map as Map
import Network.URI (escapeURIString, isUnreserved, unEscapeString)

import DataTypes
import Context
import qualified JSObject   as Object
import qualified JSArray    as Array
import qualified JSString   as String
import qualified JSFunction as Function
import qualified JSDate     as Date
import qualified JSRegExp   as RegExp
import qualified JSError    as Error
import JSMath
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

       forM ["EvalError", "RangeError", "ReferenceError", "SyntaxError", "TypeError"]
            $ \name -> defineConstructor name
                                         (Error.prototypeObjectOfName name)
                                         Error.function
                                         Error.constructor

       defineVar "Math" =<< createMathObject

       defineVar "NaN" (Number NaN)
       defineVar "Infinity" (Number $ Double $ 1 / 0)

       defineVar "undefined" Undefined

       defineBuiltInFuncs

       popCont

       return ()

       where defineConstructor name prototypeObject function construct =
                 do proto <- makeRef prototypeObject
                    constructor <- makeRef nullObject {
                            objName      = name,
                            objPropMap   = mkPropMap [("prototype", proto, [DontEnum, DontDelete, ReadOnly])],
                            objConstruct = Just construct,
                            objObject    = nullNativeFunc {
                                funcArity     = 1,
                                funcNatCode   = function
                            }
                        }
                    protoRef <- getValue $ constructor ! "prototype"
                    forM (Map.assocs $ objPropMap prototypeObject) $ \(key, prop) -> do
                        propRef <- makeRef $ propValue prop
                        putProp protoRef key (propRef, [DontEnum])
                    putProp protoRef "constructor" (constructor, [DontEnum])
                    defineVar name constructor

defineBuiltInFuncs =
    do defineVar "eval"      (nativeFunc "eval"      1 evalFunc)
       defineVar "load"      (nativeFunc "load"      1 load)
       defineVar "print"     (nativeFunc "print"     1 printLn)
       defineVar "p"         (nativeFunc "p"         1 printNative)
       defineVar "__p__"     (nativeFunc "__p__"     1 printNative)
       defineVar "__env__"   (nativeFunc "__env__"   0 env)
       defineVar "__break__" (nativeFunc "__break__" 0 break)
       defineVar "__proto__" (nativeFunc "__proto__" 1 getProto)
       defineVar "exit"      (nativeFunc "exit"      0 exit)
       defineVar "encodeURIComponent" (nativeFunc "encodeURIComponent" 1 encodeURIComponent)
       defineVar "decodeURIComponent" (nativeFunc "decodeURIComponent" 1 decodeURIComponent)
       
evalFunc _ [] =
    return Undefined

evalFunc _ (x:_) =
    do source <- readRef x
       case source of
            String source ->
                case runLex program source of
                     Left err -> throw "SyntaxError" $ showError source err
                     Right program ->
                         do result <- liftM last $ mapM eval program
                            if isVoid result
                               then return Undefined
                               else return result
            _ -> return x

load _ args =
    liftM last $ mapM loadFile args
    where loadFile file =
              do file <- toString file
                 content <- liftIO $ readFile file
                 case runLex program content of
                      Left err -> throw "SyntaxError" $ showError content err
                      Right program -> liftM last $ mapM eval program

printLn _ (x:_) =
    do string <- toString x
       liftIO $ putStrLn string
       return Undefined

printNative _ (x:_) =
    do liftIO $ print x
       return Undefined

env _ _ =
    do env <- getEnv
       object <- makeRef $ nullObject
       (object ! "frames" <~) =<< makeRef (Array.makeArray $ map frObject $ envFrames env)
       (object ! "stack" <~)  =<< makeRef (Array.makeArray $ map (String . show) (envContStack env))
       return object

getProto _ (Object { objPrototype = proto }:_) =
    return proto

getProto _ (ref@(Ref _):_) =
    do obj <- readRef ref
       getProto undefined [obj]

getProto _ _ =
    return Null

exit _ [] =
   returnCont CExit Void

exit _ (x:_) =
   returnCont CExit x

break _ _ =
    do liftIO $ putStrLn "*** break ***"
       withCC (CContinue Nothing) (runReplWithTry >> return Void)

encodeURIComponent _ [] =
    return $ String ""

encodeURIComponent _ (uri:_) =
    do uri <- toString uri
       return $ String $ escapeURIString isUnreserved uri

decodeURIComponent _ [] =
    return $ String ""

decodeURIComponent _ (uri:_) =
    do uri <- toString uri
       return $ String $ unEscapeString uri

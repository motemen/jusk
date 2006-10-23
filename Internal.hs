{-
    Internal.hs
    内部プロパティとメソッド
-}

module Internal (
        throw,
        prototypeOf, classOf,
        getProp, putProp, canPut,
        hasProperty,
        getValue, putValue,
        getVar, setVar,
        isBound,
        defineVar,
        prototypeOfVar,
        withNoRef, withNoRef2,
        (!), (<~)
    ) where
import qualified Data.Map as Map
import Data.IORef
import Control.Monad.State
import Maybe

import DataTypes
import Context
import ParserUtil (natural,runLex)

-- http://www2u.biglobe.ne.jp/~oz-07ams/prog/ecma262r3/8_Types.html#section-8.6

-- [[Prototype]]
prototypeOf :: Value -> Evaluate Value
prototypeOf ref@Ref { } =
    prototypeOf =<< readRef ref

prototypeOf object@Object { } =
    return $ objPrototype object

prototypeOf _ =
    return Null

-- [[Class]]
classOf :: Value -> Evaluate String
classOf ref@Ref { } =
    classOf =<< readRef ref

classOf object@Object { } =
    return  $ objClass object

classOf _ =
    return "null"

-- プロパティの取得/設定
-- [[Get]]
getProp :: Value -> String -> Evaluate Value
getProp object@Object { objValue = value } p | not $ isNull value =
    getOwnProp object p
    >>= maybe (prototypeOf object >>= maybeNull (getProp value p)
                                                (flip getProp p))
              (lift . return)

getProp ref@Reference { } p =
    flip getProp p =<< getValue ref
    
getProp ref@Ref { } p =
    flip getProp p =<< readRef ref

getProp object p =
    getOwnProp object p
    >>= maybe (prototypeOf object >>= maybeNull (lift $ return Undefined)
                                                (flip getProp p))
              (lift . return)

-- [[Put]]
putProp :: Value -> String -> Value -> Evaluate ()
putProp ref@(Ref objRef) p value =
    do object <- readRef ref
       canPut <- canPut object p
       value <- makeRef value
       when (canPut)
            (liftIO $ modifyIORef objRef $ insertProp value)
    where insertProp value object@Object { objPropMap = propMap }
              = object { objPropMap = Map.insert p (mkProp value []) propMap }
          insertProp value object
              = nullObject { objPropMap = mkPropMap [(p, value, [])], objValue = object }

putProp object _ _ =
    do throw "ReferenceError" $ "cannot put property to " ++ show object
       return ()

-- [[CanPut]]
canPut :: Value -> String -> Evaluate Bool
canPut object p =
    getOwnPropAttr object p 
    >>= maybe (prototypeOf object >>= maybeNull (lift $ return True)
                                                (flip canPut p))
              (lift . return . not . elem ReadOnly)

-- [[HasProperty]]
hasProperty :: Value -> String -> Evaluate Bool
hasProperty object@Object { objPropMap = props } p =
    maybe (prototypeOf object >>= maybeNull (lift $ return False)
                                            (flip hasProperty p))
          (const $ lift $ return True)
          (Map.lookup p props)

hasProperty ref@Ref { } p =
    flip hasProperty p =<< readRef ref

hasProperty o p =
    do throw "NotImplemented" $ "hasProperty: " ++ show o ++ " " ++ show p
       return False

hasOwnProperty :: Value -> String -> Evaluate Bool
hasOwnProperty Object { objPropMap = props } p =
    return $ Map.member p props

hasOwnProperty ref@Ref { } p =
    flip hasOwnProperty p =<< readRef ref

hasOwnProperty o p =
    do throw "NotImplemented" $ "hasOwnProperty: " ++ show o ++ " " ++ show p
       return False

-- Reference の解決
getValue :: Value -> Evaluate Value
getValue (Reference baseRef name) =
    do base <- readRef baseRef
       if isVoid base
          then do var <- makeRef =<< getVar name
                  liftIO $ modifyIORef (getRef var) $ setObjName name
                  getValue var
          else do baseName <- liftM getName $ readRef base
                  value <- makeRef =<< getProp base name
                  liftIO $ modifyIORef (getRef value) $ setObjName $ baseName ++ "." ++ name
                  getValue value

getValue value =
    return value

putValue :: Value -> Value -> Evaluate ()
putValue (Reference baseRef name) value =
    do baseRef <- getValue baseRef
       putProp baseRef name value

putValue (Ref ref) value =
    liftIO $ writeIORef ref value

putValue _ _ =
    do throw "ReferenceError" "invalid assignment left-hand side"
       return ()

getVar :: String -> Evaluate Value
getVar name =
    do env <- getEnv
       getFrameVar (envFrames env) name

setVar :: String -> Value -> Evaluate Value
setVar name value =
    do var <- getVar name
       var <~ value
       return value

isBound :: String -> Evaluate Bool
isBound name =
    do env <- getEnv
       isFrameVarBound (envFrames env) name

defineVar :: String -> Value -> Evaluate Value
defineVar name value =
    do frame <- currentFrame
       case frame of
            WithFrame { } -> do popFrame
                                defineVar name value
                                modify $ \env@Env { envFrames = frames } -> env { envFrames = frame:frames }
                                return value
            _ -> do value <- makeRef value
                    frameObject <- liftM frObject currentFrame
                    frameObject ! name <~ value
                    return value

getFrameVar :: [Frame] -> String -> Evaluate Value
getFrameVar [] name =
    throw "ReferenceError" $ name ++ " is not defined"

getFrameVar (f:fs) name =
    getOwnProp (frObject f) name
    >>= maybe (getFrameVar fs name)
              (return)

isFrameVarBound :: [Frame] -> String -> Evaluate Bool
isFrameVarBound [] _ =
    return False

isFrameVarBound (f:fs) name =
    do bound <- hasOwnProperty (frObject f) name
       if bound
          then return True
          else isFrameVarBound fs name

getOwnProp :: Value -> String -> Evaluate (Maybe Value)
getOwnProp (Ref objRef) p =
    do object <- liftIO $ readIORef objRef
       getOwnProp object p

getOwnProp (String string) "length" =
    return $ Just $ toValue $ length string

getOwnProp (String _) p =
    do proto <- prototypeOfVar "String"
       getOwnProp proto p

-- http://www2u.biglobe.ne.jp/~oz-07ams/prog/ecma262r3/15-4_Array_Objects.html#section-G
getOwnProp (Object { objObject = Array array }) "length" =
    return $ Just $ toValue $ length array

getOwnProp (Object { objObject = Array array }) p =
    case (runLex natural p) of
         Left _  -> do prototype <- prototypeOfVar "Array"
                       getOwnProp prototype p
         Right n -> return $ Just $ array !! (fromInteger n)

getOwnProp object@Object { } p =
    return $ liftM propValue $ Map.lookup p (objPropMap object)

getOwnProp o p =
    do warn $ "Not implemented: getOwnProp: " ++ show o ++ " " ++ p
       return Nothing

getOwnPropAttr :: Value -> String -> Evaluate (Maybe [PropertyAttribute])
getOwnPropAttr (Ref objRef) p =
    do object <- liftIO $ readIORef objRef
       getOwnPropAttr object p

getOwnPropAttr (Object { objObject = Array _ }) "length" =
    return $ Just $ [DontEnum, DontDelete]

getOwnPropAttr (Object { objObject = Array _ }) p =
    case (runLex natural p) of
         Left _  -> do a <- prototypeOfVar "Array"
                       getOwnPropAttr a p
         Right _ -> return $ Just $ []

getOwnPropAttr object@Object { } p =
    return $ liftM propAttr $ Map.lookup p (objPropMap object)

getOwnPropAttr _ _ =
    return Nothing

maybeNull :: a -> (Value -> a) -> Value -> a
maybeNull x _ Null = x
maybeNull _ r v = r v

prototypeOfVar :: String -> Evaluate Value
prototypeOfVar varName =
    do var <- getVar varName
       getProp var "prototype"

(!) :: Value -> String -> Value
(!) = Reference

(<~) :: Value -> Value -> Evaluate ()
(<~) = putValue

withNoRef :: (Value -> Evaluate a) -> Value -> Evaluate a
withNoRef f x =
    f =<< readRef =<< getValue x

withNoRef2 :: (Value -> Value -> Evaluate a) -> Value -> Value -> Evaluate a
withNoRef2 f x y =
    uncurry f =<< liftM2 (,) (readRef =<< getValue x) (readRef =<< getValue y)

throw :: String -> String -> Evaluate Value
throw name message =
    do proto <- flip getProp "prototype" =<< getVar "Error"
       let error = nullObject {
           objClass     = "Error",
           objPropMap   = mkPropMap [("name", String name, []), ("message", String message, [])],
           objPrototype = proto
       }
       returnCont CThrow error


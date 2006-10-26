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
        getVarFrameObject,
        isBound,
        defineVar,
        prototypeOfVar,
        withNoRef, withNoRef2,
        modifyValue,
        makeNullObject,
        ifM,
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
    return $ objClass object

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
putProp :: Value -> String -> (Value, [PropertyAttribute]) -> Evaluate ()
putProp ref@(Ref _) p pair =
    do object <- readRef ref
       ifM (canPut object p)
           (modifyValue ref $ insertProp p pair)
           (return ())
putProp Void name (value, _) =
    setVar name value >> return ()

putProp object p (value, _) =
    do throw "ReferenceError" $ "cannot put property " ++ getName object ++ "." ++ p ++ " " ++ show value
       return ()

insertProp :: String -> (Value, [PropertyAttribute]) -> Value -> Value
insertProp p (value, attr) object@Object { objObject = Array array } =
    case (runLex natural p) of
         Right n | 0 <= (fromInteger n)
            -> object { objObject = Array $ replace array (fromInteger n) value }
         _ -> object { objPropMap = Map.insert p (mkProp value attr) (objPropMap object) }
    where replace list n x =
              if length list < n
                 then take n (list ++ repeat Undefined) ++ [x]
                 else take n list ++ [x] ++ drop (n+1) list

insertProp p (value, attr) object@Object { objPropMap = propMap } =
    object { objPropMap = Map.insert p (mkProp value attr) propMap }

insertProp p (value, attr) object =
    nullObject { objPropMap = mkPropMap [(p, value, attr)], objValue = object }

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

hasProperty ref@Reference { } p =
    flip hasProperty p =<< getValue ref

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
       klass <- classOf base
       when (klass == "Global" || klass == "Activation")
            (ifM (liftM not $ hasOwnProperty base name)
                 ((throw "ReferenceError" $ name ++ " is not defined") >> return ())
                 (return ()))
       baseName <- liftM getName $ readRef base
       value <- getProp base name
       when (isRef value)
            (modifyValue value $ setObjName $ baseName +++ name)
       getValue value
    where "" +++ name = name
          baseName +++ name = baseName ++ "." ++ name

getValue value =
    return value

putValue :: Value -> Value -> Evaluate ()
putValue (Reference baseRef name) value =
    do baseRef <- getValue baseRef
       value <- ifM (liftM isPrimitive $ readRef value)
                    (readRef value)
                    (makeRef value)
       putProp baseRef name (value, [])

putValue (Ref ref) value =
    liftIO $ writeIORef ref value

putValue o v =
    do throw "ReferenceError" $ "invalid assignment left-hand side" ++ " " ++ show o ++ " " ++ show v
       return ()

getVar :: String -> Evaluate Value
getVar name =
    do env <- getEnv
       getFrameVar (envFrames env) name

setVar :: String -> Value -> Evaluate Value
setVar name value =
    do frame <- liftM2 fromMaybe (liftM frObject currentFrame) (getVarFrameObject name)
       frame ! name <~ value
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
            _ -> do frameObject <- liftM frObject currentFrame
                    frameObject ! name <~ value
                    return value

getFrameVar :: [Frame] -> String -> Evaluate Value
getFrameVar [] name =
    throw "ReferenceError" $ name ++ " is not defined"

getFrameVar (f:fs) name =
    getOwnProp (frObject f) name
    >>= maybe (getFrameVar fs name)
              (return)

-- 特定の変数が定義されているフレームを取得
getVarFrameObject :: String -> Evaluate (Maybe Value)
getVarFrameObject name =
    do frames <- liftM envFrames getEnv
       getVarFrameObject' frames name
    where getVarFrameObject' [] _ =
              return Nothing
          getVarFrameObject' (f:fs) name =
              ifM (hasOwnProperty (frObject f) name)
                  (return $ Just $ frObject f)
                  (getVarFrameObject' fs name)

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

getOwnProp (String string) p =
    case (runLex natural p) of
         Right n | 0 <= (fromInteger n) && (fromInteger n) < length string
            -> return $ Just $ String [string !! (fromInteger n)]
         _  -> do prototype <- prototypeOfVar "String"
                  getOwnProp prototype p

-- http://www2u.biglobe.ne.jp/~oz-07ams/prog/ecma262r3/15-4_Array_Objects.html#section-G
getOwnProp (Object { objObject = Array array }) "length" =
    return $ Just $ toValue $ length array

getOwnProp (Object { objObject = Array array }) p =
    case (runLex natural p) of
         Right n | 0 <= (fromInteger n) && (fromInteger n) < length array
            -> return $ Just $ array !! (fromInteger n)
         _  -> do prototype <- prototypeOfVar "Array"
                  getOwnProp prototype p

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

makeNullObject :: Evaluate Value
makeNullObject =
    do proto <- prototypeOfVar "Object"
       makeRef nullObject { objPrototype = proto }

modifyValue :: Value -> (Value -> Value) -> Evaluate ()
modifyValue (Ref objRef) f =
    liftIO $ modifyIORef objRef f

modifyValue ref@Reference { } f =
    readRef ref >>= flip modifyValue f

modifyValue object _ =
    do throw "ReferenceError" $ getName object ++ " cannot be modified"
       return ()

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM mc mt me =
    do cond <- mc
       if cond
          then mt
          else me


{-
    Internal.hs
    内部プロパティとメソッド
-}

module Internal (
        prototypeOf,
        classOf,
        getProp, putProp, canPut,
        hasProperty,
        getValue, putValue,
        getVar, setVar,
        isBound,
        defineVar,
        warn
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
prototypeOf (Array _) =
    "prototype" `ofVar` "Array" 

prototypeOf ref@(Ref _) =
    prototypeOf =<< readRef ref

prototypeOf object@(Object { }) =
    return $ objPrototype object

prototypeOf _ =
    return Null

-- [[Class]]
classOf :: Value -> Evaluate Value
classOf ref@(Ref _) =
    classOf =<< readRef ref

classOf object@(Object { }) =
    return $ toValue $ objClass object

classOf _ =
    return Null

-- プロパティの取得/設定
-- [[Get]]
getProp :: Value -> String -> Evaluate Value
getProp object p =
    getOwnProp object p
    >>= maybe (prototypeOf object >>= maybeNull (lift $ return Undefined)
                                                (flip getProp p))
              (lift . return)

-- [[Put]]
putProp :: IORef Value -> String -> Value -> Evaluate ()
putProp objRef p valueRef@(Ref _) =
    do object <- liftAll $ readIORef objRef
       putProp' object
    where
        putProp' (Ref objRef) =
            putProp objRef p valueRef
        putProp' object =
            do canPut <- canPut object p
               when (canPut)
                    (liftAll
                     $ modifyIORef
                           objRef
                           (\object@Object { objPropMap = props }
                                -> object {
                                       objPropMap = Map.insert p (mkProp valueRef []) props
                                   }))

putProp objRef p value =
    do valueRef <- liftAll $ liftM Ref $ newIORef value
       putProp objRef p valueRef

-- [[CanPut]]
canPut :: Value -> String -> Evaluate Bool
canPut object p =
    getOwnPropAttr object p 
    >>= maybe (prototypeOf object >>= maybeNull (lift $ return True)
                                                (flip canPut p))
              (lift . return . not . elem ReadOnly)

-- [[HasProperty]]
hasProperty :: Value -> String -> Evaluate Bool
hasProperty object@(Object { objPropMap = props }) p =
    maybe (prototypeOf object >>= maybeNull (lift $ return False)
                                            (flip hasProperty p))
          (const $ lift $ return True)
          (Map.lookup p props)

-- Reference の解決
getValue :: Value -> Evaluate Value
getValue (Reference (baseRef, name)) =
    do base <- liftAll $ readIORef baseRef
       if isNull base
          then throw $ ReferenceError "null has no properties"
          else getProp base name >>= getValue                

getValue value =
    return value

putValue :: Value -> Value -> Evaluate Value
putValue (Reference (baseRef, name)) value =
    do putProp baseRef name value
       return value

putValue _ _ =
    throw $ ReferenceError "invalid assignment left-hand side"

getVar :: String -> Evaluate Value
getVar name =
    do env <- getEnv
       getFrameVar (envFrames env) name

setVar :: String -> Value -> Evaluate Value
setVar name value =
    do env <- getEnv
       setFrameVar (envFrames env) name value

isBound :: String -> Evaluate Bool
isBound name =
    do frame <- currentFrame
       object <- readRef $ frObject frame
       hasProperty object name

defineVar :: String -> Value -> Evaluate Value
defineVar name value =
    do frame <- currentFrame
       case frame of
            WithFrame { } -> do popFrame
                                defineVar name value
                                modify (\env@Env { envFrames = frames } -> env { envFrames = frame:frames })
                                return value
            _ -> do defined <- isBound name
                    if defined 
                       then setVar name value >> return value
                       else do objRef <- liftM frObject currentFrame
                               valueRef <- makeRef value
                               putProp (getRef objRef) name valueRef
                               return value

getFrameVar :: [Frame] -> String -> Evaluate Value
getFrameVar [] name =
    throw $ ReferenceError $ name ++ " is not defined"

getFrameVar (f:fs) name =
    do getOwnProp (frObject f) name
       >>= maybe (getFrameVar fs name)
                 (return)

setFrameVar :: [Frame] -> String -> Value -> Evaluate Value
setFrameVar (f:_) name value =
    do putProp (getRef $ frObject f) name value
       return value

warn :: String -> Evaluate ()
warn message =
    do env <- getEnv
       if Warn `elem` (envFlags env)
          then liftAll $ putStrLn $ "warning: " ++ message
          else return ()

getOwnProp :: Value -> String -> Evaluate (Maybe Value)
getOwnProp object@(Object { }) p =
    return $ liftM propValue $ Map.lookup p (objPropMap object)

getOwnProp (Ref objRef) p =
    do object <- liftAll $ readIORef objRef
       getOwnProp object p

getOwnProp (Function { }) "prototype" =
    do object <- getVar "Object"
       getOwnProp object "prototype"

-- http://www2u.biglobe.ne.jp/~oz-07ams/prog/ecma262r3/15-4_Array_Objects.html#section-G
getOwnProp (Array array) "length" =
    return $ Just $ toValue $ length array

getOwnProp (Array array) p =
    case (runLex natural p) of
         Left _  -> do prototype <- "prototype" `ofVar` "Array"
                       getOwnProp prototype p
         Right n -> return $ Just $ array !! (fromInteger n)

getOwnProp (String string) "length" =
    return $ Just $ toValue $ length string

getOwnProp (String _) p =
    do prototype <- "prototype" `ofVar` "String"
       getOwnProp prototype p

getOwnProp o p =
    do throw $ NotImplemented $ "getOwnProp: " ++ show o ++ " " ++ p
       return Nothing

getOwnPropAttr :: Value -> String -> Evaluate (Maybe [PropertyAttribute])
getOwnPropAttr object@(Object { }) p =
    return $ liftM propAttr $ Map.lookup p (objPropMap object)

getOwnPropAttr (Array _) "length" =
    return $ Just $ [DontEnum, DontDelete]

getOwnPropAttr (Array _) p =
    case (runLex natural p) of
         Left _  -> do a <- getVar "Array" >>= flip getProp "prototype"
                       getOwnPropAttr a p
         Right _ -> return $ Just $ []

getOwnPropAttr (Ref objRef) p =
    do object <- liftAll $ readIORef objRef
       getOwnPropAttr object p

maybeNull :: a -> (Value -> a) -> Value -> a
maybeNull x _ Null = x
maybeNull _ r v = r v

ofVar :: String -> String -> Evaluate Value
ofVar prop varName =
    do var <- getVar varName
       getProp var prop

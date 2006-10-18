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

prototypeOf (Ref objRef) =
    do object <- liftAll $ readIORef objRef
       prototypeOf object

prototypeOf obj =
    return $ case obj of
                  Object { } -> objPrototype obj
                  _ -> Null

-- [[Class]]
classOf :: Value -> Evaluate Value
classOf obj =
    return $ case obj of
                  Object { } -> String $ objClass obj
                  _ -> Null

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
putProp objRef p ref@(Ref _) =
    do object <- liftAll $ readIORef objRef
       putProp' object
    where
        putProp' (Ref objRef) =
            putProp objRef p ref
        putProp' object =
            do canPut <- canPut object p
               when (canPut)
                    (liftAll
                     $ modifyIORef
                           objRef
                           (\object@Object { objPropMap = props }
                                -> object {
                                       objPropMap = Map.insert p (mkProp ref []) props
                                   }))

putProp objRef p value =
    do ref <- liftAll $ liftM Ref $ newIORef value
       putProp objRef p ref

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
-- TODO: throw ReferenceError
getValue :: Value -> Evaluate Value
getValue (Reference (baseRef, name)) =
    do base <- lift $ liftIO $ readIORef baseRef
       getProp base name >>= getValue

getValue value =
    return value

putValue :: Value -> Value -> Evaluate Value
putValue (Reference (baseRef, name)) value =
    do putProp baseRef name value
       return value

putValue _ _ =
    throw InvalidAssignmentLeftSide

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
       case frame of
            WithFrame objRef -> do object <- liftAll $ readIORef objRef
                                   hasProperty object name
            _ -> do binding <- liftAll $ readIORef $ frBinding frame
                    return $ maybe False (const True) (lookup name binding)

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
                       else defineVar' name value
    where defineVar' :: String -> Value -> Evaluate Value
          defineVar' name value =
              case value of
                   Ref ref -> do value <- liftAll $ readIORef ref
                                 defineVar' name value
                   _ -> do bRef <- liftM frBinding currentFrame
                           binding <- liftAll $ readIORef bRef
                           valueRef <- makeRef value
                           liftAll $ writeIORef bRef ((name, valueRef):binding)
                           return value

getFrameVar :: [Frame] -> String -> Evaluate Value
getFrameVar [] name =
    throw $ NotDefined name

getFrameVar ((WithFrame objRef):fs) name =
    do object <- liftAll $ readIORef objRef
       getFrameVar' object fs name
    where getFrameVar' :: Value -> [Frame] -> String -> Evaluate Value
          getFrameVar' object fs name =
              do value <- getOwnProp object name
                 maybe (do proto <- prototypeOf object
                           if isNull proto
                              then getFrameVar fs name
                              else getFrameVar' proto fs name)
                       (return)
                       (value)
              

getFrameVar (f:fs) name =
    do binding <- liftAll $ readIORef $ frBinding f
       maybe (getFrameVar fs name)
             (return)
             (lookup name binding)

setFrameVar :: [Frame] -> String -> Value -> Evaluate Value
setFrameVar ((WithFrame objRef):_) name value =
    do putProp objRef name value
       return value

setFrameVar (f:_) name value =
    do binding <- liftAll $ readIORef $ frBinding f
       maybe (warn $ "assignment to undeclared variable " ++ name)
             (liftIO . (flip writeIORef value) . getRef)
             (lookup name binding)
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
getOwnProp (Array array) p
    | p == "length" = return $ Just $ Number $ Integer $ toEnum $ length array
    | otherwise =
        case (runLex natural p) of
             Left _  -> do a <- "prototype" `ofVar` "Array"
                           getOwnProp a p
             Right n -> return $ Just $ array !! (fromInteger n)

getOwnProp o p =
    do throw $ NotImplemented $ "getOwnProp: " ++ show o ++ " " ++ p
       return Nothing

getOwnPropAttr :: Value -> String -> Evaluate (Maybe [PropertyAttribute])
getOwnPropAttr object@(Object { }) p =
    return $ liftM propAttr $ Map.lookup p (objPropMap object)

getOwnPropAttr (Array _) p
    | p == "length" = return $ Just $ [DontEnum, DontDelete]
    | otherwise =
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

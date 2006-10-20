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
        warn,
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
prototypeOf (Array _) =
    prototypeOfVar "Array" 

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
getProp (object@Object { objValue = value }) p | not $ isNull value =
    getOwnProp object p
    >>= maybe (prototypeOf object >>= maybeNull (getProp value p)
                                                (flip getProp p))
              (lift . return)

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
            (liftAll $ modifyIORef
                       objRef
                       (\object@Object { } -> object { objPropMap = Map.insert p (mkProp value []) (objPropMap object) }))

putProp object _ _ =
    do throw $ ReferenceError $ "cannot put property to " ++ show object
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
hasProperty object@(Object { objPropMap = props }) p =
    maybe (prototypeOf object >>= maybeNull (lift $ return False)
                                            (flip hasProperty p))
          (const $ lift $ return True)
          (Map.lookup p props)

hasProperty ref@(Ref { }) p =
    flip hasProperty p =<< readRef ref

hasProperty o p =
    do throw $ NotImplemented $ "hasProperty: " ++ show o ++ " " ++ show p
       return False

hasOwnProperty :: Value -> String -> Evaluate Bool
hasOwnProperty (Object { objPropMap = props }) p =
    return $ Map.member p props

hasOwnProperty ref@(Ref { }) p =
    flip hasOwnProperty p =<< readRef ref

hasOwnProperty o p =
    do throw $ NotImplemented $ "hasOwnProperty: " ++ show o ++ " " ++ show p
       return False

-- Reference の解決
getValue :: Value -> Evaluate Value
getValue (Reference baseRef name) =
    do base <- readRef baseRef
       if isNull base
          then throw $ ReferenceError "null has no properties"
          else getProp base name >>= getValue                

getValue value =
    return value

putValue :: Value -> Value -> Evaluate ()
putValue (Reference baseRef name) value =
    do baseRef <- getValue baseRef
       putProp baseRef name value

putValue (Ref ref) value =
    liftAll $ writeIORef ref value

putValue _ _ =
    do throw $ ReferenceError "invalid assignment left-hand side"
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
                                modify (\env@Env { envFrames = frames } -> env { envFrames = frame:frames })
                                return value
            _ -> do value <- makeRef value
                    frameObject <- liftM frObject currentFrame
                    frameObject ! name <~ value
                    return value

getFrameVar :: [Frame] -> String -> Evaluate Value
getFrameVar [] name =
    throw $ ReferenceError $ name ++ " is not defined"

getFrameVar (f:fs) name =
    do getOwnProp (frObject f) name
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
    liftM return $ prototypeOfVar "Object"

getOwnProp (Function { }) p =
    do proto <- prototypeOfVar "Function"
       getOwnProp proto p

-- http://www2u.biglobe.ne.jp/~oz-07ams/prog/ecma262r3/15-4_Array_Objects.html#section-G
getOwnProp (Array array) "length" =
    return $ Just $ toValue $ length array

getOwnProp (Array array) p =
    case (runLex natural p) of
         Left _  -> do prototype <- prototypeOfVar "Array"
                       getOwnProp prototype p
         Right n -> return $ Just $ array !! (fromInteger n)

getOwnProp (String string) "length" =
    return $ Just $ toValue $ length string

getOwnProp (String _) p =
    do proto <- prototypeOfVar "String"
       getOwnProp proto p

getOwnProp o p =
    do warn $ show $ NotImplemented $ "getOwnProp: " ++ show o ++ " " ++ p
       return Nothing

getOwnPropAttr :: Value -> String -> Evaluate (Maybe [PropertyAttribute])
getOwnPropAttr object@(Object { }) p =
    return $ liftM propAttr $ Map.lookup p (objPropMap object)

getOwnPropAttr (Array _) "length" =
    return $ Just $ [DontEnum, DontDelete]

getOwnPropAttr (Array _) p =
    case (runLex natural p) of
         Left _  -> do a <- prototypeOfVar "Array"
                       getOwnPropAttr a p
         Right _ -> return $ Just $ []

getOwnPropAttr (Ref objRef) p =
    do object <- liftAll $ readIORef objRef
       getOwnPropAttr object p

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

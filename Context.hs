{-
    Context.hs
    実行コンテキスト
    http://www2u.biglobe.ne.jp/~oz-07ams/prog/ecma262r3/10_Execution_Contexts.html
-}

module Context where
import Data.IORef
import Control.Monad.State
import Control.Monad.Cont hiding(Cont)

import DataTypes

nullEnv :: IO Binding
nullEnv = newIORef []

nullFrame :: IO Env
nullFrame = do e <- nullEnv
               return $ Env { envFrames = [GlobalFrame e Null], envContStack = [] }

pushFrame :: Value -> Binding -> Evaluate ()
pushFrame this binding =
    modify (\env@Env { envFrames = frames } -> env { envFrames = (Activation binding this):frames })

pushNullFrame :: Value -> Evaluate ()
pushNullFrame this =
    do binding <- liftAll $ newIORef []
       pushFrame this binding

popFrame :: Evaluate ()
popFrame =
    modify (\env@Env { envFrames = frames } -> env { envFrames = tail frames })

pushScope :: Binding -> Evaluate ()
pushScope binding =
    do this <- getThis
       pushFrame this binding

pushNullScope :: Evaluate ()
pushNullScope =
    do this <- getThis
       pushNullFrame this

popScope :: Evaluate ()
popScope = popFrame

pushCont :: (Value -> Evaluate Value) -> ContType -> Evaluate ()
pushCont c ct =
    modify (\env@Env { envContStack = cs } -> env { envContStack = (ct, c):cs })

popCont :: Evaluate Cont
popCont =
    do env <- get
       let cs = envContStack env
       put $ env { envContStack = tail cs }
       return $ head cs

withCC :: ContType -> Evaluate Value -> Evaluate Value
withCC ct proc =
    do callCC $ \cc -> do pushCont cc ct
                          value <- proc
                          popCont
                          return value

returnCont :: ContType -> Value -> Evaluate Value
returnCont ct value =
    do (contType, cont) <- popCont
       if contType == ct
          then cont value
          else returnCont ct value

throw :: Exception -> Evaluate Value
throw e =
    returnCont CThrow (Exception e)

isBound :: String -> Evaluate Bool
isBound name =
    do bRef <- getBinding
       binding <- liftAll $ readIORef bRef
       return $ maybe False (const True) (lookup name binding)

getVar :: String -> Evaluate Value
getVar name =
    do bRefs <- getBindings
       getVar' bRefs name
    where getVar' :: [Binding] -> String -> Evaluate Value
          getVar' [] name =
              throw $ NotDefined name
          getVar' (b:bs) name =
              do binding <- liftAll $ readIORef b
                 maybe (getVar' bs name)
                       (liftAll . readIORef)
                       (lookup name binding)

getVarRef :: String -> Evaluate (IORef Value)
getVarRef name =
    do bRefs <- getBindings
       getVarRef' bRefs name
    where getVarRef' :: [Binding] -> String -> Evaluate (IORef Value)
          getVarRef' [] name =
              (throw $ NotDefined name) >>= liftAll . newIORef
          getVarRef' (b:bs) name =
              do binding <- liftAll $ readIORef b
                 maybe (getVarRef' bs name)
                       (return)
                       (lookup name binding)

setVar :: String -> Value -> Evaluate Value
setVar name value =
    do bRef <- getBinding
       binding <- liftAll $ readIORef bRef
       maybe (return ()) -- TODO: warn
             (liftIO . (flip writeIORef value))
             (lookup name binding)
       return value

defineVar :: String -> Value -> Evaluate Value
defineVar name value =
    do alreadyDefined <- isBound name
       if alreadyDefined 
          then setVar name value >> return value
          else case value of
                    Ref valueRef -> defineVar' name valueRef
                    _ -> do valueRef <- liftAll $ newIORef value
                            defineVar' name valueRef
    where defineVar' :: String -> IORef Value -> Evaluate Value
          defineVar' name valueRef =
              do value <- liftAll $ readIORef valueRef
                 case value of
                      Ref valueRef' -> defineVar' name valueRef'
                      _ -> do bRef <- getBinding
                              binding <- liftAll $ readIORef bRef
                              liftAll $ writeIORef bRef ((name, valueRef):binding)
                              return value

getThis :: Evaluate Value
getThis = do env <- get
             return $ frThis $ head $ envFrames env

getBinding :: Evaluate Binding
getBinding = do env <- get
                return $ frBinding $ head $ envFrames env

getBindings :: Evaluate [Binding]
getBindings = do env <- get
                 return $ map frBinding $ envFrames env

bindParamArgs :: [Parameter] -> [Value] -> Evaluate Binding
bindParamArgs params args =
    do binding <- liftAll $ mapM zipArg $ zip params args
       bRef <- liftAll $ newIORef binding
       return bRef
    where zipArg :: (Parameter, Value) -> IO (String, IORef Value)
          zipArg (param, arg) = 
              do argRef <- newIORef arg
                 return (varName param, argRef)

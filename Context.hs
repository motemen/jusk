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

nullEnv :: [Flag] -> IO Env
nullEnv flags =
    do binding <- newIORef []
       return $ Env { envFrames = [GlobalFrame binding Null], envContStack = [], envFlags = flags }

getEnv :: Evaluate Env
getEnv = get

-- Frame
pushFrame :: Value -> Binding -> Evaluate ()
pushFrame this binding =
    modify (\env@Env { envFrames = frames } -> env { envFrames = (Activation binding this):frames })

pushWithFrame :: IORef Value -> Evaluate ()
pushWithFrame objRef =
    modify (\env@Env { envFrames = frames } -> env { envFrames = (WithFrame objRef):frames })

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

currentFrame :: Evaluate Frame
currentFrame = liftM (head . envFrames) getEnv

-- Continuation
pushCont :: (Value -> Evaluate Value) -> ContType -> Evaluate ()
pushCont c ct =
    modify (\env@Env { envContStack = cs } -> env { envContStack = (ct, c):cs })

popCont :: Evaluate Cont
popCont =
    do env <- getEnv
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

-- Exception
throw :: Exception -> Evaluate Value
throw e =
    returnCont CThrow (Exception e)

getThis :: Evaluate Value
getThis = do env <- getEnv
             return $ frThis $ head $ envFrames env

bindParamArgs :: [Parameter] -> [Value] -> Evaluate Binding
bindParamArgs params args =
    do binding <- mapM zipArg $ zip params args
       bRef <- liftAll $ newIORef binding
       return bRef
    where zipArg :: (Parameter, Value) -> Evaluate (String, Value)
          zipArg (param, arg) = 
              do argRef <- makeRef arg
                 return (param, argRef)

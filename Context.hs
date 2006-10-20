{-
    Context.hs
    実行コンテキスト
    http://www2u.biglobe.ne.jp/~oz-07ams/prog/ecma262r3/10_Execution_Contexts.html
-}

module Context where
import Control.Monad.State
import Control.Monad.Cont hiding(Cont)

import DataTypes

getEnv :: Evaluate Env
getEnv = get

-- Frame
pushFrame :: Value -> Binding -> Evaluate ()
pushFrame this binding =
    modify $ \env@Env { envFrames = frames } -> env { envFrames = [Activation binding this] ++ frames }

pushWithFrame :: Value -> Evaluate ()
pushWithFrame object =
    modify $ \env@Env { envFrames = frames } -> env { envFrames = [WithFrame object] ++ frames }

pushNullFrame :: Value -> Evaluate ()
pushNullFrame this =
    do binding <- makeRef $ nullObject { objClass = "Activation" }
       pushFrame this binding

popFrame :: Evaluate ()
popFrame =
    modify $ \env@Env { envFrames = frames } -> env { envFrames = tail frames }

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

withScope :: [Frame] -> Evaluate a -> Evaluate a
withScope frames thunk =
    do prevFrames <- liftM envFrames getEnv
       modify $ \env -> env { envFrames = frames }
       value <- thunk
       modify $ \env -> env { envFrames = prevFrames }
       return value

currentFrame :: Evaluate Frame
currentFrame = liftM (head . envFrames) getEnv

-- Continuation
pushCont :: (Value -> Evaluate Value) -> ContType -> Evaluate ()
pushCont c ct =
    modify (\env@Env { envContStack = cs } -> env { envContStack = [Cont ct c] ++ cs })

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
    do cont <- popCont
       if contType cont == ct
          then (contRecv cont) value
          else returnCont ct value

-- Exception
throw :: Exception -> Evaluate Value
throw e =
    returnCont CThrow (Exception e)

getThis :: Evaluate Value
getThis = do env <- getEnv
             return $ getThis' $ envFrames env
          where getThis' (WithFrame { }:fs)
                    = getThis' fs
                getThis' (f:_)
                    = frThis f

bindParamArgs :: [Parameter] -> [Value] -> Evaluate Binding
bindParamArgs params args =
    do binding <- mapM zipArg $ zip params args
       return $ nullObject { objPropMap = mkPropMap binding, objClass = "Activation" }
    where zipArg :: (Parameter, Value) -> Evaluate (String, Value, [PropertyAttribute])
          zipArg (param, arg) = 
              do argRef <- makeRef arg
                 return (param, argRef, [])

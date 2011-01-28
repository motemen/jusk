module Internal where
import DataTypes

throw :: String -> String -> Evaluate Value

prototypeOf :: Value -> Evaluate Value
classOf :: Value -> Evaluate String

getProp :: Value -> String -> Evaluate Value
putProp :: Value -> String -> (Value, [PropertyAttribute]) -> Evaluate ()
canPut :: Value -> String -> Evaluate Bool
hasProperty :: Value -> String -> Evaluate Bool

getValue :: Value -> Evaluate Value
putValue :: Value -> Value -> Evaluate ()

getVar :: String -> Evaluate Value
setVar :: String -> Value -> Evaluate Value

getVarFrameObject :: String -> Evaluate (Maybe Value)

isBound :: String -> Evaluate Bool

defineVar :: String -> Value -> Evaluate Value

prototypeOfVar :: String -> Evaluate Value

withNoRef :: (Value -> Evaluate a) -> Value -> Evaluate a
withNoRef2 :: (Value -> Value -> Evaluate a) -> Value -> Value -> Evaluate a

modifyValue :: Value -> (Value -> Value) -> Evaluate ()

makeNewObject :: Evaluate Value
makeNewArray :: [Value] -> Evaluate Value

ifM :: Monad m => m Bool -> m a -> m a -> m a

(!) :: Value -> String -> Value
(<~) :: Value -> Value -> Evaluate ()

{-
    Internal.hs
    内部プロパティとメソッド
-}

module Internal where
import Data.IORef
import Monad
import Control.Monad.Trans
import Maybe

import DataTypes
import Context
import Parser(natural, runLex)

property :: Value -> String -> Evaluate (Maybe Value)
property object@(Object { }) p =
    return $ lookup p (properties object)

property (Array array) "length" =
    return $ Just $ Number $ Integer $ toInteger $ length array

property (Array array) p =
    case (runLex natural p) of
         Left _  -> do a <- getVar "Array" >>= flip getProp "prototype"
                       property a p
         Right n -> lift $ return $ Just $ array !! (fromInteger n)

property (Ref objRef) p =
    do object <- liftAll $ readIORef objRef
       property object p

property (Function { }) "prototype" =
    do object <- getVar "Object"
       property object "prototype"

-- XXX: Debug
property o p =
    do liftAll $ print $ "property: " ++ show o ++ " " ++ p
       return $ Nothing

propAttr :: Value -> String -> Evaluate (Maybe [PropertyAttribute])
propAttr object@(Object { }) p =
    return $ lookup p (attributes object)

isPrimitive :: Value -> Bool
isPrimitive Undefined   = True
isPrimitive Null        = True
isPrimitive (Boolean _) = True
isPrimitive (Number _)  = True
isPrimitive (String _)  = True
isPrimitive _           = False

assign :: Eq a => [(a, b)] -> a -> b -> [(a, b)]
assign = flip assign' []
       where
          assign' :: Eq a => [(a, b)] -> [(a, b)] -> a -> b -> [(a, b)]
          assign' [] assigned key value =
            (key,value):reverse assigned
          assign' ((k,v):pairs) assigned key value
            | k == key  = reverse ((key,value):assigned) ++ pairs
            | otherwise = assign' pairs ((k,v):assigned) key value

-- http://www2u.biglobe.ne.jp/~oz-07ams/prog/ecma262r3/8_Types.html#section-8.6

prototypeOf :: Value -> Evaluate Value
prototypeOf (Array _) =
    do array <- getVar "Array"
       getProp array "prototype"

prototypeOf (Ref objRef) =
    do object <- liftAll $ readIORef objRef
       prototypeOf object

prototypeOf obj =
    return $ case obj of
                  Object { } -> prototype obj
                  _ -> Null

classOf :: Value -> Evaluate Value
classOf obj =
    return $ case obj of
                  Object { } -> String $ className obj
                  _ -> Null

-- value :: Value -> InternalProperty

ifNull :: (Value -> a) -> a -> Value -> a
ifNull _ g Null = g
ifNull f _ v = f v

getProp :: Value -> String -> Evaluate Value
getProp object p =
    property object p
    >>= maybe (prototypeOf object >>= flip getProp p `ifNull` (lift $ return Undefined))
              (lift . return)

putProp :: IORef Value -> String -> Value -> Evaluate ()
putProp objRef p ref@(Ref _) =
    do object <- liftAll $ readIORef objRef
       putProp' object p ref
    where
        putProp' (Ref objRef) p ref = putProp objRef p ref
        putProp' object p ref =
            do canPut <- canPut object p
               when (canPut)
                    (liftAll
                     $ modifyIORef
                           objRef
                           (\object@Object { properties = props, attributes = attrs }
                                -> object {
                                       properties = assign props p ref,
                                       attributes = assign attrs p []
                                   }))

putProp objRef p value =
    do ref <- liftAll $ liftM Ref $ newIORef value
       putProp objRef p ref

canPut :: Value -> String -> Evaluate Bool
canPut object p =
    propAttr object p 
    >>= maybe (prototypeOf object >>= flip canPut p `ifNull` (lift $ return True))
              (lift . return . not . elem ReadOnly)

hasProperty :: Value -> String -> Evaluate Bool
hasProperty object@(Object { properties = props }) p =
    maybe (prototypeOf object >>= flip hasProperty p `ifNull` (lift $ return False))
          (const $ lift $ return True)
          (lookup p props)

-- delete :: IORef Value -> String -> IO Bool

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

makeRef :: Value -> Evaluate Value
makeRef ref@(Ref _) = return ref

makeRef object =
    do ref <- liftAll $ newIORef object
       return $ Ref ref

readRef :: Value -> Evaluate Value
readRef (Ref objRef) =
    do object <- liftAll $ readIORef objRef
       readRef object

readRef object = return object

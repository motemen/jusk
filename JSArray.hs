{-
    JSArray.hs
    Arrayオブジェクト
    http://www2u.biglobe.ne.jp/~oz-07ams/prog/ecma262r3/15-4_Array_Objects.html
-}

module JSArray where
import Monad hiding(join)
import List(intersperse)
import Data.Map hiding(null)
import Data.IORef

import DataTypes
import Context
import Internal
import Eval

-- Array.prototype
prototypeObject :: Value
prototypeObject =
    nullObject {
        objPropMap = mkPropMap [("constructor", NativeFunction make,           []),
                                ("toString",    NativeFunction toStringMethod, []),
                                ("push",        NativeFunction push,           []),
                                ("pop",         NativeFunction pop,            []),
                                ("unshift",     NativeFunction unshift,        []),
                                ("shift",       NativeFunction shift,          []),
                                ("join",        NativeFunction join,           [])
                                ]
    }

-- Array()
function :: NativeFunction
function = make

-- new Array()
make :: NativeFunction
make [Number (Integer n)] =
    return $ Array $ take (fromInteger n) (repeat Undefined)

make xs =
    return $ Array xs

-- Array.prototype.push
push :: NativeFunction
push [] =
    do this <- getThis
       len <- getProp this "length"
       return len

push xs =
    do thisRef <- getThis
       this <- readRef thisRef -- Must be a reference
       liftAll $ case this of
                      Array _ -> modifyIORef (getRef thisRef)
                                             (\(Array array) -> Array $ array ++ xs)
       len <- getProp thisRef "length"
       return len

-- Array.prototype.pop
pop :: NativeFunction
pop _ =
    do thisRef <- getThis
       this <- readRef thisRef
       case this of
            Array []    -> return Undefined
            Array array -> do liftAll $ modifyIORef (getRef thisRef)
                                                    (\(Array array) -> Array $ init array)
                              return $ last array

-- Array.prototype.unshift
unshift :: NativeFunction
unshift [] =
    do this <- getThis
       len <- getProp this "length"
       return len

unshift xs =
    do thisRef <- getThis
       this <- readRef thisRef -- Must be a reference
       liftAll $ case this of
                      Array _ -> modifyIORef (getRef thisRef)
                                             (\(Array array) -> Array $ xs ++ array)
       len <- getProp thisRef "length"
       return len

-- Array.prototype.shift
shift :: NativeFunction
shift _ =
    do thisRef <- getThis
       this <- readRef thisRef
       case this of
            Array []    -> return Undefined
            Array array -> do liftAll $ modifyIORef (getRef thisRef)
                                                    (\(Array array) -> Array $ tail array)
                              return $ last array

-- Array.prototype.toString
toStringMethod :: NativeFunction
toStringMethod _ = join []

-- Array.prototype.join
join :: NativeFunction
join args =
    do this <- getThis >>= readRef
       delim <- if null args then liftAll $ return "," else toString $ head args
       case this of
            Array array
                -> do strs <- mapM toString array
                      return $ String $ foldl (++) "" $ intersperse delim strs

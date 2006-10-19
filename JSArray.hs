{-
    JSArray.hs
    Arrayオブジェクト
    http://www2u.biglobe.ne.jp/~oz-07ams/prog/ecma262r3/15-4_Array_Objects.html
-}

module JSArray where
import Monad hiding(join)
import List(intersperse)
import Data.IORef

import DataTypes
import Context
import Internal
import Eval

-- Array.prototype
prototypeObject :: Value
prototypeObject =
    nullObject {
        objPropMap = mkPropMap [("constructor", NativeFunction constructor,    []),
                                ("toString",    NativeFunction toStringMethod, []),
                                ("push",        NativeFunction push,           []),
                                ("pop",         NativeFunction pop,            []),
                                ("unshift",     NativeFunction unshift,        []),
                                ("shift",       NativeFunction shift,          []),
                                ("concat",      NativeFunction concatMethod,   []),
                                ("join",        NativeFunction join,           [])
                                ]
    }

-- Array()
function :: NativeFunction
function = constructor

-- new Array()
constructor :: NativeFunction
constructor [Number (Integer n)] =
    return $ Array $ take (fromInteger n) (repeat Undefined)

constructor xs =
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
                              return $ head array

-- Array.prototype.toString
toStringMethod :: NativeFunction
toStringMethod _ = join []

-- Array.prototype.concat
concatMethod :: NativeFunction
concatMethod args =
    do this <- readRef =<< getThis
       case this of
            Array array -> do args <- mapM readRef args
                              makeRef $ Array $ concatArgs array args
    where concatArgs array [] =
              array
          concatArgs array ((Array array'):xs) =
              concatArgs (array ++ array') xs
          concatArgs array (x:xs) =
              concatArgs (array ++ [x]) xs

-- Array.prototype.join
join :: NativeFunction
join args =
    do this <- readRef =<< getThis
       delim <- if null args then liftAll $ return "," else toString $ head args
       case this of
            Array array
                -> do strs <- mapM toString array
                      return $ toValue $ concat $ intersperse delim strs

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
import Internal
import Eval

-- Array.prototype
prototypeObject :: Value
prototypeObject =
    nullObject {
        objPropMap = nativeFuncPropMap [("constructor", constructor,    1),
                                        ("toString",    toStringMethod, 0),
                                        ("toSource",    toSourceMethod, 0),
                                        ("push",        push,           1),
                                        ("pop",         pop,            0),
                                        ("unshift",     unshift,        1),
                                        ("shift",       shift,          0),
                                        ("concat",      concatMethod,   1),
                                        ("join",        join,           1)]
    }

makeArray :: [Value] -> Evaluate Value
makeArray xs =
    do proto <- prototypeOfVar "Array"
       return $ nullObject { objClass = "Array", objPrototype = proto, objObject = Array xs }

-- Array
function :: NativeCode
function = constructor

-- new Array
constructor :: NativeCode
constructor _ [Number (Integer n)] =
    do proto <- prototypeOfVar "Array"
       return $ nullObject { objClass = "Array", objPrototype = proto, objObject = Array $ take (fromInteger n) (repeat Undefined) }

constructor _ xs =
    makeArray xs

-- Array.prototype.push
push :: NativeCode
push this [] =
    do len <- getProp this "length"
       return len

push thisRef xs =
    do this <- readRef thisRef
       case this of
            Object { objObject = Array array } -> liftIO $ modifyIORef (getRef thisRef) $ setObjObject (Array $ array ++ xs)
            _ -> do throw "NotImplemented" $ "Array.prototype.push: " ++ show this
                    return ()
       len <- getProp thisRef "length"
       return len

-- Array.prototype.pop
pop :: NativeCode
pop thisRef _ =
    do this <- readRef thisRef
       case this of
            Object { objObject = Array [] }    -> return Undefined
            Object { objObject = Array array }
                -> do liftIO $ modifyIORef (getRef thisRef) $ setObjObject (Array $ init array)
                      return $ last array
            _ -> throw "NotImplemented" $ "Array.prototype.pop: " ++ show this

-- Array.prototype.unshift
unshift :: NativeCode
unshift this [] =
    do len <- getProp this "length"
       return len

unshift thisRef xs =
    do this <- readRef thisRef
       case this of
            Object { objObject = Array array } -> liftIO $ modifyIORef (getRef thisRef) $ setObjObject (Array $ xs ++ array)
            _ -> do throw "NotImplemented" $ "Array.prototype.unshift: " ++ show this
                    return ()
       len <- getProp thisRef "length"
       return len

-- Array.prototype.shift
shift :: NativeCode
shift thisRef _ =
    do this <- readRef thisRef
       case this of
            Object { objObject = Array [] }    -> return Undefined
            Object { objObject = Array array }
                -> do liftIO $ modifyIORef (getRef thisRef) $ setObjObject (Array $ tail array)
                      return $ head array
            _ -> do throw "NotImplemented" $ "Array.prototype.shift: " ++ show this
                    return Void

-- Array.prototype.toString
toStringMethod :: NativeCode
toStringMethod this _ = join this []

-- Array.prototype.toSource
toSourceMethod :: NativeCode
toSourceMethod this _ =
    do this <- readRef this
       case this of
            Object { objObject = Array array }
                -> do srcArray <- mapM (\o -> callMethod o "toSource" [] >>= toString) array
                      return $ toValue $ "[" ++ (concat $ intersperse "," srcArray) ++ "]"
-- Array.prototype.concat
concatMethod :: NativeCode
concatMethod this args =
    do this <- readRef this
       case this of
            Object { objObject = Array array }
                -> do args <- mapM readRef args
                      makeRef =<< makeArray (concatArgs array args)
    where concatArgs array [] =
              array
          concatArgs array ((Object { objObject = Array array' }):xs) =
              concatArgs (array ++ array') xs
          concatArgs array (x:xs) =
              concatArgs (array ++ [x]) xs

-- Array.prototype.join
join :: NativeCode
join this args =
    do this <- readRef this
       delim <- if null args then liftIO $ return "," else toString $ head args
       case this of
            Object { objObject = Array array }
                -> do strs <- mapM toString array
                      return $ toValue $ concat $ intersperse delim strs

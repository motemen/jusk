{-
    JSArray.hs
    Arrayオブジェクト
    http://www2u.biglobe.ne.jp/~oz-07ams/prog/ecma262r3/15-4_Array_Objects.html
-}

module JSArray where
import Prelude hiding (toInteger)
import Monad hiding (join)
import List (intersperse)
import Control.Monad hiding (join)

import DataTypes
import Internal
import Eval
import Operator
import Context

-- Array.prototype
prototypeObject :: Value
prototypeObject =
    nullObject {
        objPropMap = nativeFuncPropMap [
                ("constructor", constructor,    1),
                ("toString",    toStringMethod, 0),
                ("toSource",    toSourceMethod, 0),
                ("concat",      concatMethod,   1),
                ("join",        join,           1),
                ("pop",         pop,            0),
                ("push",        push,           1),
                ("shift",       shift,          0),
                ("reverse",     reverseMethod,  0),
                ("slice",       slice,          2),
                ("sort",        sort,           1),
                ("splice",      splice,         2),
                ("unshift",     unshift,        1),
                ("forEach",     forEachMethod,  2)
            ]
    }

makeArray :: [Value] -> Value
makeArray xs =
    nullObject { objClass = "Array", objObject = Array xs }

-- Array
function :: NativeCode
function = constructor

-- new Array
constructor :: NativeCode
constructor _ [Number (Integer n)] =
    return $ makeArray  $ take (fromInteger n) (repeat Undefined)

constructor _ xs =
    return $ makeArray xs

-- Array.prototype.concat
concatMethod :: NativeCode
concatMethod this args =
    do this <- readRef this
       case this of
            Object { objObject = Array array } ->
                do args <- mapM readRef args
                   makeRef $ makeArray (concatArgs array args)
            _ -> throw "NotImplemented" $ "Array.prototype.concat: " ++ show this
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
            Object { objObject = Array array } ->
                do strs <- mapM toString array
                   return $ toValue $ concat $ intersperse delim strs
            _ -> throw "NotImplemented" $ "Array.prototype.join: " ++ show this

-- Array.prototype.pop
pop :: NativeCode
pop thisRef _ =
    do this <- readRef thisRef
       case this of
            Object { objObject = Array [] } -> return Undefined
            Object { objObject = Array array } ->
                do modifyValue thisRef $ setObjObject (Array $ init array)
                   return $ last array
            _ -> throw "NotImplemented" $ "Array.prototype.pop: " ++ show this

-- Array.prototype.push
push :: NativeCode
push this [] =
    do len <- getProp this "length"
       return len

push thisRef xs =
    do this <- readRef thisRef
       case this of
            Object { objObject = Array array } ->
                modifyValue thisRef $ setObjObject (Array $ array ++ xs)
            _ -> do throw "NotImplemented" $ "Array.prototype.push: " ++ show this
                    return ()
       len <- getProp thisRef "length"
       return len

-- Array.prototype.reverse
reverseMethod :: NativeCode
reverseMethod thisRef _ =
    do this <- readRef thisRef
       case this of
            Object { objObject = Array array } ->
                do modifyValue thisRef $ setObjObject (Array $ reverse array)
                   return thisRef
            _ -> throw "NotImplemented" $ "Array.prototype.push: " ++ show this

-- Array.prototype.shift
shift :: NativeCode
shift thisRef _ =
    do this <- readRef thisRef
       case this of
            Object { objObject = Array [] }    -> return Undefined
            Object { objObject = Array array } ->
                do modifyValue thisRef $ setObjObject (Array $ tail array)
                   return $ head array
            _ -> do throw "NotImplemented" $ "Array.prototype.shift: " ++ show this
                    return Void

-- Array.prototype.unshift
unshift :: NativeCode
unshift this [] =
    do len <- getProp this "length"
       return len

unshift thisRef xs =
    do this <- readRef thisRef
       case this of
            Object { objObject = Array array } ->
                modifyValue thisRef $ setObjObject (Array $ xs ++ array)
            _ -> do throw "NotImplemented" $ "Array.prototype.unshift: " ++ show this
                    return ()
       len <- getProp thisRef "length"
       return len

-- Array.prototype.slice
slice :: NativeCode
slice this [] = slice this [Number $ Integer 0, Undefined]

slice this [start] = slice this [start, Undefined]

slice thisRef (start:end:_) =
    do this <- readRef thisRef
       length <- toUInt =<< getProp this "length"
       start <- normalizePos start length
       end <- if isUndefined end then return length else normalizePos end length
       case this of
            object@Object { objObject = Array array } ->
                makeRef object { objObject = Array $ drop start $ take end array }
            _ -> do throw "NotImplemented" $ "Array.prototype.slice: " ++ show this
                    return Void
    where normalizePos pos length =
              do pos <- toInt pos
                 if pos < 0
                    then return $ max (pos + length) 0
                    else return $ min pos length

-- Array.prototype.sort
sort :: NativeCode
sort thisRef [] =
    do this <- readRef thisRef
       case this of
            object@Object { objObject = Array array } ->
                do sorted <- sortM array
                   modifyValue thisRef (const $ object { objObject = Array sorted })
                   return thisRef
            _ -> do throw "NotImplemented" $ "Array.prototype.sort: " ++ show this
                    return Void

sort thisRef (compareFn:_) =
    do this <- readRef thisRef
       case this of
            object@Object { objObject = Array array } ->
                do sorted <- sortByM cmp array
                   modifyValue thisRef (const $ object { objObject = Array sorted })
                   return thisRef
            _ -> do throw "NotImplemented" $ "Array.prototype.sort: " ++ show this
                    return Void
    where cmp x y = call compareFn [x, y]

sortM xs =
    mergeM compare (take n xs) (drop n xs)
    where n = length xs `div` 2
          compare x y = ifM (toBoolean =<< comparisonOp (<) x y)
                            (return $ toValue (-1 :: Int))
                            (ifM (toBoolean =<< comparisonOp (>) x y)
                                 (return $ toValue (1 :: Int))
                                 (return $ toValue (0 :: Int)))

sortByM f xs =
    mergeM f (take n xs) (drop n xs)
    where n = length xs `div` 2

mergeM _ xs [] = return xs
mergeM _ [] ys = return ys
mergeM f xs ys =
    do (x:xs) <- sortByM f xs
       (y:ys) <- sortByM f ys
       ord <- toInt =<< f x y
       if ord < 0
          then liftM (x:) $ mergeM f xs (y:ys)
          else liftM (y:) $ mergeM f (x:xs) ys

-- Array.prototype.splice
splice :: NativeCode
splice thisRef (start:count:items) =
    do this <- readRef thisRef
       length <- toUInt =<< getProp this "length"
       start <- normalizePos start length
       count <- toInt count
       let c = min (max count 0) (length - start)
       case this of
            object@Object { objObject = Array array } ->
                do modifyValue thisRef (const $ object { objObject = Array $ replace array start c items })
                   makeRef (makeArray $ take c $ drop start array)
            _ -> do throw "NotImplemented" $ "Array.prototype.splice: " ++ show this
                    return Void
    where replace array start count items =
              take start array ++ items ++ drop (start + count) array
          normalizePos pos length =
              do pos <- toInt pos
                 if pos < 0
                    then return $ max (pos + length) 0
                    else return $ min pos length

splice thisRef [start] =
    splice thisRef [start, Undefined]

splice _ [] =
    return Undefined

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
            _ -> throw "NotImplemented" $ "Array.prototype.toSource: " ++ show this

-- Array.prototype.forEach
forEachMethod :: NativeCode
forEachMethod thisRef (callbackFn:thisObject:_) =
    do this <- readRef thisRef
       case this of
            object@Object { objObject = Array array } ->
                do forM [0 .. length array - 1] 
                        (\i -> callWithThis this callbackFn [ array !! i, toValue i, this ])
                   return Void
            _ -> do throw "NotImplemented" $ "Array.prototype.forEach: " ++ show this
                    return Void

forEachMethod thisRef [ callbackFn ] =
    do global <- getGlobal
       forEachMethod thisRef [ callbackFn, global ]

forEachMethod thisRef [] =
    forEachMethod thisRef [ Undefined ]

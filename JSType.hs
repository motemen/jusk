{-
    JSType.hs
    型変換
-}

module JSType where
import Prelude hiding(toInteger)
import Data.IORef

import DataTypes
import Parser (numericLiteral)
import ParserUtil (runLex)
import Internal
import Context
import Eval
import PrettyShow

toPrimitive :: Value -> String -> Evaluate Value
toPrimitive Undefined _ =
    return Undefined

toPrimitive Null _ =
    return Null

toPrimitive bool@(Boolean _) _ =
    return bool

toPrimitive num@(Number _) _ =
    return num

toPrimitive string@(String _) _ =
    return string

toPrimitive ref@(Reference { }) preferredType =
    flip toPrimitive preferredType =<< getValue ref

toPrimitive ref@(Ref _) preferredType =
    flip toPrimitive preferredType =<< readRef ref

toPrimitive object preferredType =
    defaultValue object preferredType

toBoolean :: Value -> Evaluate Bool
toBoolean Undefined = return False
toBoolean Null      = return False

toBoolean (Boolean bool) = return bool

toBoolean (Number NaN)         = return False
toBoolean (Number (Double n))  = return $ n /= 0
toBoolean (Number (Integer n)) = return $ n /= 0

toBoolean (String string) = return $ not $ null string

toBoolean _ = return True

toNumber :: Value -> Evaluate Number
toNumber Undefined =
    return NaN

toNumber Null =
    return $ Integer 0

toNumber (Boolean bool) =
    return $ if bool then Integer 1
                     else Double 0.0

toNumber (Number num) =
    return num

toNumber (String string) =
    case runLex numericLiteral string of
         Left _ -> return NaN
         Right (Literal (Number n)) -> return n

toNumber object@Object { } =
    toPrimitive object "Number" >>= toNumber

toNumber ref@Ref { } =
    readRef ref >>= toNumber

toNumber ref@Reference { } =
    getValue ref >>= toNumber

toNumber o =
    (throw $ NotImplemented $ "toNumber: " ++ show o) >> return NaN

toInteger :: Value -> Evaluate Integer
toInteger (Number NaN) = return 0

toInteger (Number (Integer n)) = return n

toInteger (Number (Double n)) = return $ truncate n

toInteger value =
    do num <- toNumber value
       toInteger $ Number num

toIntWith :: (Integer -> Int) -> Value -> Evaluate Int
toIntWith convert value =
    do num <- toNumber value
       return $ case num of
                     NaN -> 0
                     Double n | isInfinite n -> 0
                              | otherwise -> convert $ truncate n
                     Integer n -> convert n

toInt :: Value -> Evaluate Int
toInt = toIntWith integerToInt
    where integerToInt :: Integer -> Int
          integerToInt n =
              let intMax = floor (2**32)
                  n' = n `rem` intMax
                  in fromEnum $ if n' >= floor (2**31)
                                   then n' - intMax
                                   else n'

toUInt :: Value -> Evaluate Int
toUInt = toIntWith integerToUInt
    where integerToUInt :: Integer -> Int
          integerToUInt n =
              fromEnum $ n `rem` floor (2**32)

toUInt16 :: Value -> Evaluate Int
toUInt16 = toIntWith integerToUInt16
    where integerToUInt16 :: Integer -> Int
          integerToUInt16 n =
              fromEnum $ n `rem` floor (2**16)

toString :: Value -> Evaluate String
toString Void      = return ""
toString Undefined = return "undefined"
toString Null      = return "null"

toString (Boolean False) = return "false"
toString (Boolean True)  = return "true"

toString (String string) = return string

toString (Number (Integer n)) = return $ show n
toString (Number (Double n))  = return $ show n
toString (Number NaN)         = return "NaN"

toString Object { objName = name, objObject = NativeFunction { } } =
    return $ "function " ++ name ++ "() { [native code] }"

toString ref@Reference { } =
    do object <- getValue ref
       toString object

toString (Ref objRef) =
    do object <- liftIO $ readIORef objRef
       toString object

toString (Exception e) =
    return $ show e

toString object = 
    do s <- callMethod object "toString" []
       case s of
            String s -> return s
            Object { objValue = value } | not (isNull value) -> toString value
            _ -> return $ show $ TypeError $ show object ++ ".toString did not return string: " ++ show s

toSource :: Value -> Evaluate String
toSource Void      = return ""
toSource Undefined = return "undefined"
toSource Null      = return "null"

toSource (Boolean False) = return "false"
toSource (Boolean True)  = return "true"

toSource (String string) = return $ show string

toSource (Number (Integer n)) = return $ show n
toSource (Number (Double n))  = return $ show n
toSource (Number NaN)         = return "NaN"

toSource func@Object { objObject = Function { } } =
    return $ prettyShow func
    
toSource Object { objName = name, objObject = NativeFunction { } } =
    return $ "function " ++ name ++ "() { [native code] }"

toSource ref@Reference { } =
    do object <- getValue ref
       toSource object

toSource (Ref objRef) =
    do object <- liftIO $ readIORef objRef
       toSource object

toSource object = 
    toString =<< callMethod object "toSource" []

toObject :: Value -> Evaluate Value
toObject Undefined =
    throw $ TypeError "undefined cannot be converted to object"

toObject Null =
    throw $ TypeError "null cannot be converted to object"

toObject num@(Number _) =
    do klass <- getVar "Number"
       object <- makeRef =<< construct klass []
       liftIO $ modifyIORef (getRef object) (setObjValue num)
       return object

toObject str@(String _) =
    do klass <- getVar "String"
       object <- makeRef =<< construct klass []
       liftIO $ modifyIORef (getRef object) (setObjValue str)
       return object

toObject x = return x

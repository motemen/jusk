{-# OPTIONS_GHC -fglasgow-exts #-}
{-
    DataTypes.hs
    Haskell内部の型
-}

module DataTypes (module DataTypes, Control.Monad.Trans.liftIO) where
import Data.Map (Map, assocs)
import qualified Data.Map as Map
import Data.IORef
import System.IO.Unsafe
import Control.Monad.Trans (liftIO)
import Control.Monad.State
import Control.Monad.Cont hiding(Cont)
import Text.Regex
import List

data Flag
    = Debug
    | Warn
    | ParseOnly
    | InputFile String
    | EvalStr String
    | EnterRepl
    | Version
    deriving (Show, Eq)

class Eval a where
    eval :: a -> Evaluate Value

class ToValue a where
    toValue :: a -> Value

type Evaluate a
    = ContT Value (StateT Env IO) a

data Env
    = Env { envFrames :: [Frame], envContStack :: [Cont], envFlags :: [Flag] }

instance Show Env where
    show (Env { envFrames = frames, envContStack = conts, envFlags = flags }) =
        "  Frames:\n" ++
        (unlines $ map indent $ map show frames) ++
        "  Cont:\n" ++
        (unlines $ map indent $ map show conts) ++
        "  Flags:\n" ++
        (indent $ unwords $ map show flags)
        where indent s = "    " ++ s

data Frame
    = GlobalFrame { frObject :: Value, frThis :: Value }
    | Activation { frObject :: Value, frThis :: Value }
    | WithFrame { frObject :: Value }
    deriving Eq

instance Show Frame where
    show (GlobalFrame { frObject = object, frThis = this }) =
        "<GlobalFrame " ++ show object ++ " this=" ++ show this ++ ">"
    show (Activation { frObject = object, frThis = this }) =
        "<Activation " ++ show object ++ " this=" ++ show this ++ ">"
    show (WithFrame { frObject = object }) =
        "<WithFrame " ++ show object ++ ">"

data Cont
    = Cont { contType :: ContType, contRecv :: Value -> Evaluate Value, contScope :: [Frame] }

instance Show Cont where
    show Cont { contType = ct } = show ct

type JavaScriptProgram =
    [Statement]

type VariableBinding
    = (String, Maybe Expression)

data Statement
    = STVarDef { varDefBindings :: [VariableBinding] }
    | STFuncDef { funcDefName :: String, funcDefFunc :: NativeObject }
    | STEmpty
    | STExpression Expression
    | STBlock [Statement]
    | STLabelled String Statement
    | STIf { ifCond :: Expression, ifThen :: Statement, ifElse :: Maybe Statement }
    | STSwitch { swExpression :: Expression, swClauses :: [(Maybe Expression, Statement)] }
    | STDoWhile Expression Statement
    | STWhile Expression Statement
    | STFor { forInitialize :: Statement, forCondition :: Expression, forUpdate :: Expression, forBlock :: Statement }
    | STForIn { forBinding :: Statement, forObject :: Expression, forBlock :: Statement }
    | STWith { withExpression :: Expression, withBlock :: Statement }
    | STContinue (Maybe String)
    | STBreak (Maybe String)
    | STReturn (Maybe Expression)
    | STThrow Expression
    | STTry { tryClause :: Statement, tryCatchClause :: Maybe (Parameter, Statement), tryFinallyClause :: Maybe Statement }
    deriving (Show, Eq)

data Expression
    = Keyword String
    | Punctuator String
    | Identifier String
    | Literal Value
    | ArrayLiteral [Expression]
    | ObjectLiteral [(Expression, Expression)]
    | RegExpLiteral String [Char]
    | List [Expression]
    | Operator { opOperator :: String, opArgs :: [Expression] }
    | Let { letLeft :: Expression, letRight :: Expression }
    deriving (Show, Eq)

data Value
    = Undefined
    | Null
    | Boolean Bool
    | Number Number
    | String String
    | Object {
        objPropMap   :: Map String PropertyPair,

        --  内部プロパティ
        objPrototype :: Value,              -- [[Prototype]]
        objClass     :: String,             -- [[Class]]
        objValue     :: Value,              -- [[Value]]
        objConstruct :: Maybe NativeCode,   -- [[Construct]]
        objName      :: String,

        objObject    :: NativeObject
      }
{-
    内部でのみ使用
-}
    | Reference { refBase :: Value, refName :: String }
    | Ref { getRef :: IORef Value }
    | Void
    deriving Eq

data NativeObject
    = SimpleObject
    | Function {
        funcParam     :: Parameters,
        funcBody      :: Statement,
        funcScope     :: [Frame]            -- [[Scope]]
      }
    | NativeFunction {
        funcArity     :: Int,
        funcNatCode   :: NativeCode
      }
    | RegExp {
        regexpRegex   :: Regex,
        regexpPattern :: String,
        regexpFlags   :: [Char]
      }
    | Array [Value]

instance Eq NativeObject where
    (==) (Function p s f) (Function p' s' f') = p == p' && s == s' && f == f'
    (==) (NativeFunction { }) (NativeFunction { }) = False
    (==) (RegExp _ p f) (RegExp _ p' f') = p == p' && f == f'
    (==) _ _ = False
    
setObjName :: String -> Value -> Value
setObjName name object@Object { } = object { objName = name }
setObjName _ x = x

setObjProto :: Value -> Value -> Value
setObjProto proto object@Object { } = object { objPrototype = proto }
setObjProto _ x = x

setObjValue :: Value -> Value -> Value
setObjValue value object@Object { } = object { objValue = value }
setObjValue _ x = x

setObjObject :: NativeObject -> Value -> Value
setObjObject obj object@Object { } = object { objObject = obj }
setObjObject _ x = x

instance Show Value where
    show Undefined = "undefined"
    show Null      = "null"

    show (Boolean True)  = "true"
    show (Boolean False) = "false"

    show (Number (Integer n)) = show n
    show (Number (Double n))  = show n
    show (Number NaN)         = "NaN"

    show (String string) = show string

    show (Object { objPropMap = propMap,
                   objPrototype = prototype,
                   objClass = klass,
                   objValue = value,
                   objName = name,
                   objObject = obj }) =
        "<Object" ++ (if null name then "" else " " ++ show name) ++
            " {" ++ showMap propMap ++ "}" ++
            " #prototype=" ++ showShallow prototype ++
            " #class=" ++ show klass ++
            " #value=" ++ show value ++
            " #object=" ++ show obj ++ ">"
        where showMap mapData =
                  concat $ ", " `intersperse` map showPair (assocs mapData)
              showPair (k, v) =
                  k ++ ": " ++ show v

    show (Reference baseRef p) = "<Reference " ++ show baseRef ++ " " ++ p ++ ">"

    show (Ref refObj) = "<Ref " ++ (show $ unsafePerformIO $ readIORef refObj) ++ ">"

    show Void = "<Void>"

{-
instance Eq Value where
    Undefined == Undefined = True
    Null == Null           = True
    Undefined == Null      = True
    Null == Undefined      = True

    (Number n) == (Number m)   = n == m
    (String s) == (String t)   = s == t
    (Boolean a) == (Boolean b) = a == b
-}

instance Show NativeObject where
    show SimpleObject = "<SimpleObject>"
    show (Function { funcParam = params, funcBody = body }) =
        "<Function " ++ show params ++ " " ++ show body ++ ">"
    show (NativeFunction { }) = "<NativeFunction>"
    show (RegExp { regexpPattern = pattern }) = "<RegExp " ++ pattern ++ ">"
    show (Array _) = "<Array>"

showShallow :: Value -> String
showShallow (Object { objName = "" })   = "<Object ...>"
showShallow (Object { objName = name }) = name

showShallow (Ref refObj)   = "<Ref " ++ (showShallow $ unsafePerformIO $ readIORef $ refObj) ++ ">"
showShallow x = show x

type NativeCode
    = Value -> [Value] -> Evaluate Value

instance Eq (a -> b) where
    (/=) _ _ = True

data Number
    = Integer Integer
    | Double Double
    | NaN
    deriving Show

instance Eq Number where
    NaN == _ = False
    _ == NaN = False
    (Integer n) == (Integer m) = n == m
    (Double n) == (Double m) = n == m
    (Integer n) == (Double m) = (toEnum $ fromEnum n) == m
    (Double n) == (Integer m) = n == (toEnum $ fromEnum m)

data PropertyPair
    = PropertyPair { propValue :: Value, propAttr :: [PropertyAttribute] }
    deriving Eq

instance Show PropertyPair where
    show (PropertyPair { propValue = value, propAttr = [] }) =
        showShallow value

    show (PropertyPair { propValue = value, propAttr = attrs }) =
        showShallow value ++ "(" ++ concat ("," `intersperse` map show attrs) ++ ")"

mkProp :: Value -> [PropertyAttribute] -> PropertyPair
mkProp = PropertyPair

mkPropMap :: [(String, Value, [PropertyAttribute])] -> Map String PropertyPair
mkPropMap values =
    Map.fromList $ map mkPropEntry values
    where mkPropEntry (name, value, attr) = (name, mkProp value attr)

data PropertyAttribute
    = ReadOnly
    | DontEnum
    | DontDelete
    deriving (Show, Eq)

type Parameters =
    [Parameter]

type Parameter =
    String

type RestParameter =
    Maybe Parameter

data ContType
    = CReturn
    | CThrow
    | CBreak { ctLabel :: Maybe String }
    | CContinue { ctLabel :: Maybe String }
    | CExit
    deriving (Eq, Show)

instance Show (a -> b) where
    show _ = ""

instance Show a => Show (IORef a) where
    show x = "IORef " ++ (show $ unsafePerformIO $ readIORef x)

makeRef :: Value -> Evaluate Value
makeRef ref@Ref { } = return ref

makeRef object =
    do ref <- liftIO $ newIORef object
       return $ Ref ref

readRef :: Value -> Evaluate Value
readRef (Ref objRef) =
    do object <- liftIO $ readIORef objRef
       readRef object

readRef object = return object

tidyNumber :: Value -> Value
tidyNumber num@(Number (Double x))
    | isInfinite x || isNaN x = num 
    | x == (fromIntegral $ round x) = Number $ Integer $ round x
    | otherwise = num
tidyNumber x = x

nullObject :: Value
nullObject = Object {
        objPropMap    = Map.empty,
        objValue      = Null,
        objPrototype  = Null,
        objClass      = "Object",
        objConstruct  = Nothing,
        objName       = "",
        objObject     = SimpleObject
    }

nullFunction :: NativeObject
nullFunction = Function {
        funcParam     = [],
        funcBody      = STEmpty,
        funcScope     = []
    }

nullNativeFunc :: NativeObject
nullNativeFunc = NativeFunction {
        funcArity     = 0,
        funcNatCode   = undefined
    }

nativeFunc :: String -> Int -> NativeCode -> Value
nativeFunc name arity code =
    nullObject {
        objName = name,
        objObject = nullNativeFunc {
            funcArity   = arity,
            funcNatCode = code
        }
    }

-- 組み込みオブジェクトを作るヘルパー
nativeFuncPropMap :: [(String, NativeCode, Int)] -> Map String PropertyPair
nativeFuncPropMap =
    mkPropMap . map convert
    where convert (name, code, arity) =
              (name, nativeFunc name arity code, [DontEnum])

isUndefined :: Value -> Bool
isUndefined Undefined = True
isUndefined _ = False

isNull :: Value -> Bool
isNull Null = True
isNull _ = False

isBoolean :: Value -> Bool
isBoolean (Boolean _) = True
isBoolean _ = False

isNumber :: Value -> Bool
isNumber (Number _) = True
isNumber _ = False

isString :: Value -> Bool
isString (String _) = True
isString _ = False

isObject :: Value -> Bool
isObject (Object { }) = True
isObject _ = False

isFunction :: Value -> Bool
isFunction (Object { objObject = Function { } }) = True
isFunction _ = False

isNativeFunction :: Value -> Bool
isNativeFunction (Object { objObject = NativeFunction { } }) = True
isNativeFunction _ = False

isRegExp :: Value -> Bool
isRegExp (Object { objObject = RegExp { } }) = True
isRegExp _ = False

isArray :: Value -> Bool
isArray (Object { objObject = Array { } }) = True
isArray _ = False

isVoid :: Value -> Bool
isVoid Void = True
isVoid _ = False

isReference :: Value -> Bool
isReference (Reference { }) = True
isReference _ = False

isRef :: Value -> Bool
isRef (Ref { }) = True
isRef _ = False

isPrimitive :: Value -> Bool
isPrimitive Undefined   = True
isPrimitive Null        = True
isPrimitive (Boolean _) = True
isPrimitive (Number _)  = True
isPrimitive (String _)  = True
isPrimitive _           = False

typeString :: Value -> String
typeString Undefined    = "undefined"
typeString Null         = "null"
typeString (Boolean _)  = "boolean"
typeString (Number _)   = "number"
typeString (String _)   = "string"
typeString (Object { objObject = Function { }})
                        = "function"
typeString (Object { objObject = NativeFunction { }})
                        = "function"
typeString (Object { }) = "object"
typeString (Ref { })    = "ref"
typeString (Reference { }) = "reference"

getName :: Value -> String
getName o | isPrimitive o = show o
getName Object { objName = name } = name
getName (Reference base name) = getName base ++ "." ++ name
getName (Ref ref) = getName $ unsafePerformIO $ readIORef ref
getName _ = ""

toDouble :: Number -> Double
toDouble (Integer n) = fromIntegral n
toDouble (Double n)  = n
toDouble NaN = undefined

instance ToValue Int where
    toValue n = Number $ Integer $ toEnum n

instance ToValue Integer where
    toValue n = Number $ Integer n

instance ToValue Double where
    toValue n = Number $ Double n

instance ToValue String where
    toValue s = String s

instance ToValue Bool where
    toValue b = Boolean b


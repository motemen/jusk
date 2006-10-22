{-# OPTIONS_GHC -fglasgow-exts #-}
{-
    DataTypes.hs
    Haskell内部の型
-}

module DataTypes (module DataTypes) where
import Data.Map (Map, assocs)
import qualified Data.Map as Map
import Data.IORef
import System.IO.Unsafe
import Control.Monad.State
import Control.Monad.Cont hiding(Cont)
import List

data Flag
    = Debug
    | Warn
    | ParseOnly
    | InputFile String
    | EvalStr String
    | Version
    deriving (Show, Eq)

class Eval a where
    eval :: a -> Evaluate Value

class ToValue a where
    toValue :: a -> Value

type Evaluate a
    = ContT Value (StateT Env IO) a

liftAll :: IO a -> Evaluate a
liftAll = lift . lift

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
    = Cont { contType :: ContType, contRecv :: Value -> Evaluate Value }

instance Show Cont where
    show (Cont { contType = ct }) = show ct

type JavaScriptProgram =
    [Statement]

type VariableBinding
    = (String, Maybe Expression)

data Statement
    = STVarDef { varDefBindings :: [VariableBinding] }
    | STFuncDef { funcDefFunc :: Value }
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
    | Array [Value]
    | Function {
        funcName  :: String,
        funcParam :: Parameters,
        funcBody  :: Statement,
        funcScope :: [Frame],                  -- [[Scope]]
        funcConstruct :: Maybe NativeCode, -- [[Construct]]
        objPropMap :: Map String PropertyPair
      }
    | Object {
        objPropMap :: Map String PropertyPair,

        --  内部プロパティ
        objPrototype :: Value,  -- [[Prototype]]
        objClass     :: String, -- [[Class]]
        objValue     :: Value,  -- [[Value]]
        objName      :: String
      }
    | Exception { exceptionBody :: Exception }
--  | RegularExpression
{-
    内部でのみ使用
-}
    | NativeFunction {
        funcName      :: String,
        funcArity     :: Int,
        funcNatCode   :: NativeCode,
        funcConstruct :: Maybe NativeCode,
        objPropMap    :: Map String PropertyPair
      }
    | Reference { refBase :: Value, refName :: String }
    | Ref { getRef :: IORef Value }
    | Void
    deriving Eq

instance Show Value where
    show Undefined = "undefined"
    show Null      = "null"

    show (Boolean True)  = "true"
    show (Boolean False) = "false"

    show (Number (Integer n)) = show n
    show (Number (Double n))  = show n
    show (Number NaN)         = "NaN"

    show (String string) = show string

    show (Array array) = "[" ++ concat ("," `intersperse` map show array) ++ "]"

    show (Function { objPropMap = propMap, funcName = name, funcParam = params, funcBody = body }) =
        "<Function " ++ (if null name then "" else name ++ " ") ++ show params ++ " " ++ show body ++
            " {" ++ showMap propMap ++ "}>"
        where showMap mapData =
                  concat $ ", " `intersperse` map showPair (assocs mapData)
              showPair (k, v) =
                  k ++ ": " ++ show v

    show (Object { objPropMap = propMap,
                   objPrototype = prototype,
                   objClass = klass,
                   objValue = value,
                   objName = name }) =
        "<Object" ++ (if null name then "" else " " ++ show name) ++
            " {" ++ showMap propMap ++ "}" ++
            " #prototype=" ++ showShallow prototype ++
            " #class=" ++ show klass ++
            " #value=" ++ show value ++ ">"
        where showMap mapData =
                  concat $ ", " `intersperse` map showPair (assocs mapData)
              showPair (k, v) =
                  k ++ ": " ++ show v

    show (Exception e) = show e

    show (NativeFunction { funcName = name }) = "<NativeFunction " ++ name ++ ">"

    show (Reference baseRef p) = "<Reference " ++ show baseRef ++ " " ++ p ++ ">"

    show (Ref refObj) = "<Ref " ++ (show $ unsafePerformIO $ readIORef refObj) ++ ">"

    show Void = "<Void>"

showShallow :: Value -> String
showShallow (Function { }) = "<Function ...>"

showShallow (Object { objName = "" })   = "<Object ...>"
showShallow (Object { objName = name }) = "<Object " ++ name ++ ">"

showShallow (Ref refObj)   = "<Ref " ++ (showShallow $ unsafePerformIO $ readIORef $ refObj) ++ ">"
showShallow x = show x

type NativeCode
    = [Value] -> Evaluate Value

instance Eq (a -> b) where
    (/=) _ _ = True

data Number
    = Integer Integer
    | Double Double
    | NaN
    deriving (Show, Eq)

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

data Exception
    = ReferenceError String
    | TypeError String
    | SyntaxError String
{-
    内部でのみ使用
-}
    | NotImplemented String
    | InternalError String
    | SysExit
    deriving Eq

instance Show Exception where
    show (TypeError e)             = "TypeError: " ++ e
    show (ReferenceError msg)      = "ReferenceError: " ++ msg
    show (NotImplemented message) = "*** not implemented: " ++ message

data ContType
    = CReturn
    | CThrow
    | CBreak { ctLabel :: Maybe String }
    | CContinue { ctLabel :: Maybe String }
    deriving (Eq, Show)

instance Show (a -> b) where
    show _ = ""

instance Show a => Show (IORef a) where
    show x = "IORef " ++ (show $ unsafePerformIO $ readIORef x)

makeRef :: Value -> Evaluate Value
makeRef ref@(Ref _) = return ref

makeRef object =
    do ref <- liftAll $ newIORef object
       return $ Ref ref

makeIORef :: Value -> Evaluate (IORef Value)
makeIORef (Ref ref) = return ref

makeIORef object =
    do ref <- liftAll $ newIORef object
       return $ ref

readRef :: Value -> Evaluate Value
readRef (Ref objRef) =
    do object <- liftAll $ readIORef objRef
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
        objName       = ""
    }

nullFunction :: Value
nullFunction = Function {
        funcName      = "",
        funcParam     = [],
        funcBody      = STEmpty,
        funcScope     = [],
        funcConstruct = Nothing,
        objPropMap    = Map.empty
    }

nullNativeFunc :: Value
nullNativeFunc = NativeFunction {
        funcName      = "",
        funcArity     = 0,
        funcNatCode   = undefined,
        funcConstruct = Nothing,
        objPropMap    = Map.empty
    }

nativeFunc :: String -> Int -> NativeCode -> Value
nativeFunc name arity code =
    nullNativeFunc {
        funcName    = name,
        funcArity   = arity,
        funcNatCode = code
    }

-- 組み込みオブジェクトを作るヘルパー
nativeFuncPropMap :: [(String, NativeCode, Int)] -> Map String PropertyPair
nativeFuncPropMap =
    mkPropMap . map convert
    where convert (name, code, arity) =
              (name, nativeFunc name arity code, [])

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

isArray :: Value -> Bool
isArray (Array _) = True
isArray _ = False

isFunction :: Value -> Bool
isFunction (Function { }) = True
isFunction _ = False

isObject :: Value -> Bool
isObject (Object { }) = True
isObject _ = False

isException :: Value -> Bool
isException (Exception _) = True
isException _ = False

isVoid :: Value -> Bool
isVoid Void = True
isVoid _ = False

isNativeFunction :: Value -> Bool
isNativeFunction (NativeFunction { }) = True
isNativeFunction _ = False

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

{-
    DataTypes.hs
    Haskell内部の型
    http://developer.mozilla.org/es4/spec/chapter_4_fundamental_concepts.html#abstract_data
-}

module DataTypes where
import Data.IORef
import System.IO.Unsafe
import Control.Monad.State
import Control.Monad.Cont hiding(Cont)

class JSConvertible a where
    fromValue :: Value -> a

class Eval a where
    eval :: a -> Evaluate Value

type Evaluate a
    = ContT Value (StateT Env IO) a

liftAll :: IO a -> Evaluate a
liftAll = lift . lift

data Env
    = Env { envFrames :: [Frame], envContStack :: [Cont] }
    deriving Show

data Frame
    = GlobalFrame { frBinding :: Binding, frThis :: Value }
    | Class
    | Instance
    | WithFrame { frWithObj :: Value }
    | Activation { frBinding :: Binding, frThis :: Value }
    deriving Show

type Binding
    = IORef [(String, IORef Value)]

type Cont
    = (ContType, Value -> Evaluate Value)

type JavaScriptProgram =
    -- ([PackageDefinition], [Directive])
    [Statement]

type VariableBinding
    = ((String, Maybe Expression), Maybe Expression)
--   (identifier, type)

data Statement
    = STVariableDefinition { varDefIsConst :: Bool, varDefBindings :: [VariableBinding] }
    | STFunctionDefinition { funcDefType :: Maybe GetterOrSetter, funcDefFunc :: Value }
    | STEmpty
    | STExpression Expression
--  | STSuper
    | STBlock [Statement]
--  | STLabeled
    | STIf Expression Statement (Maybe Statement)
--  | STSwitch 
--  | STDo
    | STWhile Expression Statement
    | STFor { forInitialize :: Statement, forCondition :: Expression, forUpdate :: Expression, forBlock :: Statement }
    | STForIn { forBinding :: Statement, forObject :: Expression, forBlock :: Statement }
--  | STWith
    | STContinue (Maybe String)
    | STBreak (Maybe String)
    | STReturn (Maybe Expression)
--  | STThrow
    | STTry { tryClause :: Statement, tryCatchClauses :: [(Parameter, Statement)], tryFinallyClause :: Maybe Statement }
    deriving Show

data Expression
    = Keyword String
    | Punctuator String
    | Identifier String
    | QualifiedIdentifier (Expression, String)
    | Literal Value
    | ArrayLiteral [Expression]
    | ObjectLiteral [(Expression, Expression)]
    | List [Expression]
    | LineBreak
    | EndOfInput
    | Operator { opOperator :: String, opArgs :: [Expression] }
    | Let { letLeft :: Expression, letRight :: Expression }
    deriving Show

data Value
    = Undefined
    | Null
    | Boolean Bool
    | Number Number
    | String String
    | Array [Value]
    | Function {
        funcName :: Maybe String,
        funcParam :: Parameters,
        funcBody :: Statement
      }
    | Object {
        properties :: Map String Value,
        attributes :: Map String [PropertyAttribute],

        --  内部プロパティ
        delegate :: Value,
        prototype :: Value,
        className :: String,
        construct :: Value
      }
    | Exception { exceptionBody :: Exception }
--  | RegularExpression
{-
    内部でのみ使用
-}
    | Void
    | NativeFunction NativeFunction
    | Reference (IORef Value, String)
    | Ref { objRef :: IORef Value }
    deriving Show

type Namespace
    = String

type NativeFunction
    = ([Value] -> Evaluate Value)

type Map a b
    = [(a, b)]

data Number
    = Integer Integer
    | Double Double
    | NaN
    deriving (Show, Eq)

data PropertyAttribute
    = ReadOnly
    | DontEnum
    | DontDelete
--  | Internal
    deriving (Show, Eq)

type Parameters =
    ([Parameter], Maybe RestParameter)

data Parameter
    = NonConst { varName :: String }
    | Const { varName :: String }
    deriving Show

data GetterOrSetter
    = Getter
    | Setter
    deriving Show

type RestParameter =
    Maybe Parameter

data Exception
    = TypeError String
    | ReferenceError String
    | NotDefined String
    | InvalidAssignmentLeftSide

instance Show Exception where
    show (TypeError e) = "TypeError: " ++ e
    show (ReferenceError id) = id ++ " is not defined"
    show (NotDefined id) = id ++ " is not defined"
    show InvalidAssignmentLeftSide = "invalid assignment left-hand side"

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

instance JSConvertible Bool where
    fromValue Undefined = False

    fromValue Null = False

    fromValue (Boolean bool) = bool

    fromValue (Number (Integer n)) = n /= 0
    fromValue (Number (Double n))  = n /= 0
    fromValue (Number NaN) = False

    fromValue (String "") = False

    fromValue _ = True

nullObject :: Value
nullObject = Object {
        properties = [],
        attributes = [],
        delegate   = Null,
        prototype  = Null,
        className  = "Object",
        construct  = Null
    }

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
isNativeFunction (NativeFunction _) = True
isNativeFunction _ = False

isReference :: Value -> Bool
isReference (Reference _) = True
isReference _ = False

isRef :: Value -> Bool
isRef (Ref { }) = True
isRef _ = False

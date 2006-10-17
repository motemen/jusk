{-
    DataTypes.hs
    Haskell内部の型
-}

module DataTypes where
import Data.Map (Map)
import qualified Data.Map as Map
import Data.IORef
import System.IO.Unsafe
import Control.Monad.State
import Control.Monad.Cont hiding(Cont)

data Flag
    = Debug
    | Warn
    | ParseOnly
    | InputFile String
    | EvalStr String
    deriving (Show, Eq)

class Eval a where
    eval :: a -> Evaluate Value

type Evaluate a
    = ContT Value (StateT Env IO) a

liftAll :: IO a -> Evaluate a
liftAll = lift . lift

data Env
    = Env { envFrames :: [Frame], envContStack :: [Cont], envFlags :: [Flag] }
    deriving Show

data Frame
    = GlobalFrame { frBinding :: Binding, frThis :: Value }
    | Activation { frBinding :: Binding, frThis :: Value }
    | WithFrame { frWithObjRef :: IORef Value }
    deriving Show

type Binding
    = IORef [(String, Value)]

type Cont
    = (ContType, Value -> Evaluate Value)

type JavaScriptProgram =
    [Statement]

type VariableBinding
    = (String, Maybe Expression)

data Statement
    = STVariableDefinition { varDefBindings :: [VariableBinding] }
    | STFunctionDefinition { funcDefFunc :: Value }
    | STEmpty
    | STExpression Expression
    | STBlock [Statement]
    | STLabelled String Statement
    | STIf Expression Statement (Maybe Statement)
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
--      funcScope :: [Frame]
      }
    | Object {
        objPropMap :: Map String PropertyPair,

        --  内部プロパティ
        objPrototype :: Value,  -- [[Prototype]]
        objClass :: String,     -- [[Class]]
        objDefault :: Value,    -- [[DefaultValue]]
        objConstruct :: Value   -- [[Construct]]
      }
    | Exception { exceptionBody :: Exception }
--  | RegularExpression
{-
    内部でのみ使用
-}
    | NativeFunction NativeFunction
    | Reference (IORef Value, String)
    | Ref { getRef :: IORef Value }
    | Void
    deriving Show

type NativeFunction
    = ([Value] -> Evaluate Value)

data Number
    = Integer Integer
    | Double Double
    | NaN
    deriving (Show, Eq)

data PropertyPair
    = PropertyPair { propValue :: Value, propAttr :: [PropertyAttribute] }
    deriving Show

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
    = TypeError String
    | ReferenceError String
    | NotDefined String
    | InvalidAssignmentLeftSide
{-
    内部でのみ使用
-}
    | NotImplemented String
    | SysExit

instance Show Exception where
    show (TypeError e)             = "TypeError: " ++ e
    show (ReferenceError msg)      = "ReferenceError: " ++ msg
    show (NotDefined id)           = id ++ " is not defined"
    show InvalidAssignmentLeftSide = "invalid assignment left-hand side"

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
        objDefault    = Null,
        objPrototype  = Null,
        objClass      = "Object",
        objConstruct  = Null
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

isPrimitive :: Value -> Bool
isPrimitive Undefined   = True
isPrimitive Null        = True
isPrimitive (Boolean _) = True
isPrimitive (Number _)  = True
isPrimitive (String _)  = True
isPrimitive _           = False

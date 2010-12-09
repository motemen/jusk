{-# OPTIONS_GHC -fglasgow-exts #-}
{-
    DataTypes.hs
    Haskell内部の型
-}

module DataTypes (module DataTypes, liftIO) where
import Data.Map (Map, assocs)
import qualified Data.Map as Map
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad.Trans (liftIO)
import Control.Monad.State
import Control.Monad.Cont hiding (Cont)
import Text.Regex
import Data.List
import Data.Maybe

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
        (unlines $ map ("    " ++) $ map show frames) ++
        "  Cont:\n" ++
        (unlines $ map ("    " ++) $ map show conts) ++
        "  Flags:\n    " ++
        (unwords $ map show flags)

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
    deriving (Eq)

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
    deriving (Eq)

{- TODO
data Operator
    = UnaryOp UnaryOperator Expression
    | BinaryOp BinaryOperator (Expression, Expression)
    | TernaryOp (Expression, Expression, Expression)
    | MultiaryOp MultiaryOperator [Expression]

data UnaryOperator
    = OpTypeof              -- typeof
    | OpUnaryPlus           -- +
    | OpUnaryMinus          -- -
    | OpBitwiseNot          -- ~
    | OpLogicalNot          -- !

data BinaryOperator
    = OpMultiplication      -- *
    | OpDivision            -- /
    | OpRemainder           -- %
    | OpBinaryPlus          -- +
    | OpBinaryMinus         -- -
    | OpSignedRightShift    -- >>
    | OpBitwiseLeftShift    -- <<
    | OpUnsignedRightShift  -- >>>
    | OpLessThan            -- <
    | OpGreaterThan         -- >
    | OpOpLessThanEq        -- <=
    | OpGreaterThanEq       -- >=
    | OpInstanceof          -- instanceof
    | OpIn                  -- in
    | OpEqual               -- ==
    | OpNotEqual            -- !=
    | OpStrictlyEqual       -- ===
    | OpStrictlyNotEqual    -- !==
    | OpBitwiseAnd          -- &
    | OpBitwiseOr           -- |
    | OpBitwiseXor          -- ^
    | OpBracket             -- []
-}

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

        objObject    :: NativeObject,
        objGetter    :: Value
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
    | ULObject

instance Eq NativeObject where
    (==) (Function p s f) (Function p' s' f') = p == p' && s == s' && f == f'
    (==) (NativeFunction { }) (NativeFunction { }) = False
    (==) (RegExp _ p f) (RegExp _ p' f') = p == p' && f == f'
    (==) SimpleObject SimpleObject = True
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
    (Double n)  == (Double m)  = n == m
    (Integer n) == (Double m)  = (toEnum $ fromEnum n) == m
    (Double n)  == (Integer m) = n == (toEnum $ fromEnum m)

data PropertyPair
    = PropertyPair { propValue :: Value, propAttr :: [PropertyAttribute] }
    deriving Eq

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

-- instance Show (a -> b) where
--     show _ = ""

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
        objObject     = SimpleObject,
        objGetter     = Null
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
        funcNatCode   = error "nullNativeFunc: undefined funcNatCode"
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

-- 型の判別
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

isLiteral :: Expression -> Bool
isLiteral (Literal _) = True
isLiteral _           = False

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
typeString Void         = "void"

getName :: Value -> String
getName o | isPrimitive o = show o
getName Object { objName = name } = name
getName (Reference base name) = getName base ++ "." ++ name
getName (Ref ref) = getName $ unsafePerformIO $ readIORef ref
getName _ = ""

toDouble :: Number -> Double
toDouble (Integer n) = fromIntegral n
toDouble (Double n)  = n
toDouble NaN = error "toDouble: NaN"

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

-- Showing
joinBy :: [String] -> String -> String
joinBy strings sep = concat $ intersperse sep strings

indent :: String -> String
indent = unlines . (map ("    " ++)) . lines

instance Show Statement where
    show (STVarDef bindings) =
        "var " ++ map showVarBinding bindings `joinBy` "," ++ ";"

    show (STFuncDef name func) =
        show $ nullObject { objName = name, objObject = func }
    
    show (STEmpty) = ";"

    show (STExpression expr) = show expr ++ ";"

    show (STBlock statements) = "{\n" ++ indent (unlines $ map show statements) ++ "}"

    show (STLabelled label st) = label ++ ":\n" ++ show st

    show (STIf cond thenSt Nothing) =
        "if (" ++ show cond ++ ") " ++ show thenSt
    show (STIf cond thenSt (Just elseSt)) =
        "if (" ++ show cond ++ ") " ++ show thenSt ++ " else " ++ show elseSt
    
    show (STSwitch cond clauses) =
        "switch (" ++ show cond ++ ") {\n" ++ indent (unlines $ map showSwitchClause clauses) ++ "\n}"

    show (STDoWhile cond st) =
        "do " ++ show st ++ "} while (" ++ show cond ++ ")"

    show (STWhile cond st) =
        "while (" ++ show cond ++ ") " ++ show st

    show (STFor init cond upd block) =
        "for (" ++ show init ++ " " ++ show cond ++ "; " ++  show upd ++ ") " ++ show block

    show (STForIn binding object block) =
        "for (" ++ show binding ++ " in " ++ show object ++ ") " ++ show block

    show (STWith object block) =
        "with (" ++ show object ++ ") " ++ show block

    show (STContinue label) =
        "continue" ++ maybe "" (" " ++) label ++ ";"

    show (STBreak label) =
        "break" ++ maybe "" (" " ++) label ++ ";"

    show (STReturn expr) =
        "return" ++ maybe "" ((" " ++) . show) expr ++ ";"

    show (STThrow expr) =
        "throw " ++ show expr

    show (STTry try catch finally) =
        "try " ++ show try ++ maybe "" (\(p, s) -> " catch (" ++ p ++ ") " ++ show s) catch ++ maybe "" ((" finally " ++) . show) finally

showVarBinding (name, Nothing)   = name
showVarBinding (name, Just expr) = name ++ " = " ++ show expr

showSwitchClause (Nothing, st) = "default: " ++ show st
showSwitchClause (Just l, st)  = "case " ++ show l ++ ": " ++ show st

instance Show Expression where
    showsPrec _ (Keyword keyword) = showString keyword
    showsPrec _ (Punctuator op)   = showString op
    showsPrec _ (Identifier name) = showString name

    showsPrec _ (Literal value)   = shows value

    showsPrec _ (ArrayLiteral exprs)
        = showString $ "[" ++ map show exprs `joinBy` "," ++ "]"

    showsPrec _ (ObjectLiteral pairs)
        = showString $ "{" ++ map showObjPair pairs `joinBy` "," ++ "}"

    showsPrec _ (RegExpLiteral pattern flags)
        = showString $ "/" ++ pattern ++ "/" ++ flags

    showsPrec _ (List exprs)
        = showString $ map show exprs `joinBy` ","

    showsPrec _ (Operator "[]" [num@(Literal (Number _)), Literal (String p)])
        = showString "("
        . shows num
        . showString ")"
        . showString "."
        . showString p

    showsPrec _ (Operator "[]" [obj, Literal (String p)])
        = showsPrec (length opPrecedence) obj
        . showString "."
        . showString p

    showsPrec _ (Operator "[]" [obj, p])
        = showsPrec (length opPrecedence) obj
        . showString "["
        . shows p
        . showString "]"

    showsPrec _ (Operator "()" (c:args))
        = shows c
        . showParen True
                    (if null args
                        then id
                        else foldl1 (\x y -> x . showString "," . y) $ map shows args)

    showsPrec _ (Operator "new" (c:args))
        = showString "new "
        . shows c
        . (if null args
              then id
              else showParen True
                             (foldl1 (\x y -> x . showString "," . y) $ map shows args))

    showsPrec p (Operator op@"?:" [x, y, z])
        = showParen (prec < p)
        $ showsPrec prec x
        . showString " ? "
        . showsPrec prec y
        . showString " : "
        . showsPrec prec z
        where prec = fromMaybe (length opPrecedence) (findIndex (op `elem`) opPrecedence)

    showsPrec p (Operator op [x, y])
        = showParen (prec < p)
        $ showsPrec prec x
        . showString " "
        . showString op
        . showString " "
        . showsPrec prec y
        where prec = fromMaybe (length opPrecedence) (findIndex (op `elem`) opPrecedence)

    showsPrec p (Operator op [x])
        = showParen (prec < p)
        $ showString op
        . showString " "
        . showsPrec prec x
        where prec = fromMaybe (length opPrecedence) (findIndex (op `elem`) opPrecedence)

    showsPrec _ (Operator _ _)
        = error "showPrec: unknown operator"

    {-
    showsPrec p (Operator op xs)
        = showParen (prec < p)
        $ showString op
        . showString " "
        . showList xs
        where prec = fromMaybe (length opPrecedence) (findIndex (op `elem`) opPrecedence)
    -}

    showsPrec _ (Let left right) = showString $ show left ++ " = " ++ show right

opPrecedence :: [[String]]
opPrecedence = reverse [
        ["[]", "new"],
        ["()"],
        ["_++", "_--"],
        ["delete", "void", "typeof", "++", "--", "+", "-", "~", "!"],
        ["*", "/", "%"],
        ["+", "-"],
        ["<<", ">>", ">>>"],
        ["<", ">", "<=", ">=", "instanceof", "in"],
        ["==", "!=", "===", "!=="],
        ["&"],
        ["^"],
        ["|"],
        ["&&"],
        ["||"],
        ["?:"]
    ]

showObjPair (p, v) = show p ++ ": " ++ show v

instance Show Value where
    show Undefined = "undefined"
    show Null      = "null"

    show (Boolean True)  = "true"
    show (Boolean False) = "false"

    show (Number (Integer n)) = show n
    show (Number (Double n))  = show n
    show (Number NaN)         = "NaN"

    show (String string) = show string

    show (Object { objObject = RegExp { regexpPattern = pattern, regexpFlags = flags } }) =
        "/" ++ pattern ++ "/" ++ flags

    show (Object { objName = name, objObject = Function { funcParam = params, funcBody = body } }) =
        "function" ++ (if null name then "" else " " ++ name) ++ "(" ++ params `joinBy` "," ++ ") " ++ show body

    show _ =
        ""

-- Inspecting
class Inspect a where
    inspect :: a -> String

instance Inspect Char where
    inspect ch = [ch]

instance Inspect a => Inspect [a] where
    inspect xs = show $ map inspect xs

instance Inspect a => Inspect (Maybe a) where
    inspect (Just a) = "Just " ++ inspect a
    inspect Nothing  = "Nothing"

instance (Inspect a, Inspect b) => Inspect (a, b) where
    inspect (a, b) = "(" ++ inspect a ++ "," ++ inspect b ++ ")"

instance Inspect PropertyPair where
    inspect (PropertyPair { propValue = value, propAttr = [] }) =
        inspectShallow value

    inspect (PropertyPair { propValue = value, propAttr = attrs }) =
        inspectShallow value ++ "(" ++ (map show attrs `joinBy` ",") ++ ")"

instance Inspect Value where
    inspect Undefined = "undefined"
    inspect Null      = "null"

    inspect (Boolean True)  = "true"
    inspect (Boolean False) = "false"

    inspect (Number (Integer n)) = show n
    inspect (Number (Double n))  = show n
    inspect (Number NaN)         = "NaN"

    inspect (String string) = show string

    inspect (Object { objPropMap = propMap,
                      objPrototype = prototype,
                      objClass = klass,
                      objValue = value,
                      objName = name,
                      objObject = obj,
                      objGetter = getter }) =
        "<Object" ++ (if null name then "" else " " ++ name) ++
            " {" ++ showMap propMap ++ "}" ++
            " #prototype=" ++ inspectShallow prototype ++
            " #class=" ++ klass ++
            " #value=" ++ inspect value ++
            " #object=" ++ inspect obj ++
            " #getter=" ++ inspectShallow getter ++ ">"
        where showMap mapData =
                  map showPair (assocs mapData) `joinBy` ","
              showPair (k, v) =
                  k ++ ": " ++ inspect v

    inspect (Reference baseRef p) = "<Reference " ++ inspect baseRef ++ " " ++ p ++ ">"

    inspect (Ref refObj) = "<Ref " ++ (inspect $ unsafePerformIO $ readIORef refObj) ++ ">"

    inspect Void = "<Void>"

instance Inspect NativeObject where
    inspect SimpleObject = ""

    inspect (Function { funcParam = params, funcBody = body }) =
        "<Function" ++ (concat $ map (' ':) params) ++ " " ++ inspect body ++ ">"

    inspect (NativeFunction { }) = "<NativeFunction>"

    inspect (RegExp { regexpPattern = pattern }) = "<RegExp " ++ pattern ++ ">"

    inspect (Array _) = "<Array>"

    inspect ULObject = "<UL-Object>"

instance Inspect Expression where
    inspect (Keyword keyword)   = "Keyword " ++ keyword

    inspect (Punctuator punc)   = "Punctuator " ++ punc

    inspect (Identifier ident)  = "Identifier " ++ ident

    inspect (Literal value)     = "Literal " ++ inspect value

    inspect (ArrayLiteral exprs)
        = "ArrayLiteral [" ++ (map inspect exprs `joinBy` ",") ++ "]"

    inspect (ObjectLiteral pairs)
        = "ObjectLiteral " ++ show pairs

    inspect (RegExpLiteral pat flags)
        = "RegExpLiteral " ++ pat ++ " " ++ flags

    inspect (List exprs)        = "List [" ++ (map inspect exprs `joinBy` ",") ++ "]"

    inspect (Operator op args)  = "Operator " ++ op ++ " [" ++ (map inspect args `joinBy` ",") ++ "]"

    inspect (Let left right)    = "Let " ++ inspect left ++ " " ++ inspect right

instance Inspect Statement where
    inspect (STVarDef { varDefBindings = bindings })
        = "STVarDef " ++ show bindings

    inspect (STFuncDef { funcDefName = name, funcDefFunc = func })
        = "STFuncDef " ++ name ++ "\n" ++ inspect func

    inspect STEmpty                 = "STEmpty"

    inspect (STExpression expr)     = "STExpression " ++ inspect expr

    inspect (STBlock sts)           = "STBlock " ++ show sts

    inspect (STLabelled label st)   = "STLabelled " ++ label ++ " " ++ inspect st

    inspect (STIf { ifCond = expr, ifThen = thenSt, ifElse = elseSt })
        = "STIf " ++ inspect expr ++ "\n" ++ inspect thenSt ++ maybe "" (("\n" ++) . inspect) elseSt

    inspect (STSwitch { swExpression = expr, swClauses = clauses })
        = "STSwitch " ++ inspect expr ++ inspect clauses

    inspect (STDoWhile expr st)     = "STDoWhile " ++ inspect expr ++ inspect st

    inspect (STWhile expr st)       = "STWhile " ++ inspect expr ++ inspect st

    inspect (STFor { forInitialize = init, forCondition = cond, forUpdate = update, forBlock = block })
        = "STFor " ++ inspect init ++ inspect cond ++ inspect update ++ inspect block

    inspect (STForIn { forBinding = binding, forObject = obj, forBlock = block })
        = "STForIn " ++ inspect binding ++ inspect obj ++ inspect block

    inspect (STWith { withExpression = expr, withBlock = block })
        = "STWith " ++ inspect expr ++ inspect block

    inspect (STContinue label)
        = "STContinue" ++ fromMaybe "" label

    inspect (STBreak label)
        = "STBreak" ++ fromMaybe "" label

    inspect (STReturn expr)
        = "STReturn" ++ maybe "" inspect expr

    inspect (STThrow expr)
        = "STThrow " ++ inspect expr

    inspect (STTry { tryClause = clause, tryCatchClause = catch, tryFinallyClause = finally })
        = "STTry " ++ inspect clause ++ " " ++ inspect catch ++ " " ++ inspect finally

inspectShallow :: Value -> String
inspectShallow (Object { objName = "" })   = "<Object ...>"
inspectShallow (Object { objName = name }) = name

inspectShallow (Ref refObj)   = "<Ref " ++ (inspectShallow $ unsafePerformIO $ readIORef $ refObj) ++ ">"
inspectShallow x = show x

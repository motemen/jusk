{-
    PrettyShow.hs
    ‚«‚ê‚¢‚È\•¶•\Ž¦
-}

module PrettyShow where
import List

import DataTypes

joinBy :: [String] -> String -> String
joinBy strings sep = concat $ intersperse sep strings

indent :: String -> String
indent = unlines . (map ("    " ++)) . lines

class PrettyShow a where
    prettyShow :: a -> String

instance PrettyShow Statement where
    prettyShow (STVarDef bindings) =
        "var " ++ map showVarBinding bindings `joinBy` "," ++ ";"

    prettyShow (STFuncDef name func) =
        prettyShow $ nullObject { objName = name, objObject = func }
    
    prettyShow (STEmpty) = ";"

    prettyShow (STExpression expr) = prettyShow expr ++ ";"

    prettyShow (STBlock statements) = "{\n" ++ indent (unlines (map prettyShow statements)) ++ "}"

    prettyShow (STLabelled label st) = label ++ ":\n" ++ prettyShow st

    prettyShow (STIf cond thenSt Nothing) =
        "if (" ++ prettyShow cond ++ ") " ++ prettyShow thenSt
    prettyShow (STIf cond thenSt (Just elseSt)) =
        "if (" ++ prettyShow cond ++ ") " ++ prettyShow thenSt ++ " else " ++ prettyShow elseSt
    
    prettyShow (STSwitch cond clauses) =
        "switch (" ++ prettyShow cond ++ ") {\n" ++ indent (unlines (map showSwitchClause clauses)) ++ "\n}"

    prettyShow (STDoWhile cond st) =
        "do " ++ prettyShow st ++ "} while (" ++ prettyShow cond ++ ")"

    prettyShow (STWhile cond st) =
        "while (" ++ prettyShow cond ++ ") " ++ prettyShow st

    prettyShow (STFor init cond upd block) =
        "for (" ++ prettyShow init ++ " " ++ prettyShow cond ++ "; " ++  prettyShow upd ++ ") " ++ prettyShow block

    prettyShow (STForIn binding object block) =
        "for (" ++ prettyShow binding ++ " in " ++ prettyShow object ++ ") " ++ prettyShow block

    prettyShow (STWith object block) =
        "with (" ++ prettyShow object ++ ") " ++ prettyShow block

    prettyShow (STContinue label) =
        "continue" ++ maybe "" (" " ++) label ++ ";"

    prettyShow (STBreak label) =
        "break" ++ maybe "" (" " ++) label ++ ";"

    prettyShow (STReturn expr) =
        "return" ++ maybe "" ((" " ++) . prettyShow) expr ++ ";"

    prettyShow (STThrow expr) =
        "throw " ++ prettyShow expr

    prettyShow (STTry try catch finally) =
        "try " ++ prettyShow try ++ maybe "" (\(p, s) -> " catch (" ++ p ++ ") " ++ prettyShow s) catch ++ maybe "" ((" finally " ++) . prettyShow) finally

showVarBinding (name, Nothing)   = name
showVarBinding (name, Just expr) = name ++ " = " ++ prettyShow expr

showSwitchClause (Nothing, st) = "default: " ++ prettyShow st
showSwitchClause (Just l, st)  = "case " ++ prettyShow l ++ ": " ++ prettyShow st

instance PrettyShow Expression where
    prettyShow (Keyword keyword) = keyword
    prettyShow (Punctuator op)   = op
    prettyShow (Identifier name) = name

    prettyShow (Literal value)   = prettyShow value

    prettyShow (ArrayLiteral exprs) = "[" ++ map prettyShow exprs `joinBy` "," ++ "]"

    prettyShow (ObjectLiteral pairs) = "{" ++ map showObjPair pairs `joinBy` "," ++ "}"

    prettyShow (RegExpLiteral pattern flags) = "/" ++ pattern ++ "/" ++ flags

    prettyShow (List exprs) = map prettyShow exprs `joinBy` ","

    prettyShow (Operator "()" (callee:args)) = prettyShow callee ++ "(" ++ map prettyShow args `joinBy` "," ++ ")"

    prettyShow (Operator "[]" [obj, Literal (String p)]) =
        prettyShow obj ++ "." ++ p
    prettyShow (Operator "[]" [obj, p]) =
        prettyShow obj ++ "[" ++ prettyShow p ++ "]"

    prettyShow (Operator "?:" [x, y, z])     = prettyShow x ++ " ? " ++ prettyShow y ++ " : " ++ prettyShow z

    prettyShow (Operator "new" (c:args))     = "new " ++ prettyShow c ++ "(" ++ map prettyShow args `joinBy` "," ++ ")"
    prettyShow (Operator "typeof" [o])       = "typeof " ++ prettyShow o
    prettyShow (Operator op [x, y])          = prettyShow x ++ " " ++ op ++ " " ++ prettyShow y
    prettyShow (Operator ('_':op) [x])       = prettyShow x ++ op
    prettyShow (Operator op [x])             = op ++ prettyShow x

    prettyShow (Let left right)              = prettyShow left ++ " = " ++ prettyShow right

showObjPair (p, v) = prettyShow p ++ ": " ++ prettyShow v

instance PrettyShow Value where
    prettyShow Undefined = "undefined"
    prettyShow Null      = "null"

    prettyShow (Boolean True)  = "true"
    prettyShow (Boolean False) = "false"

    prettyShow (Number (Integer n)) = show n
    prettyShow (Number (Double n))  = show n
    prettyShow (Number NaN)         = "NaN"

    prettyShow (String string) = show string

    prettyShow (Object { objObject = RegExp { regexpPattern = pattern, regexpFlags = flags } }) =
        "/" ++ pattern ++ "/" ++ flags

    prettyShow (Object { objName = name, objObject = Function { funcParam = params, funcBody = body } }) =
        "function" ++ (if null name then "" else " " ++ name) ++ "(" ++ params `joinBy` "," ++ ") " ++ prettyShow body


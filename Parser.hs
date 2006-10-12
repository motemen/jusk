{-
    Parser.hs
    構文パーザ
    http://www.mozilla.org/js/language/js20/formal/parser-grammar.html
-}

-- Module definition {{{
module Parser where
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as T
import Data.Char (isDigit,digitToInt)
import Monad
import List

import DataTypes

data ParserParameter
    = Abbrev
    | Full
    | NoShortIf
    | AllowIn
    | NoIn
    deriving Show
    
runLex :: Parser a -> String -> Either ParseError a
runLex p input = parse (do { whiteSpace; x <- p; eof; return x }) "" input
-- }}}

-- Lexer {{{
lexer :: T.TokenParser ()
lexer = T.makeTokenParser
        $ javaStyle {
            identStart = letter <|> oneOf "$_",
            identLetter = alphaNum <|> oneOf "$_",
            reservedNames = [
                "as", "break", "case", "catch", "class", "const",
                "continue", "default", "delete", "do", "else",
                "export", "extends", "false", "finally", "for",
                "function", "if", "import", "in", "instanceof",
                "is", "namespace", "new", "null", "package",
                "private", "public", "return", "super", "switch",
                "this", "throw", "true", "try", "typeof", "use",
                "var", "void", "while", "with",
                "abstract", "debugger", "enum", "goto",
                "implements", "interface", "native", "protected",
                "synchronized", "throws", "transient", "volatile"
            ],
            reservedOpNames = [
                "!", "!=", "!==", "%", "%=", "&", "&&", "&&=",
                "&=", "(", ")", "*", "*=", "+", "++", "+=",
                ",", "-", "--", "-=", ".", "...", "/", "/=",
                ":", "::", ";", "<", "<<", "<<=", "<=", "=",
                "==", "===", ">", ">=", ">>", ">>=", ">>>",
                ">>>=", "?", "[", "]", "^", "^=", "^^", "^^=",
                "{", "|", "|=", "||", "||=", "}", "~"
            ],
            caseSensitive = True
        }

lexeme         = T.lexeme lexer
whiteSpace     = T.whiteSpace lexer
symbol         = T.symbol lexer
natural        = T.natural lexer
float          = T.float lexer
naturalOrFloat = T.naturalOrFloat lexer
parens         = T.parens lexer             -- ()
braces         = T.braces lexer             -- {}
squares        = T.squares lexer            -- []
colon          = T.colon lexer              -- :
semi           = T.semi lexer               -- ;
comma          = T.comma lexer              -- ,
reserved       = T.reserved lexer           
reservedOp     = T.reservedOp lexer
hexadecimal    = T.hexadecimal lexer
decimal        = T.decimal lexer
identifierString = T.identifier lexer
-- }}}

-- Utilities {{{
keyword :: String -> Parser Expression
keyword name =
    do reserved name
       return $ Keyword name

operator :: String -> Parser Expression
operator op =
    do reservedOp op
       return $ Punctuator op

followedBy :: Show tok => GenParser tok st a -> GenParser tok st b -> GenParser tok st a
p `followedBy` q = do { x <- p; q; return x }

times :: Integer -> GenParser tok st a -> GenParser tok st [a]
times n p = foldl (\xs _ -> do { ys <- xs; y <- p; return $ y:ys }) (return []) [1..n]

ifFail :: GenParser tok st a -> a -> GenParser tok st a
p `ifFail` x = option x p

pushArg :: Expression -> Expression -> Expression
pushArg expr (Operator op args) = Operator op (expr:args)
-- }}}

-- Expressions {{{
-- http://www.mozilla.org/js/language/js20/core/expressions.html

--- Identifiers
identifier :: Parser Expression
identifier = liftM Identifier identifierString

--- Qualified Identifiers
simpleQualifiedIdentifier :: Parser Expression
simpleQualifiedIdentifier =
    (do n <- reservedNamespace
        reservedOp "::"
        i <- identifierString
        return $ QualifiedIdentifier (n, i))
    <|> (do i <- identifier
            option i (do reservedOp "::"
                         i' <- identifierString
                         return $ QualifiedIdentifier (i, i')))
                               
expressionQualifiedIdentifier :: Parser Expression
expressionQualifiedIdentifier =
    do e <- parenExpression
       reservedOp "::"
       i <- identifierString
       return $ QualifiedIdentifier (e, i)

qualifiedIdentifier :: Parser Expression
qualifiedIdentifier = simpleQualifiedIdentifier
                  <|> expressionQualifiedIdentifier

--- Primary Expressions
--- http://www.mozilla.org/js/language/js20/core/expressions.html#N-PrimaryExpression
primaryExpression :: Parser Expression
primaryExpression = (keyword "null")
                <|> booleanLiteral
                <|> numberLiteral
                <|> stringLiteral
                <|> (keyword "this")
--              <|> regularExpression
                <|> reservedNamespace
                <|> parenListExpression
                <|> arrayLiteral
                <|> objectLiteral
                <|> functionExpression

reservedNamespace :: Parser Expression
reservedNamespace = keyword "public"
                <|> keyword "private"

parenExpression :: Parser Expression
parenExpression = parens (assignmentExpression AllowIn)

parenListExpression :: Parser Expression
parenListExpression = liftM List (parens $ (assignmentExpression AllowIn) `sepBy` comma)

--- Function Expressions
functionExpression :: Parser Expression
functionExpression =
    do reserved "function"
       name <- option Nothing (liftM Just identifierString)
       Function { funcParam = params, funcBody = body } <- functionCommon
       return $ Literal $ Function name params body
    <?> "function expression"

--- Object Literals
objectLiteral :: Parser Expression
objectLiteral = braces (objectLiteralPair `sepBy` comma) >>= return . ObjectLiteral
            <?> "object literal"

objectLiteralPair :: Parser (String, Expression)
objectLiteralPair =
    do name <- identifierString
       colon
       expr <- assignmentExpression AllowIn
       return (name, expr)

--- Array Literals
arrayLiteral :: Parser Expression
arrayLiteral =
    do exprs <- squares (assignmentExpression AllowIn `sepBy` comma)
       return $ ArrayLiteral exprs
    <?> "array"

--- Boolean Literals
booleanLiteral :: Parser Expression
booleanLiteral = (reserved "true"  >> (return $ Literal $ Boolean True))
             <|> (reserved "false" >> (return $ Literal $ Boolean False))
             <?> "boolean"

--- String Literals
-- code from libraries/parsec/Text/ParserCombinators/Parsec/Expression.hs
stringLiteral :: Parser Expression
stringLiteral = liftM (Literal . String)
                      (do str <- (stringLiteralQuotedBy '"' <|> stringLiteralQuotedBy '\'')
                          whiteSpace
                          return $ foldr (maybe id (:)) "" str)
            <?> "string"
              where
                stringLiteralQuotedBy q = (between (char q) (char q) (many $ stringChar q))

stringChar q = (char '\\' >> (controlEscape <|> zeroEscape <|> hexEscape <|> identityEscape))
           <|> (liftM Just $ satisfy (\c -> c /= q))

identityEscape = satisfy (\c -> c /= '_') >>= return . Just
controlEscape  = liftM Just $ choice (map escaped $ zip "bfnrtv" "\b\f\n\r\t\v") -- TODO
               where
                 escaped (c, code) = do { char c; return code }

zeroEscape     = do { char '0'; notFollowedBy (satisfy isDigit); return '0' } >> return Nothing
hexEscape      = do char 'x'
                    digits <- times 2 hexDigit
                    return $ Just $ toEnum $ foldr (\b a -> a * 16 + (digitToInt b)) 0 digits
--           <|> char 'u' >> times 4 hexDigit
--           <|> char 'U' >> times 8 hexDigit

--- Number Literals
numberLiteral :: Parser Expression
numberLiteral =
    do num <- naturalOrFloat
       return $ Literal $ Number
              $ case num of
                     Left n -> Integer n
                     Right n -> Double n

--- Super Expressions
superExpression :: Parser Expression
superExpression =
    do s <- keyword "super"
       option s (do { args <- parenExpression; return $ Operator "super" [args] })

--- Postfix Expressions
-- TODO: new Hoge(a,b)
postfixExpression :: Parser Expression
postfixExpression =
    do e1 <- attributeExpression
         <|> (try $ do e1 <- primaryExpression
                         <|> expressionQualifiedIdentifier
                         <|> fullNewExpression
                         <|> do { e <- superExpression; o <- propertyOperator; return $ pushArg e o }
                       e2 <- many operatorOrArguments
                       return $ foldl pushArg e1 e2)
         <|> shortNewExpression
       e2 <- many $ (do e1 <- ((do { reservedOp "++"; return $ Operator "_++" [] })
                           <|> (do { reservedOp "--"; return $ Operator "_--" [] }))
                        e2 <- many operatorOrArguments
                        return $ e1:e2)
       return $ foldl pushArg e1 (foldl (++) [] e2)
    where operatorOrArguments = propertyOperator <|> arguments

attributeExpression :: Parser Expression
attributeExpression =
    do x <- simpleQualifiedIdentifier
       xs <- many (propertyOperator <|> arguments)
       return $ foldl pushArg x xs

fullNewExpression :: Parser Expression
fullNewExpression =
    do reserved "new"
       e <- fullNewSubexpression
       a <- arguments
       return $ Operator "new" [e, a]

fullNewSubexpression :: Parser Expression
fullNewSubexpression = primaryExpression
                   <|> qualifiedIdentifier
                   <|> fullNewExpression
                   <|> do { e <- fullNewSubexpression; o <- propertyOperator; return $ pushArg e o }
                   <|> do { e <- superExpression;      o <- propertyOperator; return $ pushArg e o }

shortNewExpression :: Parser Expression
shortNewExpression =
    do reserved "new"
       e <- shortNewSubexpression
       return $ Operator "new" [e]

shortNewSubexpression :: Parser Expression
shortNewSubexpression = fullNewSubexpression
                    <|> shortNewExpression

--- Property Operators
propertyOperator :: Parser Expression
propertyOperator = (do reservedOp "."
                       Identifier i <- qualifiedIdentifier
                       return $ Operator "[]" [Literal $ String i])
               <|> brackets

brackets :: Parser Expression
brackets = (try $ do { (reservedOp "[" >> reservedOp "]"); return $ Operator "[]" [] })
       <|> (do expr <- squares (listExpression AllowIn <|> expressionWithRest)
               return $ Operator "[]" [expr])

arguments :: Parser Expression
arguments =
    do List args <- (try $ do { (reservedOp "(" >> reservedOp ")"); return $ List [] })
                <|> (try parenListExpression)
                <|> (parens expressionWithRest)
       return $ Operator "()" args

expressionWithRest :: Parser Expression
expressionWithRest =
    restExpression
    <|> do List list <- listExpression AllowIn
           comma
           rest <- restExpression
           return $ List $ list ++ [rest]

restExpression :: Parser Expression
restExpression =
    do reservedOp "..."
       e <- assignmentExpression AllowIn
       return $ Operator "..." [e]

--- Unary Operators
operatorWithArg1 :: String -> Parser Expression -> Parser Expression
operatorWithArg1 op p =
    do reservedOp op
       arg <- p
       return $ Operator op [arg]

chainOperator :: [String] -> Parser Expression -> Parser Expression
chainOperator ops p =
    do a <- p
       e <- many $ do Punctuator op <- choice $ map operator ops
                      a <- p
                      return $ Operator op [a]
       return $ foldl pushArg a e

unaryExpression :: Parser Expression
unaryExpression = postfixExpression
              <|> operatorWithArg1 "delete" postfixExpression
              <|> operatorWithArg1 "void" unaryExpression
              <|> operatorWithArg1 "typeof" unaryExpression
              <|> operatorWithArg1 "++" postfixExpression
              <|> operatorWithArg1 "--" postfixExpression
              <|> operatorWithArg1 "+" unaryExpression
              <|> operatorWithArg1 "-" unaryExpression
--            <|> operatorWithArg1 "-" negatedMinLong
              <|> operatorWithArg1 "~" unaryExpression
              <|> operatorWithArg1 "!" unaryExpression

--- Multiplicative Operators
multiplicativeExpression = chainOperator ["*", "/", "%"] unaryExpression

--- Additive Operators
additiveExpression = chainOperator ["+", "-"] multiplicativeExpression

--- Bitwise Shift Operators
shiftExpression = chainOperator ["<<", ">>", ">>>"] additiveExpression

--- Relational Operators
relationalExpression AllowIn = chainOperator ["<", ">", "<=", ">=", "is", "as", "in", "instanceof"] shiftExpression
relationalExpression NoIn    = chainOperator ["<", ">", "<=", ">=", "is", "as", "instanceof"] shiftExpression

--- Equality Operators
equalityExpression p = chainOperator ["==", "!=", "===", "!=="] (relationalExpression p)

--- Binary Bitwise Operators
bitwiseAndExpression p = chainOperator ["&"] (equalityExpression p)
bitwiseXorExpression p = chainOperator ["^"] (bitwiseAndExpression p)
bitwiseOrExpression  p = chainOperator ["|"] (bitwiseXorExpression p)

--- Binary Logical Operators
logicalAndExpression p = chainOperator ["&&"] (bitwiseOrExpression p)
logicalXorExpression p = chainOperator ["^^"] (logicalAndExpression p)
logicalOrExpression  p = chainOperator ["||"] (logicalXorExpression p)

--- Conditional Operators
conditionalExpression :: ParserParameter -> Parser Expression
conditionalExpression p =
    do c <- logicalOrExpression p
       (do reservedOp "?"
           t <- assignmentExpression p
           reservedOp ":"
           e <- assignmentExpression p
           return $ Operator "?:" [c, t, e])
           `ifFail` c

nonAssignmentExpression :: ParserParameter -> Parser Expression
nonAssignmentExpression p =
    do c <- logicalOrExpression p
       (do reservedOp "?"
           t <- nonAssignmentExpression p
           reservedOp ":"
           e <- nonAssignmentExpression p
           return $ Operator "?:" [c, t, e])
           `ifFail` c

--- Assignment Operators
assignmentExpression :: ParserParameter -> Parser Expression
assignmentExpression p =
    try (do l <- postfixExpression
            choice [
                   (do reservedOp "="
                       r <- assignmentExpression p
                       return $ Let l r),
                   (do Punctuator op <- compoundAssignment <|> logicalAssignment
                       r <- assignmentExpression p
                       return $ Let l $ Operator (init op) [l, r])
               ])
    <|> conditionalExpression p

compoundAssignment = choice $ map operator ["*=", "/=", "%=", "+=", "-=", "<<=", ">>=", ">>>=", "&=", "^=", "|="]
logicalAssignment  = choice $ map operator ["&&=", "^^=", "||="]

--- Comma Expressions
listExpression :: ParserParameter -> Parser Expression
listExpression p = liftM List $ (assignmentExpression p) `sepBy` comma

--- Type Expressions
typeExpression :: ParserParameter -> Parser Expression
typeExpression p = nonAssignmentExpression p
-- }}}

-- Statements {{{
-- http://www.mozilla.org/js/language/js20/core/statements.html
-- XXX: This order
statement :: ParserParameter -> Parser Statement
statement p = (ifStatement p)
          <|> (try block)
          <|> (whileStatement p)
          <|> (forStatement p)
          <|> (continueStatement `followedBy` (semicolon p))
          <|> (breakStatement `followedBy` (semicolon p))
          <|> (returnStatement `followedBy` (semicolon p))
          <|> (expressionStatement `followedBy` (semicolon p))
--        <|> superStatement `followedBy` (semicolon p)
--        <|> (block)
--        <|> labeledStatement
--        <|> switchStatement
--        <|> (doStatement `followedBy` (semicolon p))
--        <|> withStatement
--        <|> throwStatement `followedBy` (semicolon p)
          <|> tryStatement
--        <?> "statement(" ++ show p ++ ")"

substatement :: ParserParameter -> Parser Statement
substatement p = emptyStatement
             <|> (statement p)
--           <|> (simpleVariableDefinition `followedBy` (semicolon p))
--           <|> do attributes 
--                  braces substatements

substatements =
    do substatementsPrefix
       substatement Abbrev

substatementsPrefix =
    do substatementsPrefix
       substatement Full

semicolon :: ParserParameter -> Parser String
semicolon Full = semi
             <|> (newline >> return ";")
semicolon _    = semi
             <|> (newline >> return ";")
             <|> (eof >> return ";")
             <|> return ";"

--- Empty Statement
emptyStatement :: Parser Statement
emptyStatement =
    do semi
       return STEmpty

--- Expression Statement
expressionStatement :: Parser Statement
expressionStatement = ((reserved "function" <|> reservedOp "{") >> fail "")
                  <|> do expr <- listExpression AllowIn
                         return $ STExpression expr

--- Super Statement
-- TODO

--- Block Statement
block :: Parser Statement
block = liftM STBlock (braces directives) <?> "block"

--- Labeled Statements
--- If Statement
ifStatement NoShortIf =
    do reserved "if"
       condition <- parenListExpression
       thenStatement <- substatement NoShortIf
       reserved "else"
       elseStatement <- substatement NoShortIf
       return $ STIf condition thenStatement (Just elseStatement)

ifStatement p =
    do reserved "if"
       condition <- parenListExpression
       thenStatement <- substatement p
       (do reserved "else"
           elseStatement <- substatement p
           return $ STIf condition thenStatement (Just elseStatement))
           `ifFail` STIf condition thenStatement Nothing

--- Switch Statement
--- Do-While Statement
--- While Statement
whileStatement :: ParserParameter -> Parser Statement
whileStatement p =
    do reserved "while"
       condition <- parenListExpression
       block <- substatement p
       return $ STWhile condition block

--- For Statements
forStatement :: ParserParameter -> Parser Statement
forStatement p =
    do reserved "for"
       symbol "("
       (try $ do init <- forInitializer
                 semi
                 cond <- optionalExpression
                 semi
                 updt <- optionalExpression
                 symbol ")"
                 block <- substatement p
                 return $ STFor init cond updt block)
           <|> (do binding <- forInBinding
                   reserved "in"
                   object <- listExpression AllowIn
                   symbol ")"
                   block <- substatement p
                   return $ STForIn binding object block)

forInitializer :: Parser Statement
forInitializer = -- do { attributes; variableDefinition NoIn }
                 (variableDefinition NoIn)
             <|> (liftM STExpression $ listExpression NoIn)
             <|> (return STEmpty)

optionalExpression :: Parser Expression
optionalExpression = (listExpression AllowIn)
                 <|> (return $ List [])

forInBinding :: Parser Statement
forInBinding = --do { attributes; variableDefinitionKind; variableDefinition NoIn }
               (do isConst <- variableDefinitionKind
                   binding <- variableBinding NoIn
                   return $ STVariableDefinition isConst [binding])
           <|> liftM STExpression postfixExpression

--- With Statement
--- Continue and Break Statements
continueStatement :: Parser Statement
continueStatement =
    do reserved "continue"
       label <- option Nothing (liftM Just identifierString)
       return $ STContinue label

breakStatement :: Parser Statement
breakStatement =
    do reserved "break"
       label <- option Nothing (liftM Just identifierString)
       return $ STBreak label

--- Return Statement
returnStatement :: Parser Statement
returnStatement =
    do reserved "return"
       liftM STReturn
             (liftM Just (listExpression AllowIn) <|> mzero)

-- Throw Statement
-- Try Statement
tryStatement :: Parser Statement
tryStatement =
    do reserved "try"
       tryBlock <- block
       catchClauses <- many catchClause
       finallyClause <- do reserved "finally"
                           b <- block
                           return $ Just b
                    <|> (if null catchClauses
                            then fail "no catch clause"
                            else return Nothing)
       return $ STTry tryBlock catchClauses finallyClause

catchClause :: Parser (Parameter, Statement)
catchClause =
    do reserved "catch"
       param <- parens parameter
       block <- block
       return (param, block)

--- Directives
--- http://www.mozilla.org/js/language/js20/core/statements.html#directive
directive :: ParserParameter -> Parser Statement
directive p = (try $ annotatableDirective p)
          <|> (try $ statement p)
--        <|> do { attributes; annotatableDirective }
--        <|> do { attributes; braces directives }
--        <|> do { includeDirective; semi }
--        <|> do { pragma; semi }

annotatableDirective :: ParserParameter -> Parser Statement
annotatableDirective p = do { x <- variableDefinition AllowIn; semicolon p; return x }
                     <|> functionDefinition
--                   <|> classDefinition
--                   <|> nameSpaceDefinition `followedBy` semi
--                   <|> importDirective `followedBy` semi
--                   <|> expretDefinition `followedBy` semi
--                   <|> useDirective `followedBy` semi
--                   <?> "annotatableDirective(" ++ show p ++ ")"

directives :: Parser [Statement]
directives =
    do ds <- many $ try $ directive Full
       d <- ((directive Abbrev >>= return . return) <|> return [])
       return $ filter notEmptyStatement (ds ++ d)
    where notEmptyStatement (STExpression (List [])) = False
          notEmptyStatement _ = True

--- Programs
program :: Parser JavaScriptProgram
program = do {- ps <- many packageDefinition -} -- TODO
             ds <- directives
             return ds
-- }}}

-- Variables {{{
-- http://www.mozilla.org/js/language/js20/core/variables.html
--- Variable Definitions
variableDefinition :: ParserParameter -> Parser Statement
variableDefinition p =
    do isConst <- (reserved "const" >> return True) <|> (reserved "var" >> return False)
       bindings <- (variableBinding p) `sepBy` comma
       return $ STVariableDefinition isConst bindings
    <?> "variable definition"

variableDefinitionKind :: Parser Bool
variableDefinitionKind = (reserved "const" >> return True) <|> (reserved "var" >> return False)

variableBinding :: ParserParameter -> Parser VariableBinding
variableBinding p =
    do var <- typedIdentifier p
       init <- variableInitialisation p
       return (var, init)

variableInitialisation :: ParserParameter -> Parser (Maybe Expression)
variableInitialisation p =
    option Nothing
           (reservedOp "=" >> variableInitialiser p >>= return . return)
                
variableInitialiser :: ParserParameter -> Parser Expression
variableInitialiser p = assignmentExpression p
--                  <|> attributeCombination

typedIdentifier :: ParserParameter -> Parser (String, Maybe Expression)
typedIdentifier p =
    do name <- identifierString
       option (name, Nothing)
              (do colon
                  varType <- typeExpression p
                  return (name, Just varType))
-- }}}

-- Functions {{{
-- http://www.mozilla.org/js/language/js20/core/functions.html

--- Syntax
--- http://www.mozilla.org/js/language/js20/core/functions.html#N-FunctionCommon
functionDefinition :: Parser Statement
functionDefinition =
    do reserved "function"
       (getOrSet, name) <- functionName
       function <- functionCommon
       return $ STFunctionDefinition { funcDefType = getOrSet, funcDefFunc = function { funcName = Just name } }

functionName :: Parser (Maybe GetterOrSetter, String)
functionName =
    do getOrSet <- (do { reserved "get"; return $ Just Getter })
               <|> (do { reserved "set"; return $ Just Setter })
               <|> (return Nothing)
       name <- identifierString
       return (getOrSet, name)

functionCommon :: Parser Value
functionCommon =
    do params <- parens parameters
--     resultType <- result
       body <- block
       return $ Function Nothing params body

--- Parameters
--- http://www.mozilla.org/js/language/js20/core/functions.html#N-Parameters
parameters :: Parser Parameters
parameters = option ([], Nothing) nonemptyParameters

nonemptyParameters :: Parser Parameters
nonemptyParameters =
    (do headP <- parameterInit
        (tailP, rest) <- option ([], Nothing) (comma >> nonemptyParameters)
        return (headP:tailP, rest))
    <|> (do rest <- restParameter
            return ([], rest))

parameter :: Parser Parameter
parameter =
    do constOrNot <- parameterAttributes
--     typedIdentifier
       name <- identifierString
       return $ constOrNot name
    <?> "parameter"

parameterInit :: Parser Parameter
parameterInit =
    do param <- parameter
       option param
              (do reservedOp "="
                  assignmentExpression AllowIn
                  return param)

restParameter :: Parser (Maybe RestParameter)
restParameter =
    do reservedOp "..."
       option (Just Nothing)
              (do constOrNot <- parameterAttributes
                  name <- identifierString
                  return $ (Just . Just . constOrNot) name)

parameterAttributes :: Parser (String -> Parameter)
parameterAttributes = option NonConst (reserved "const" >> return Const)
-- }}}

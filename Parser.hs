{-
    Parser.hs
    構文パーザ
    http://www2u.biglobe.ne.jp/~oz-07ams/prog/ecma262r3/11_Expressions.html
    http://www2u.biglobe.ne.jp/~oz-07ams/prog/ecma262r3/12_Statements.html
-}

-- Module definition {{{
module Parser (module Parser, Text.ParserCombinators.Parsec.ParseError) where
import Text.ParserCombinators.Parsec hiding(Parser)
import Monad
import List
import Data.Char (isDigit,digitToInt)

import DataTypes
import ParserUtil

-- Literals {{{
-- http://www2u.biglobe.ne.jp/~oz-07ams/prog/ecma262r3/7_Lexical_Conventions.html
--- Identifiers
identifier :: Parser Expression
identifier = liftM Identifier identifierString

-- Literals
literal :: Parser Expression
literal = nullLiteral
      <|> booleanLiteral
      <|> numericLiteral
      <|> stringLiteral

--- Null Literals
nullLiteral :: Parser Expression
nullLiteral = do reserved "null"
                 return $ Literal Null

--- Boolean Literals
booleanLiteral :: Parser Expression
booleanLiteral = (reserved "true"  >> (return $ Literal $ Boolean True))
             <|> (reserved "false" >> (return $ Literal $ Boolean False))
             <?> "boolean"

--- Numeric Literals
numericLiteral :: Parser Expression
numericLiteral =
    do num <- naturalOrFloat
       return $ Literal
              $ Number
              $ case num of
                     Left n -> Integer n
                     Right n -> Double n
    <?> "number"

--- String Literals
-- some code from libraries/parsec/Text/ParserCombinators/Parsec/Expression.hs
stringLiteral :: Parser Expression
stringLiteral = (do charOrExprs <- (stringCharacters '"' <|> stringCharacters '\'')
                    whiteSpace
                    return $ (+++) $ foldr join [] charOrExprs)
            <?> "string"
    where join (Right c) ((Literal (String cs)):xs) =
              [Literal $ String $ c:cs] ++ xs
          join (Right c) xs = 
              [Literal $ String [c]] ++ xs
          join (Left e) xs =
              [e] ++ xs
          (+++) [x] = x
          (+++) (x:xs) = Operator "+" [x, (+++) xs]

stringCharacters :: Char -> Parser [Either Expression Char]
stringCharacters q = between (char q) (char q) (many $ stringChar q)

stringChar :: Char -> Parser (Either Expression Char)
stringChar q = (char '\\' >> escapeSequence)
           <|> (liftM Right $ noneOf (q:"\r\n\f"))

escapeSequence :: Parser (Either Expression Char)
escapeSequence = liftM Left stringInterpolateSequence
             <|> liftM Right characterEscapeSequence
             <|> liftM Right (do { char '0'; notFollowedBy $ satisfy isDigit; return '\0' })
             <|> liftM Right octEscapeSequence -- Not specified in ECMA-3
             <|> liftM Right hexEscapeSequence
--           <|> unicodeEscapeSequence

characterEscapeSequence :: Parser Char
characterEscapeSequence = singleEscapeCharacter
                      <|> noneOf "xu0123456789\r\n\f"

singleEscapeCharacter :: Parser Char
singleEscapeCharacter = choice $ zipWith escaped "'\"\\bfnrtv" "'\"\\\b\f\n\r\t\v"
                      where escaped c code = do { char c; return code }

octEscapeSequence :: Parser Char
octEscapeSequence = do digits <- try (times 3 digit) <|> try (times 2 digit) <|> times 1 digit
                       return $ toEnum $ foldr (\b a -> a * 8 + (digitToInt b)) 0 digits

hexEscapeSequence :: Parser Char
hexEscapeSequence = do char 'x'
                       digits <- times 2 hexDigit
                       return $ toEnum $ foldr (\b a -> a * 16 + (digitToInt b)) 0 digits

stringInterpolateSequence :: Parser Expression
stringInterpolateSequence =
    do char '{'
       expr <- expression AllowIn
       char '}'
       return expr
-- }}}

-- Expressions {{{
-- http://www2u.biglobe.ne.jp/~oz-07ams/prog/ecma262r3/11_Expressions.html

--- Primary Expressions
primaryExpression :: Parser Expression
primaryExpression = (keyword "this")
                <|> literal
                <|> identifier
                <|> arrayLiteral
                <|> objectLiteral
                <|> parens (expression AllowIn)
                <?> ""

--- Array Literals
arrayLiteral :: Parser Expression
arrayLiteral =
    (squares $ do el <- elisionOpt
                  es <- option [] elementList
                  return $ ArrayLiteral $ el ++ es)
     <?> "array"

elementList :: Parser [Expression]
elementList =
    do e <- assignmentExpression AllowIn
       es <- option [] elementList1
       return $ (e:es)

elementList1 :: Parser [Expression]
elementList1 =
    do comma
       el <- elisionOpt
       (try $ do es <- elementList
                 return $ el ++ es)
        <|> return el

elisionOpt :: Parser [Expression]
elisionOpt = many comma >>= return . map (const $ Literal Undefined)

--- Object Literals
objectLiteral :: Parser Expression
objectLiteral = braces (objectLiteralPair `sepBy` comma) >>= return . ObjectLiteral
            <?> "object literal"

objectLiteralPair :: Parser (Expression, Expression)
objectLiteralPair =
    do name <- stringLiteral <|> numericLiteral <|> (liftM (Literal . String) identifierString)
       colon
       expr <- assignmentExpression AllowIn
       return (name, expr)

--- Left-Hand-Side Expressions
memberExpression :: Parser Expression
memberExpression =
    do e <- (primaryExpression
             <|> functionExpression
             <|> do reserved "new"
                    e <- memberExpression
                    Operator _ args <- arguments
                    return $ Operator "new" (e:args))
       ps <- many (squares (expression AllowIn)
                   <|> (do reservedOp "."
                           liftM (Literal . String) identifierString)
                   <?> "")
       return $ foldl (\e p -> Operator "[]" [e,p]) e ps

newExpression :: Parser Expression
newExpression =
    try memberExpression
    <|> (do reserved "new"
            e <- newExpression 
            return $ Operator "new" [e])

callExpression :: Parser Expression
callExpression =
    do e <- memberExpression
       a <- arguments
       ps <- many $ (arguments
                     <|> (liftM (Operator "[]" . return) $ squares $ expression AllowIn)
                     <|> (do reservedOp "."
                             liftM (Operator "[]" . return . Literal . String) identifierString)
                     <?> "")
       return $ foldl pushArg (pushArg e a) ps

arguments :: Parser Expression
arguments =
    liftM (Operator "()") (parens $ (assignmentExpression AllowIn) `sepBy` comma)

leftHandSideExpression :: Parser Expression
leftHandSideExpression = (try callExpression) <|> newExpression

parenExpression :: Parser Expression
parenExpression = parens (assignmentExpression AllowIn)

parenListExpression :: Parser Expression
parenListExpression = liftM List (parens $ (assignmentExpression AllowIn) `sepBy` comma)

--- Postfix Expressions
postfixExpression :: Parser Expression
postfixExpression =
    do e <- leftHandSideExpression
       e <- option e (do { noLineTerminatorHere; reservedOp "++"; return $ Operator "_++" [e] }
                      <|> do { noLineTerminatorHere; reservedOp "--"; return $ Operator "_--" [e] }) 
       return e

--- Unary Operators
operatorWithArg1 :: String -> Parser Expression -> Parser Expression
operatorWithArg1 op p =
    do reservedOp op
       arg <- p
       return $ Operator op [arg]

chainOperator :: [String] -> Parser Expression -> Parser Expression
chainOperator ops p =
    (do a <- p
        e <- many $ do Punctuator op <- choice $ map operator ops
                       a <- p
                       return $ Operator op [a]
        return $ foldl pushArg a e)
     <?> ""

unaryExpression :: Parser Expression
unaryExpression = postfixExpression
              <|> operatorWithArg1 "delete" postfixExpression
              <|> operatorWithArg1 "void" unaryExpression
              <|> operatorWithArg1 "typeof" unaryExpression
              <|> operatorWithArg1 "++" postfixExpression
              <|> operatorWithArg1 "--" postfixExpression
              <|> operatorWithArg1 "+" unaryExpression
              <|> operatorWithArg1 "-" unaryExpression
              <|> operatorWithArg1 "~" unaryExpression
              <|> operatorWithArg1 "!" unaryExpression

--- Multiplicative Operators
multiplicativeExpression :: Parser Expression
multiplicativeExpression = chainOperator ["*", "/", "%"] unaryExpression

--- Additive Operators
additiveExpression :: Parser Expression
additiveExpression = chainOperator ["+", "-"] multiplicativeExpression

--- Bitwise Shift Operators
shiftExpression :: Parser Expression
shiftExpression = chainOperator ["<<", ">>", ">>>"] additiveExpression

--- Relational Operators
relationalExpression :: ParserParameter -> Parser Expression
relationalExpression AllowIn = chainOperator ["<", ">", "<=", ">=", "is", "as", "in", "instanceof"] shiftExpression
relationalExpression NoIn    = chainOperator ["<", ">", "<=", ">=", "is", "as", "instanceof"] shiftExpression

--- Equality Operators
equalityExpression :: ParserParameter -> Parser Expression
equalityExpression p = chainOperator ["==", "!=", "===", "!=="] (relationalExpression p)

--- Binary Bitwise Operators
bitwiseAndExpression, bitwiseXorExpression, bitwiseOrExpression :: ParserParameter -> Parser Expression
bitwiseAndExpression p = chainOperator ["&"] (equalityExpression p)
bitwiseXorExpression p = chainOperator ["^"] (bitwiseAndExpression p)
bitwiseOrExpression  p = chainOperator ["|"] (bitwiseXorExpression p)

--- Binary Logical Operators
logicalAndExpression, logicalXorExpression, logicalOrExpression :: ParserParameter -> Parser Expression
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
    try (do l <- leftHandSideExpression
            (do reservedOp "="
                r <- assignmentExpression p
                return $ Let l r)
             <|> (do op <- compoundAssignment
                     r <- assignmentExpression p
                     return $ Let l $ Operator (init op) [l, r]))
    <|> conditionalExpression p

compoundAssignment, logicalAssignment :: Parser String
compoundAssignment = choice $ map opString ["*=", "/=", "%=", "+=", "-=", "<<=", ">>=", ">>>=", "&=", "^=", "|="]
logicalAssignment  = choice $ map opString ["&&=", "^^=", "||="] -- Not defined

opString :: String -> Parser String
opString op = do reservedOp op
                 return op

--- Comma Operator
expression :: ParserParameter -> Parser Expression
expression p = liftM toList $ (assignmentExpression p) `sepBy1` comma
             where toList [x] = x
                   toList xs = List xs
-- }}}

-- Statements {{{
-- http://www.mozilla.org/js/language/js20/core/statements.html
statement :: Parser Statement
statement = block
        <|> (variableStatement AllowIn)
        <|> emptyStatement
        <|> (try labelledStatement)
        <|> expressionStatement
        <|> ifStatement
        <|> iterationStatement
        <|> continueStatement
        <|> breakStatement
        <|> returnStatement
        <|> withStatement
        <|> switchStatement
        <|> throwStatement
        <|> tryStatement

semicolon :: Parser ()
semicolon = (semi >> return ())
        <|> ((symbol "}" <?> "") >> putBack "}") 
        <|> lineTerminator
        <|> do pos <- getPosition
               st <- getState 
               input <- getInput
               case () of
                    _ | null input -> do setInput ";"
                                         semi 
                                         return ()
                    _ | pos == stLTPos st
                                   -> do setInput (';':input)
                                         semicolon
                    _ | otherwise  -> fail ""

--- Block
block :: Parser Statement
block = liftM STBlock (braces statementListOpt) <?> "block"

statementListOpt :: Parser [Statement]
statementListOpt = many statement

--- Variable statement
variableStatement :: ParserParameter -> Parser Statement
variableStatement p =
    do reserved "var"
       bindings <- variableDeclarationList p
       semicolon
       return $ STVariableDefinition bindings
    <?> "variable definition"

variableDeclarationList :: ParserParameter -> Parser [VariableBinding]
variableDeclarationList p =
    (variableDeclaration p) `sepBy` comma

variableDeclaration :: ParserParameter -> Parser VariableBinding
variableDeclaration p =
    do var <- identifierString
       init <- initialiserOpt p
       return (var, init)

initialiserOpt :: ParserParameter -> Parser (Maybe Expression)
initialiserOpt p =
    option Nothing
           (do reservedOp "="
               liftM Just $ assignmentExpression p)

--- Empty Statement
emptyStatement :: Parser Statement
emptyStatement =
    do semi
       return STEmpty

--- Expression Statement
expressionStatement :: Parser Statement
expressionStatement = ((reserved "function" <|> reservedOp "{") >> fail "")
                  <|> do expr <- expression AllowIn
                         semicolon
                         return $ STExpression expr

--- The if Statement
ifStatement :: Parser Statement
ifStatement =
    do reserved "if"
       condition <- parens $ expression AllowIn
       thenStatement <- statement
       (do reserved "else"
           elseStatement <- statement
           return $ STIf condition thenStatement (Just elseStatement))
           `ifFail` STIf condition thenStatement Nothing

--- Iteration Statements
iterationStatement :: Parser Statement
iterationStatement = doWhileStatement
                 <|> whileStatement
                 <|> forStatement

---- Do-While Statement
doWhileStatement :: Parser Statement
doWhileStatement =
    do reserved "do"
       block <- statement
       reserved "while"
       condition <- parens $ expression AllowIn
       semicolon
       return $ STDoWhile condition block

---- While Statement
whileStatement :: Parser Statement
whileStatement =
    do reserved "while"
       condition <- parens $ expression AllowIn
       block <- statement
       return $ STWhile condition block

--- For Statements
forStatement :: Parser Statement
forStatement =
    do reserved "for"
       symbol "("
       (try $ do init <- forInitializer
                 semi
                 cond <- expressionOpt
                 semi
                 updt <- expressionOpt
                 symbol ")"
                 block <- statement
                 return $ STFor init cond updt block)
           <|> (do binding <- (do reserved "var" -- TODO: leftHandSideExpression
                                  liftM STVariableDefinition $ variableDeclarationList NoIn)
                   reserved "in"
                   object <- expression AllowIn
                   symbol ")"
                   block <- statement
                   return $ STForIn binding object block)

forInitializer :: Parser Statement
forInitializer =
    option STEmpty
           ((do reserved "var"
                liftM STVariableDefinition $ variableDeclarationList NoIn)
            <|> (liftM STExpression $ expression NoIn))

expressionOpt :: Parser Expression
expressionOpt = (expression AllowIn)
            <|> (return $ List [])

--- Switch Statement
--- With Statement
--- Continue and Break Statements
continueStatement :: Parser Statement
continueStatement =
    do reservedWithNoLT "continue"
       label <- option Nothing (liftM Just identifierString)
       semicolon
       return $ STContinue label

breakStatement :: Parser Statement
breakStatement =
    do reservedWithNoLT "break"
       label <- option Nothing (liftM Just identifierString)
       semicolon
       return $ STBreak label

--- Return Statement
returnStatement :: Parser Statement
returnStatement =
    do reservedWithNoLT "return"
       expr <- option Nothing (liftM Just (try $ expression AllowIn))
       semicolon
       return $ STReturn expr

-- Labelled Statements
labelledStatement :: Parser Statement
labelledStatement =
    do label <- identifierString
       colon
       st <- statement
       return $ STLabelled label st

-- The with Statement
withStatement :: Parser Statement
withStatement =
    do reserved "with"
       expr <- parens $ expression AllowIn
       st <- statement
       return $ STWith expr st

-- The switch Statement
switchStatement :: Parser Statement
switchStatement =
    do reserved "switch"
       expr <- parens $ expression AllowIn
       cases <- caseBlock
       return $ STSwitch expr cases

caseBlock :: Parser [(Maybe Expression, Statement)]
caseBlock =
    braces $ do clausesPre <- caseClausesOpt
                clausesDefault <- option mzero (liftM return defaultCase)
                clausesPost <- caseClausesOpt
                return $ clausesPre ++ clausesDefault ++ clausesPost

caseClausesOpt :: Parser [(Maybe Expression, Statement)]
caseClausesOpt =
    many $ do reserved "case"
              expr <- expression AllowIn
              colon
              ss <- statementListOpt
              return (Just expr, STBlock ss)

defaultCase :: Parser (Maybe Expression, Statement)
defaultCase =
    do reserved "default"
       colon
       ss <- statementListOpt
       return (Nothing, STBlock ss)

-- Throw Statement
throwStatement :: Parser Statement
throwStatement =
    do reservedWithNoLT "throw"
       expr <- expression AllowIn
       semicolon
       return $ STThrow expr

-- Try Statement
tryStatement :: Parser Statement
tryStatement =
    do reserved "try"
       try <- block
       (catch, finally) <- (do c <- catchClause
                               f <- option Nothing $ liftM Just finallyClause
                               return (Just c, f))
                               <|> (do f <- finallyClause
                                       return (Nothing, Just f)) 
       return $ STTry try catch finally

catchClause :: Parser (Parameter, Statement)
catchClause =
    do reserved "catch"
       param <- parens identifierString
       block <- block
       return (param, block)

finallyClause :: Parser Statement
finallyClause =
    do reserved "finally"
       block

--- Programs
program :: Parser JavaScriptProgram
program = many (functionDeclaration <|> statement)
-- }}}

-- Functions {{{
--- http://www2u.biglobe.ne.jp/~oz-07ams/prog/ecma262r3/13_Function_Definition.html

--- Function Expressions
functionDeclaration :: Parser Statement
functionDeclaration =
    do reserved "function"
       name <- identifierString
       function <- functionCommon
       return $ STFunctionDefinition { funcDefFunc = function { funcName = name } }
    <?> "function declaration"

functionExpression :: Parser Expression
functionExpression =
    do reserved "function"
       name <- option "" identifierString
       Function { funcParam = params, funcBody = body } <- functionCommon
       return $ Literal $ nullFunction { funcName = name, funcParam = params, funcBody = body }

functionCommon :: Parser Value
functionCommon =
    do params <- parens formalParameterListOpt
       body <- block
       return $ nullFunction { funcParam = params, funcBody = body }

formalParameterListOpt :: Parser Parameters
formalParameterListOpt = identifierString `sepBy` comma
-- }}}

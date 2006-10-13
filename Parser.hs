{-
    Parser.hs
    構文パーザ
    http://www.mozilla.org/js/language/js20/formal/parser-grammar.html
-}

-- Module definition {{{
module Parser where
import Text.ParserCombinators.Parsec hiding(Parser)
import Text.ParserCombinators.Parsec.Pos
import Text.ParserCombinators.Parsec.Error
import Data.Char (isDigit,digitToInt)
import Monad
import List
import Foreign

import DataTypes

data ParserParameter
    = AllowIn
    | NoIn
    deriving Show
    
data ParserState = ParserState { stAllowLT :: Bool, stSourcePos :: SourcePos }

type Parser a = GenParser Char ParserState a

initialState :: ParserState
initialState = ParserState { stAllowLT = True, stSourcePos = newPos "" (-1) (-1) }

runLex :: Parser a -> String -> Either ParseError a
runLex p input =
    runParser (do { whiteSpace; x <- p; eof; return x })
              initialState 
              ""
              input

showError :: String -> ParseError -> String
showError source err =
    let pos = errorPos err
        in unlines
           $ [lines source !! (sourceLine pos - 1), 
              take (sourceColumn pos - 1) (repeat '.') ++ "^",
              showErrorMessages "or" "unknown parse error" "expecting" "unexpected" "end of input"
                                (errorMessages err)]
autoInsertSemi :: Parser ()
autoInsertSemi =
    do st <- getState
       pos <- getPosition
       setState $ st { stSourcePos = pos }

putBack :: [Char] -> Parser ()
putBack toks =
    do input <- getInput
       setInput $ toks ++ input

-- [No line terminator here]
withNoLineTerminator :: Parser a -> Parser a
withNoLineTerminator p =
    do updateState $ \st -> st { stAllowLT = False }
       x <- p
       updateState $ \st -> st { stAllowLT = True }
       return x
-- }}}

-- Lexer {{{
reservedNames,reservedOpNames :: [String]
reservedNames = [
        "break",  "else",  "new",  "var", 
        "case",  "finally",  "return",  "void", 
        "catch",  "for",  "switch",  "while", 
        "continue",  "function",  "this",  "with", 
        "default",  "if",  "throw", 
        "delete",  "in",  "try", 
        "do",  "instanceof",  "typeof", 
        -- Future reserved
        "abstract",  "enum",  "int",  "short", 
        "boolean",  "export",  "interface",  "static", 
        "byte",  "extends",  "long",  "super", 
        "char",  "final",  "native",  "synchronized", 
        "class",  "float",  "package",  "throws", 
        "const",  "goto",  "private",  "transient", 
        "debugger",  "implements",  "protected",  "volatile", 
        "double",  "import",  "public"
    ]

reservedOpNames = [
        "!", "!=", "!==", "%", "%=", "&", "&&", "&&=",
        "&=", "(", ")", "*", "*=", "+", "++", "+=",
        ",", "-", "--", "-=", ".", "...", "/", "/=",
        ":", "::", ";", "<", "<<", "<<=", "<=", "=",
        "==", "===", ">", ">=", ">>", ">>=", ">>>",
        ">>>=", "?", "[", "]", "^", "^=", "^^", "^^=",
        "{", "|", "|=", "||", "||=", "}", "~"
    ]

lexeme :: Parser a -> Parser a
lexeme p =
    do { x <- p; whiteSpace; return x }

whiteSpace :: Parser ()
whiteSpace =
    do st <- getState
       if stAllowLT st
          then skipMany (simpleSpace <|> lineTerminator <|> oneLineComment <|> multiLineComment <?> "")
          else skipMany (simpleSpace <|> oneLineComment <|> multiLineComment <?> "")

simpleSpace :: Parser ()
simpleSpace =
    skipMany1 (oneOf " \t\v\f\b") -- TODO: Unicode char

lineTerminator :: Parser ()
lineTerminator =
    do many1 $ oneOf "\r\n"
       autoInsertSemi

oneLineComment :: Parser ()
oneLineComment =
    do try $ string "//"
       skipMany $ satisfy (/= '\n')
       return ()

multiLineComment :: Parser ()
multiLineComment =
    do try $ string "/*"
       inComment

inComment :: Parser ()
inComment = do { try $ string "*/"      ; return () }
        <|> do { skipMany1 $ noneOf "*/"; inComment }
        <|> do { oneOf "*/"             ; inComment }
        <?> "end of comment"

symbol :: String -> Parser String
symbol name =
    lexeme (string name)

naturalOrFloat :: Parser (Either Integer Double)
naturalOrFloat  = lexeme (natFloat) <?> "number"

float           = lexeme floating   <?> "float"
integer         = lexeme int        <?> "integer"
natural         = lexeme nat        <?> "natural"

-- floats
floating        = do{ n <- decimal 
                    ; fractExponent n
                    }

natFloat        = do{ char '0'
                    ; zeroNumFloat
                    }
                  <|> decimalFloat
                  
zeroNumFloat    =  do{ n <- hexadecimal <|> octal
                     ; return (Left n)
                     }
                <|> decimalFloat
                <|> fractFloat 0
                <|> return (Left 0)                  
                  
decimalFloat    = do{ n <- decimal
                    ; option (Left n) 
                             (fractFloat n)
                    }

fractFloat n    = do{ f <- fractExponent n
                    ; return (Right f)
                    }
                    
fractExponent n = do{ fract <- fraction
                    ; expo  <- option 1.0 exponent'
                    ; return ((fromInteger n + fract)*expo)
                    }
                <|>
                  do{ expo <- exponent'
                    ; return ((fromInteger n)*expo)
                    }

fraction        = do{ char '.'
                    ; digits <- many1 digit <?> "fraction"
                    ; return (foldr op 0.0 digits)
                    }
                  <?> "fraction"
                where
                  op d f    = (f + fromIntegral (digitToInt d))/10.0
                    
exponent'       = do{ oneOf "eE"
                    ; f <- sign
                    ; e <- decimal <?> "exponent"
                    ; return (power (f e))
                    }
                  <?> "exponent"
                where
                   power e  | e < 0      = 1.0/power(-e)
                            | otherwise  = fromInteger (10^e)


-- integers and naturals
int             = do{ f <- lexeme sign
                    ; n <- nat
                    ; return (f n)
                    }
                    
-- sign            :: CharParser st (Integer -> Integer)
sign            =   (char '-' >> return negate) 
                <|> (char '+' >> return id)     
                <|> return id

nat             = zeroNumber <|> decimal
    
zeroNumber      = do{ char '0'
                    ; hexadecimal <|> octal <|> decimal <|> return 0
                    }
                  <?> ""       

decimal         = number 10 digit        
hexadecimal     = do{ oneOf "xX"; number 16 hexDigit }
octal           = do{ oneOf "oO"; number 8 octDigit  }

-- number :: Integer -> CharParser st Char -> CharParser st Integer
number base baseDigit
    = do{ digits <- many1 baseDigit
        ; let n = foldl (\x d -> base*x + toInteger (digitToInt d)) 0 digits
        ; seq n (return n)
        }          

-----------------------------------------------------------
-- Operators & reserved ops
-----------------------------------------------------------
reservedOp name =   
   (lexeme $ try $
    do{ string name
      ; notFollowedBy (oneOf "+-*/%?:&|^<>=") <?> ("end of " ++ show name)
      }) <?> ""

{-
operator =
    lexeme $ try $
    do{ name <- oper
      ; if (isReservedOp name)
         then unexpected ("reserved operator " ++ show name)
         else return name
      }
      
oper =
    do{ c <- (opStart languageDef)
      ; cs <- many (opLetter languageDef)
      ; return (c:cs)
      }
    <?> "operator"
-}
    
isReservedOp name =
    isReserved (sort (reservedOpNames)) name          
    
-----------------------------------------------------------
-- Identifiers & Reserved words
-----------------------------------------------------------
reserved name =
   (lexeme $ try $
    do{ string name
      ; notFollowedBy (alphaNum <|> oneOf "$_") <?> ("end of " ++ show name)
      }) <?> ""

identifierString =
   (lexeme $ try $
    do{ name <- ident
      ; if (isReservedName name)
         then unexpected ("reserved word " ++ show name)
         else return name
      }) <?> "identifier"
    
ident           
    = do{ c <- (letter <|> oneOf "$_") <?> ""
        ; cs <- many (alphaNum <|> oneOf "$_") <?> ""
        ; return (c:cs)
        }
    <?> "identifier"

isReservedName name
    = isReserved theReservedNames name
    
isReserved names name    
    = scan names
    where
      scan []       = False
      scan (r:rs)   = case (compare r name) of
                        LT  -> scan rs
                        EQ  -> True
                        GT  -> False

theReservedNames
    = sort reservedNames

parens p        = between (symbol "(") (symbol ")") p
braces p        = between (symbol "{") (symbol "}") p
angles p        = between (symbol "<") (symbol ">") p
brackets p      = between (symbol "[") (symbol "]") p
squares         = brackets

semi            = symbol ";" 
comma           = symbol ","
dot             = symbol "."
colon           = symbol ":"

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
    <?> ""

followedBy :: Show tok => GenParser tok st a -> GenParser tok st b -> GenParser tok st a
p `followedBy` q = do { x <- p; q; return x }

times :: Integer -> GenParser tok st a -> GenParser tok st [a]
times n p = foldl (\xs _ -> do { ys <- xs; y <- p; return $ y:ys }) (return []) [1..n]

ifFail :: GenParser tok st a -> a -> GenParser tok st a
p `ifFail` x = option x p

pushArg :: Expression -> Expression -> Expression
pushArg expr (Operator op args) = Operator op (expr:args)
-- }}}

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
    do e <- withNoLineTerminator leftHandSideExpression
       e <- option e (do { reservedOp "++"; return $ Operator "_++" [e] }
                      <|> do { reservedOp "--"; return $ Operator "_--" [e] }) 
       whiteSpace
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
expression p = liftM List $ (assignmentExpression p) `sepBy1` comma
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
--      <|> withStatement
        <|> switchStatement
        <|> throwStatement
        <|> tryStatement

semicolon :: Parser ()
semicolon = (semi >> return ())
        <|> (symbol "}" >> putBack "}") 
        <|> lineTerminator
        <|> do pos <- getPosition
               st <- getState 
               input <- getInput
               case () of
                    _ | null input -> do setInput ";"
                                         semi 
                                         return ()
                    _ | pos == stSourcePos st
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
       semi
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
           <|> (do binding <- variableStatement NoIn -- TODO: leftHandSideExpression
                   reserved "in"
                   object <- expression AllowIn
                   symbol ")"
                   block <- statement
                   return $ STForIn binding object block)

forInitializer :: Parser Statement
forInitializer =
    option STEmpty
           (variableStatement NoIn <|> (liftM STExpression $ expression NoIn))

expressionOpt :: Parser Expression
expressionOpt = (expression AllowIn)
            <|> (return $ List [])

--- Switch Statement
--- With Statement
--- Continue and Break Statements
continueStatement :: Parser Statement
continueStatement =
    do withNoLineTerminator $ reserved "continue"
       label <- option Nothing (liftM Just identifierString)
       semicolon
       return $ STContinue label

breakStatement :: Parser Statement
breakStatement =
    do withNoLineTerminator $ reserved "break"
       label <- option Nothing (liftM Just identifierString)
       semicolon
       return $ STBreak label

--- Return Statement
returnStatement :: Parser Statement
returnStatement =
    do withNoLineTerminator $ reserved "return"
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
    do withNoLineTerminator $ reserved "throw"
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
       return $ STFunctionDefinition { funcDefFunc = function { funcName = Just name } }
    <?> "function declaration"

functionExpression :: Parser Expression
functionExpression =
    do reserved "function"
       name <- option Nothing (liftM Just identifierString)
       Function { funcParam = params, funcBody = body } <- functionCommon
       return $ Literal $ Function name params body

functionCommon :: Parser Value
functionCommon =
    do params <- parens formalParameterListOpt
       body <- block
       return $ Function Nothing params body

formalParameterListOpt :: Parser Parameters
formalParameterListOpt = identifierString `sepBy` comma
-- }}}

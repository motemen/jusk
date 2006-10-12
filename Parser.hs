{-
    Parser.hs
    構文パーザ
    http://www.mozilla.org/js/language/js20/formal/parser-grammar.html
-}

-- Module definition {{{
module Parser where
import Text.ParserCombinators.Parsec
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
    skipMany (simpleSpace <|> oneLineComment <|> multiLineComment <?> "")

simpleSpace :: Parser ()
simpleSpace =
    skipMany1 (oneOf " \t") -- TODO: Unicode char

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
    lexeme $ try $
    do{ string name
      ; notFollowedBy (oneOf "+-*/%?:&|^<>") <?> ("end of " ++ show name)
      }

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
    lexeme $ try $
    do{ string name
      ; notFollowedBy (alphaNum <|> oneOf "$_") <?> ("end of " ++ show name)
      }

identifierString =
    lexeme $ try $
    do{ name <- ident
      ; if (isReservedName name)
         then unexpected ("reserved word " ++ show name)
         else return name
      }
    
    
ident           
    = do{ c <- (letter <|> oneOf "$_")
        ; cs <- many (alphaNum <|> oneOf "$_")
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
       return $ Literal $ Number
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
                <|> identifier
                <|> literal
                <|> arrayLiteral
                <|> objectLiteral
                <|> parens (expression AllowIn)

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
                    List args <- arguments
                    return $ Operator "new" (e:args))
       ps <- many (squares (expression AllowIn)
                   <|> (do reservedOp "."
                           liftM (Literal . String) identifierString))
       return $ foldl (\e p -> Operator "[]" [e,p]) e ps

newExpression :: Parser Expression
newExpression =
    (do reserved "new"
        e <- newExpression 
        return $ Operator "new" [e])
    <|> memberExpression

callExpression :: Parser Expression
callExpression =
    do e <- memberExpression
       a <- arguments
       ps <- many $ (arguments
                     <|> (squares $ expression AllowIn)
                     <|> (do reservedOp "."
                             liftM (Literal . String) identifierString))
       return $ foldl pushArg (pushArg e a) ps

arguments :: Parser Expression
arguments =
    liftM (Operator "()") (parens $ (assignmentExpression AllowIn) `sepBy` comma)

leftHandSideExpression :: Parser Expression
leftHandSideExpression = (try newExpression)
                     <|> callExpression

parenExpression :: Parser Expression
parenExpression = parens (assignmentExpression AllowIn)

parenListExpression :: Parser Expression
parenListExpression = liftM List (parens $ (assignmentExpression AllowIn) `sepBy` comma)

--- Postfix Expressions
postfixExpression :: Parser Expression
postfixExpression =
    do e <- leftHandSideExpression
       option e (do { reservedOp "++"; return $ Operator "_++" [e] }
                 <|> do { reservedOp "--"; return $ Operator "_--" [e] }) 

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
logicalAssignment  = choice $ map opString ["&&=", "^^=", "||="]

opString :: String -> Parser String
opString op = do reservedOp op
                 return op

--- Comma Expressions
expression :: ParserParameter -> Parser Expression
expression p = liftM List $ (assignmentExpression p) `sepBy` comma
-- }}}

-- Statements {{{
-- http://www.mozilla.org/js/language/js20/core/statements.html
-- XXX: This order
statement :: ParserParameter -> Parser Statement
statement p = block
          <|> (variableStatement p)
          <|> (ifStatement p)
          <|> (whileStatement p)
          <|> (forStatement p)
          <|> (continueStatement `followedBy` (semicolon p))
          <|> (breakStatement `followedBy` (semicolon p))
          <|> (returnStatement `followedBy` (semicolon p))
--        <|> superStatement `followedBy` (semicolon p)
--        <|> (block)
--        <|> labeledStatement
--        <|> switchStatement
--        <|> (doStatement `followedBy` (semicolon p))
--        <|> withStatement
--        <|> throwStatement `followedBy` (semicolon p)
          <|> tryStatement
          <|> (expressionStatement `followedBy` semi)
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
                  <|> do expr <- expression AllowIn
                         return $ STExpression expr

--- Block Statement
block :: Parser Statement
block = liftM STBlock (braces $ many $ statement AllowIn) <?> "block"

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
                 cond <- expressionOpt
                 semi
                 updt <- expressionOpt
                 symbol ")"
                 block <- substatement p
                 return $ STFor init cond updt block)
           <|> (do binding <- variableStatement NoIn -- TODO: leftHandSideExpression
                   reserved "in"
                   object <- expression AllowIn
                   symbol ")"
                   block <- substatement p
                   return $ STForIn binding object block)

forInitializer :: Parser Statement
forInitializer =
    option STEmpty
           (variableStatement NoIn <|> (liftM STExpression $ expression NoIn))

expressionOpt :: Parser Expression
expressionOpt = (expression AllowIn)
            <|> (return $ List [])

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
             (liftM Just (expression AllowIn) <|> mzero)

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
       param <- parens identifierString
       block <- block
       return (param, block)

--- Programs
program :: Parser JavaScriptProgram
program = many (statement AllowIn <|> functionDeclaration)
-- }}}

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

-- Functions {{{
--- http://www2u.biglobe.ne.jp/~oz-07ams/prog/ecma262r3/13_Function_Definition.html

--- Function Expressions
functionDeclaration :: Parser Statement
functionDeclaration =
    do reserved "function"
       name <- identifierString
       function <- functionCommon
       return $ STFunctionDefinition { funcDefFunc = function { funcName = Just name } }

functionExpression :: Parser Expression
functionExpression =
    do reserved "function"
       name <- option Nothing (liftM Just identifierString)
       Function { funcParam = params, funcBody = body } <- functionCommon
       return $ Literal $ Function name params body
    <?> "function expression"

functionCommon :: Parser Value
functionCommon =
    do params <- parens formalParameterListOpt
       body <- block
       return $ Function Nothing params body

formalParameterListOpt :: Parser Parameters
formalParameterListOpt = identifierString `sepBy` comma
-- }}}

{-
    ParserUtil.hs
    パーザユーティリティー関数
-}

module ParserUtil where
import Text.ParserCombinators.Parsec hiding(Parser)
import Text.ParserCombinators.Parsec.Pos
import Text.ParserCombinators.Parsec.Error
import Data.Char (digitToInt)
import List
import Control.Monad

import DataTypes

data ParserParameter
    = AllowIn
    | NoIn
    deriving Show
    
data ParserState = ParserState { stSeenLT :: Bool, stLTPos :: SourcePos }

type Parser a = GenParser Char ParserState a

initialState :: ParserState
initialState = ParserState { stSeenLT = False, stLTPos = newPos "" (-1) (-1) }

runLex :: Parser a -> String -> Either ParseError a
runLex p input =
    runParser (do { whiteSpace; x <- p; eof; return x })
              initialState 
              ""
              input

isErrorAtEnd :: String -> ParseError -> Bool
isErrorAtEnd "" _ = True

isErrorAtEnd source err =
    let pos = errorPos err
        in sourceLine pos == (length $ lines source) &&
           sourceColumn pos > (length $ last $ lines source)

showError :: String -> ParseError -> String
showError source err =
    let pos = errorPos err
        line = sourceLine pos
        col  = sourceColumn pos
        in unlines $ [
               "line " ++ show line ++ ", column " ++ show col ++ ":",
                if length (lines source) > line - 1
                   then lines source !! (line - 1)
                   else last $ lines source,
                take (col - 1) (repeat '.') ++ "^",
                showErrorMessages "or" "unknown parse error" "expecting" "unexpected" "end of input" (errorMessages err)
           ]
autoInsertSemi :: Parser ()
autoInsertSemi =
    do st <- getState
       pos <- getPosition
       setState $ st { stLTPos = pos }

putBack :: [Char] -> Parser ()
putBack toks =
    do input <- getInput
       setInput $ toks ++ input

whiteSpace :: Parser ()
whiteSpace =
    do updateState $ \st -> st { stSeenLT = False }
       skipMany (simpleSpace <|> lineTerminator <|> oneLineComment <|> multiLineComment <?> "")
       st <- getState
       when (stSeenLT st)
            (do pos <- getPosition
                setState $ st { stLTPos = pos })

noLineTerminatorHere :: Parser ()
noLineTerminatorHere =
    do st <- getState
       pos <- getPosition
       when (pos == stLTPos st)
            (fail "No line terminator here")
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

simpleSpace :: Parser ()
simpleSpace =
    skipMany1 (oneOf " \t\v\f\b") -- TODO: Unicode char

lineTerminator :: Parser ()
lineTerminator =
    do many1 $ oneOf "\r\n"
       updateState $ \st -> st { stSeenLT = True }

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

reservedWithNoLT name =
   (try $ do string name
             notFollowedBy (alphaNum <|> oneOf "$_") <?> ("end of " ++ show name)
             skipMany (simpleSpace <|> oneLineComment <|> multiLineComment <?> ""))
   <?> ""

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

semi            = symbol ";" <?> ""
comma           = symbol "," <?> ""
dot             = symbol "." <?> ""
colon           = symbol ":" <?> ""

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

times :: Int -> GenParser tok st a -> GenParser tok st [a]
times = replicateM

ifFail :: GenParser tok st a -> a -> GenParser tok st a
p `ifFail` x = option x p

pushArg :: Expression -> Expression -> Expression
pushArg expr (Operator op args) = Operator op (expr:args)
-- }}}

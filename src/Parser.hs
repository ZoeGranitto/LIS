module Parser where

import           Text.ParserCombinators.Parsec
import           Text.Parsec.Token
import           Text.Parsec.Language           ( emptyDef )
import           AST

-----------------------
-- Función para facilitar el testing del parser.
totParser :: Parser a -> Parser a
totParser p = do
  whiteSpace lis
  t <- p
  eof
  return t

-- Analizador de Tokens
lis :: TokenParser u
lis = makeTokenParser
  (emptyDef
    { commentStart    = "/*"
    , commentEnd      = "*/"
    , commentLine     = "//"
    , opLetter        = char '='
    , reservedNames   = ["true", "false", "skip", "if", "else", "repeat", "until"]
    , reservedOpNames = [ "+"
                        , "-"
                        , "*"
                        , "/"
                        , "<"
                        , ">"
                        , "&&"
                        , "||"
                        , "!"
                        , "="
                        , "=="
                        , "!="
                        , ";"
                        , ","
                        , "++"
                        , "--"
                        ]
    }
  )

-----------------------------------
--- Parsers aux
-----------------------------------
variableParser :: Parser (Exp Int)
variableParser = do
		   v <- identifier lis 
		   return (Var v)

uminusParser :: Parser (Exp Int)
uminusParser = do 
		reservedOp lis "-" (este parser devuelve -)
		e <- .... ( e :: Exp Int)
		return (UMinus e)

plusOp :: Parser (Exp Int -> Exp Int -> Exp Int)
plusOp = do 
    reservedOp lis "+"
    return Plus

minusOp :: Parser (Exp Int -> Exp Int -> Exp Int)
minusOp = do 
    reservedOp lis "-"
    return Minus

timesOp  :: Parser (Exp Int -> Exp Int -> Exp Int)
timesOp = do 
    reservedOp lis "*"
    return Times

divOp :: Parser (Exp Int -> Exp Int -> Exp Int)
divOp = do 
    reservedOp lis "/"
    return Div

varincOp :: Parser (Exp Int -> Exp Int)
varinc = do 
    reservedOp lis "++"
    return VarInc

vardecOp :: Parser (Exp Int -> Exp Int)
vardec = do 
    reservedOp lis "--"
    return VarDec 

-----------------------------------
--- Parser de expresiones enteras
-----------------------------------
intfactor ::
intfactor = 

intterm :: 
intterm = chainl1 intfactor (timesOp <|> divOp)

intexp :: Parser (Exp Int)
intexp = chainl1 intterm (plusOp <|> minusOp)

------------------------------------
--- Parser de expresiones booleanas
------------------------------------

boolexp :: Parser (Exp Bool)
boolexp = undefined

-----------------------------------
--- Parser de comandos
-----------------------------------

comm :: Parser Comm
comm = undefined

------------------------------------
-- Función de parseo para tests
------------------------------------
parseIntExp :: SourceName -> String -> Either ParseError Exp
parseIntExp = parse (totParser intexp)

------------------------------------
-- Función de parseo para tests
------------------------------------
parseBoolExp :: SourceName -> String -> Either ParseError Exp
parseBoolExp = parse (totParser boolexp)

------------------------------------
-- Función de parseo
------------------------------------
parseComm :: SourceName -> String -> Either ParseError Comm
parseComm = parse (totParser comm)
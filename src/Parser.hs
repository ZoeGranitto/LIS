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

-- lo saque de parsing.lhs
natParser :: Parser (Exp Int)
natParser = do xs <- many1 digit
               n <- read xs
               return (Const n)

varParser :: Parser (Exp Int)
varParser = do v <- identifier lis 
		                return (Var v)

varIncParser :: Parser (Exp Int)
varIncParser = do v <- identifier lis
                  symbol "++"
                  return (VarInc v)

varDecParser :: Parser (Exp Int)
varDecParser = do v <- identifier lis
                  symbol "--"
                  return (VarDec v)

variableParser :: Parser (Exp Int)
variableParser = try varInc <|> try varDecParser <|> varParser
-- en el utlimo hace falta el try? o los va chequeando en orden?
-- capaz directamente ahi puedo meter la el do <- ident... y no hacerlo aparte

uminusParser :: Parser (Exp Int)
uminusParser = do 
		reservedOp lis "-" --(este parser devuelve -)
		e <- natParser --( e :: Exp Int)
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

seqOp :: Parser (Comm -> Comm -> Comm)
seqOp = do reservedOp lis ";"
           return Seq

skipOp :: Parser Comm
skipOp = do reservedOp lis "skip"
            return Skip

repeatUntil :: Parser Comm
repeatUntil = do
  symbol "repeat"
  c <- comm  -- Parser del comando dentro del repeat
  symbol "until"
  cond <- expr -- Parser de la condición booleana
  return (RepeatUntil c cond)

ifThenElse :: Parser Comm
ifThenElse = do symbol "if"
                e <- negexpr
                symbol "then"
                c1 <- comm
                symbol "else"
                c2 <- comm
                return (IfThenElse e c1 c2)

eqOp :: Parser (Exp -> Exp -> Exp)
eqOp = do symbol "=="
           return Eq

ltOp :: Parser (Exp -> Exp -> Exp)
ltOp = do symbol "<"
           return Lt

gtOp :: Parser (Exp -> Exp -> Exp)
gtOp = do symbol ">"
           return Gt

neqOp :: Parser (Exp -> Exp -> Exp)
neqOp = do symbol "!="
              return NEq

andOp :: Parser (Exp -> Exp -> Exp)
andOp = do symbol "&&"
          return And

orOp :: Parser (Exp -> Exp -> Exp)
orOp = do symbol "||"
         return Or

notOp :: Parser (Exp -> Exp)
notOp = do symbol "!"
         return Not

-----------------------------------
--- Parser de expresiones enteras
-----------------------------------

-- aca dnde corno meto las variables?
-- eso va? o es cualk?
intvar :: Parser (Exp Int)
intvar = chainl1 natParser variableParser

intfactor :: Parser (Exp Int)
intfactor = uminusParser <|> intvar 

intterm :: Parser (Exp Int)
intterm = chainl1 intfactor (timesOp <|> divOp)

intexp :: Parser (Exp Int)
intexp = chainl1 intterm (plusOp <|> minusOp)

------------------------------------
--- Parser de expresiones booleanas
------------------------------------
-- no se si este va a funcar por el tema de (exp int) y (exp bool)
compexp :: Parser (Exp Bool)
compexp = chainl1 intexp (ltOp <|> gtOp <|> eqOp <|> neqOp)

boolfactor :: Parser (Exp Bool)
boolfactor = compexp <|> notOp

boolterm :: Parser (Exp Bool)
boolterm = chainl1 boolfactor andOp

boolexp :: Parser (Exp Bool)
boolexp = chainl1 boolterm orOp

-----------------------------------
--- Parser de comandos
-----------------------------------
-- Parser general para comandos
command :: Parser Comm
command = skipParser
      <|> letin
      <|> repeatUntil
      <|> ifThenElse

comm :: Parser Comm
comm = chainl1 command seqOp

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
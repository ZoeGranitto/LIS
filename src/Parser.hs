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
natParser = do n <- natural lis
               return (Const (fromInteger n))

varParser :: Parser (Exp Int)
varParser = do v <- identifier lis 
               return (Var v)

varIncParser :: Parser (Exp Int)
varIncParser = do v <- varParser
                  reservedOp lis "++"
                  return (VarInc v)

varDecParser :: Parser (Exp Int)
varDecParser = do v <- varParser
                  reservedOp lis "--"
                  return (VarDec v)

variableParser :: Parser (Exp Int)
variableParser = try varIncParser <|> try varDecParser <|> varParser
-- en el utlimo hace falta el try? o los va chequeando en orden?
-- capaz directamente ahi puedo meter la el do <- ident... y no hacerlo aparte

uminusParser :: Parser (Exp Int)
uminusParser = do reservedOp lis "-"
                  e <- intexp
                  return (UMinus e)

plusOp :: Parser (Exp Int -> Exp Int -> Exp Int)
plusOp = do reservedOp lis "+"
            return Plus

minusOp :: Parser (Exp Int -> Exp Int -> Exp Int)
minusOp = do reservedOp lis "-"
             return Minus

timesOp  :: Parser (Exp Int -> Exp Int -> Exp Int)
timesOp = do reservedOp lis "*"
             return Times

divOp :: Parser (Exp Int -> Exp Int -> Exp Int)
divOp = do reservedOp lis "/"
           return Div

seqOp :: Parser (Comm -> Comm -> Comm)
seqOp = do reservedOp lis ";"
           return Seq

------------------------------------------

skipOp :: Parser Comm
skipOp = do reservedOp lis "skip"
            return Skip

repeatUntil :: Parser Comm
repeatUntil = do
  reservedOp lis "repeat"
  c <- braces lis comm  -- Parser del comando dentro del repeat
  reservedOp lis "until"
  cond <- boolexp -- Parser de la condición booleana
  return (RepeatUntil c cond)

ifThenElse :: Parser Comm
ifThenElse = do reservedOp lis "if"
                e <- boolexp
                c1 <- braces lis comm
                do reservedOp lis "else" 
                   c2 <- braces lis comm
                   return (IfThenElse e c1 c2)
                   <|> return (IfThenElse e c1 Skip)

letIn:: Parser Comm
letIn = do v <- identifier lis
           reservedOp lis "="
           u <- intexp
           return (Let v u)
------------------------------------------------------

eqOp :: Parser (Exp Bool)
eqOp = do a <- intexp
          reservedOp lis "=="
          b <- intexp
          return (Eq a b)

ltOp :: Parser (Exp Bool)
ltOp = do a <- intexp
          reservedOp lis "<"
          b <- intexp
          return (Lt a b)

gtOp :: Parser (Exp Bool)
gtOp = do a <- intexp
          reservedOp lis ">"
          b <- intexp
          return (Gt a b)

neqOp :: Parser (Exp Bool)
neqOp = do a <- intexp
           reservedOp lis "!="
           b <- intexp
           return (NEq a b)

andOp :: Parser (Exp Bool -> Exp Bool -> Exp Bool)
andOp = do reservedOp lis "&&"
           return And

orOp :: Parser (Exp Bool -> Exp Bool -> Exp Bool)
orOp = do reservedOp lis "||"
          return Or

notOp :: Parser (Exp Bool)
notOp = do reservedOp lis "!"
           b <- boolexp
           return (Not b)

-----------------------------------
--- Parser de expresiones enteras
-----------------------------------

intfactor :: Parser (Exp Int)
intfactor = uminusParser <|> (natParser <|> variableParser) <|> parens lis intexp

intterm :: Parser (Exp Int)
intterm = chainl1 intfactor (timesOp <|> divOp)

intexp :: Parser (Exp Int)
intexp = chainl1 intterm (plusOp <|> minusOp)

------------------------------------
--- Parser de expresiones booleanas
------------------------------------
-- no se si este va a funcar por el tema de (exp int) y (exp bool)
comp :: Parser (Exp Bool)
comp = ltOp
      <|> gtOp
      <|> eqOp
      <|> neqOp

boolfactor :: Parser (Exp Bool)
boolfactor = notOp <|> comp <|> parens lis boolexp

boolterm :: Parser (Exp Bool)
boolterm = chainl1 boolfactor andOp

boolexp :: Parser (Exp Bool)
boolexp = chainl1 boolterm orOp

-----------------------------------
--- Parser de comandos
-----------------------------------
-- Parser general para comandos
command :: Parser Comm
command = skipOp
      <|> letIn
      <|> repeatUntil
      <|> ifThenElse

comm :: Parser Comm
comm = chainl1 command seqOp

------------------------------------
-- Función de parseo para tests
------------------------------------
parseIntExp :: SourceName -> String -> Either ParseError (Exp Int)
parseIntExp = parse (totParser intexp)

------------------------------------
-- Función de parseo para tests
------------------------------------
parseBoolExp :: SourceName -> String -> Either ParseError (Exp Bool)
parseBoolExp = parse (totParser boolexp)

------------------------------------
-- Función de parseo
------------------------------------
parseComm :: SourceName -> String -> Either ParseError Comm
parseComm = parse (totParser comm)
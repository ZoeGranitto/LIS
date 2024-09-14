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
--- Parsers auxiliares para Exp Int
-----------------------------------
natParser :: Parser (Exp Int)
natParser = do n <- natural lis
               return (Const (fromInteger n))

varParser :: Parser (Exp Int)
varParser = do v <- identifier lis 
               return (Var v)

varIncDecParser :: Parser (Exp Int)
varIncDecParser = do (Var v) <- varParser
                     do reservedOp lis "++"
                        return (VarInc v)
                        <|> do reservedOp lis "--"
                               return (VarDec v)

variableParser :: Parser (Exp Int)
variableParser = try varIncDecParser <|> varParser

uminusParser :: Parser (Exp Int)
uminusParser = do reservedOp lis "-"
                  e <- intexp
                  return (UMinus e)

------------------------------------
--- Parsers auxiliares para Exp Bool
------------------------------------
notParser :: Parser (Exp Bool)
notParser = do reservedOp lis "!"
               b <- boolexp
               return (Not b)

---------------------------------
--- Parsers auxiliares para Comm
---------------------------------
skip :: Parser Comm
skip = do reservedOp lis "skip"; return Skip

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
                   <|> return (IfThen e c1)

letIn:: Parser Comm
letIn = do v <- identifier lis
           reservedOp lis "="
           u <- intexp
           return (Let v u)

-----------------------------------
--- Funciones que reconocen operadores
-----------------------------------
plusMinusOp :: Parser (Exp Int -> Exp Int -> Exp Int)
plusMinusOp = do { reservedOp lis "+"; return Plus  }
          <|> do { reservedOp lis "-"; return Minus }

timesDivOp :: Parser (Exp Int -> Exp Int -> Exp Int)
timesDivOp  = do { reservedOp lis "*"; return Times }
          <|> do { reservedOp lis "/"; return Div   }

andOp :: Parser (Exp Bool -> Exp Bool -> Exp Bool)
andOp = do reservedOp lis "&&"; return And

orOp :: Parser (Exp Bool -> Exp Bool -> Exp Bool)
orOp = do reservedOp lis "||"; return Or

seqOp :: Parser (Comm -> Comm -> Comm)
seqOp = do reservedOp lis ";"; return Seq

compOp :: Parser (Exp Int -> Exp Int -> Exp Bool)
compOp = do { reservedOp lis "=="; return Eq  }
     <|> do { reservedOp lis "!="; return NEq }
     <|> do { reservedOp lis "<" ; return Lt  }
     <|> do { reservedOp lis ">" ; return Gt  }

-----------------------------------
--- Parser de expresiones enteras
-----------------------------------
intfactor :: Parser (Exp Int)
intfactor = try (parens lis intexp)
            <|> try uminusParser 
            <|> try natParser 
            <|> try variableParser

intterm :: Parser (Exp Int)
intterm = chainl1 intfactor timesDivOp

intexp :: Parser (Exp Int)
intexp = chainl1 intterm plusMinusOp

------------------------------------
--- Parser de expresiones booleanas
------------------------------------
comparisons :: Parser (Exp Bool)
comparisons = do a <- intexp
                 operator <- compOp
                 b <- intexp
                 return (operator a b)

boolfactor :: Parser (Exp Bool)
boolfactor = try notParser 
         <|> try comparisons 
         <|> try (parens lis boolexp) 
         <|> do reservedOp lis "true"; return BTrue
         <|> do reservedOp lis "false"; return BFalse


boolterm :: Parser (Exp Bool)
boolterm = chainl1 boolfactor andOp

boolexp :: Parser (Exp Bool)
boolexp = chainl1 boolterm orOp

-----------------------------------
--- Parser de comandos
-----------------------------------
-- Parser general para comandos
command :: Parser Comm
command = skip
      <|> letIn
      <|> repeatUntil
      <|> ifThenElse

comm :: Parser Comm
comm = chainl1 command seqOp

------------------------------------
-- Función de parseo
------------------------------------
parseComm :: SourceName -> String -> Either ParseError Comm
parseComm = parse (totParser comm)
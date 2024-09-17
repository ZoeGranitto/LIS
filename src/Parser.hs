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
-- Parser de números naturales.
natParser :: Parser (Exp Int)
natParser = do n <- natural lis
               return (Const (fromInteger n))

-- Parser de variables
varParser :: Parser (Exp Int)
varParser = do v <- identifier lis 
               return (Var v)

-- Parser de inc y dec.
varIncDecParser :: Parser (Exp Int)
varIncDecParser = do (Var v) <- varParser
                     do reservedOp lis "++"
                        return (VarInc v)
                        <|> do reservedOp lis "--"
                               return (VarDec v)

-- Parser general de variables, maneja varInc y varDec.
variableParser :: Parser (Exp Int)
variableParser = try varIncDecParser <|> varParser

-- Parser para números negativos.
uminusParser :: Parser (Exp Int)
uminusParser = do reservedOp lis "-"
                  e <- intexp
                  return (UMinus e)

------------------------------------
--- Parsers auxiliares para Exp Bool
------------------------------------
-- Parser para el operador '!'
notParser :: Parser (Exp Bool)
notParser = do reservedOp lis "!"
               b <- boolfactor
               return (Not b)

-- Parser de operadores booleanos que reciben expresiones enteras. 
comparisons :: Parser (Exp Bool)
comparisons = do a <- intexp
                 operator <- compOp
                 b <- intexp
                 return (operator a b)

---------------------------------
--- Parsers auxiliares para Comm
---------------------------------
-- Praser para el comando 'Skip'.
skip :: Parser Comm
skip = do reservedOp lis "skip"; return Skip

-- Parser para el comando 'repeat-until'-
repeatUntil :: Parser Comm
repeatUntil = do
  reservedOp lis "repeat"
  c <- braces lis comm  -- Parsea el comando dentro del repeat
  reservedOp lis "until"
  cond <- boolexp -- Parsea la condición booleana
  return (RepeatUntil c cond)

-- Parser para el comando 'if-then-else'.
ifThenElse :: Parser Comm
ifThenElse = do reservedOp lis "if"
                e <- boolexp
                c1 <- braces lis comm
                do reservedOp lis "else" 
                   c2 <- braces lis comm
                   return (IfThenElse e c1 c2)
                   <|> return (IfThen e c1)

-- Parser para la asignación let.
letIn:: Parser Comm
letIn = do v <- identifier lis
           reservedOp lis "="
           u <- intexp
           return (Let v u)

-----------------------------------
--- Funciones que reconocen operadores
-----------------------------------
-- Parser para los operadores '+' y '-'
plusMinusOp :: Parser (Exp Int -> Exp Int -> Exp Int)
plusMinusOp = do { reservedOp lis "+"; return Plus  }
          <|> do { reservedOp lis "-"; return Minus }

-- Parser para los operadores '*' y '/'
timesDivOp :: Parser (Exp Int -> Exp Int -> Exp Int)
timesDivOp  = do { reservedOp lis "*"; return Times }
          <|> do { reservedOp lis "/"; return Div   }

-- Parser para el operador '&&'
andOp :: Parser (Exp Bool -> Exp Bool -> Exp Bool)
andOp = do reservedOp lis "&&"; return And

-- Parser para el operador '||'
orOp :: Parser (Exp Bool -> Exp Bool -> Exp Bool)
orOp = do reservedOp lis "||"; return Or

-- Parser para el operador de secuenciación de comandos ';'
seqOp :: Parser (Comm -> Comm -> Comm)
seqOp = do reservedOp lis ";"; return Seq

-- Parser para los operadores binarios de comparación de booleanos.
compOp :: Parser (Exp Int -> Exp Int -> Exp Bool)
compOp = do { reservedOp lis "=="; return Eq  }
     <|> do { reservedOp lis "!="; return NEq }
     <|> do { reservedOp lis "<" ; return Lt  }
     <|> do { reservedOp lis ">" ; return Gt  }

-----------------------------------
--- Parser de expresiones enteras
-----------------------------------
-- Parser que maneja la presencia de paréntesis, números enteros o variables.
intfactor :: Parser (Exp Int)
intfactor = (parens lis intexp)
            <|> uminusParser 
            <|> natParser 
            <|> variableParser

-- Parser para expresiones enteras que maneja multiplicación y división.
-- Parseamos con chainl1 multiplicaciones y divisiones entre expresiones enteras,
-- las cuales se parsean con intfactor.
intterm :: Parser (Exp Int)
intterm = chainl1 intfactor timesDivOp

-- Parser para expresiones enteras que maneja suma y resta.
-- Parseamos con chainl1 sumas y restas entre expresiones enteras,
-- las cuales se parsean con intterm.
intexp :: Parser (Exp Int)
intexp = chainl1 intterm plusMinusOp

------------------------------------
--- Parser de expresiones booleanas
------------------------------------
-- Parser para expresiones boolenas que maneja los operadores
-- de comparación, los paréntesis y valores booleanos
-- (true y false).
boolfactor :: Parser (Exp Bool)
boolfactor = comparisons 
         <|> (parens lis boolexp) 
         <|> do reservedOp lis "true"; return BTrue
         <|> do reservedOp lis "false"; return BFalse

-- Parser para expresiones booleanas que maneja conjunción.
-- Parseamos con chainl1 el operador '&&' entre expresiones booleanas,
--las cuales se parsean con boolfactor.
boolterm :: Parser (Exp Bool)
boolterm = chainl1 (notParser <|> boolfactor) andOp

-- Parser para expresiones booleanas que maneja disyunción.
-- Parseamos con chainl1 el operador '||' entre expresiones booleanas,
--las cuales se parsean con boolterm.
boolexp :: Parser (Exp Bool)
boolexp = chainl1 boolterm orOp

------------------------------------
--- Parser de comandos
------------------------------------
-- Llama a los distintos parsers de comandos según corresponda.
command :: Parser Comm
command = skip
      <|> letIn
      <|> repeatUntil
      <|> ifThenElse

-- Parser general para comandos
comm :: Parser Comm
comm = chainl1 command seqOp

------------------------------------
-- Función de parseo
------------------------------------
parseComm :: SourceName -> String -> Either ParseError Comm
parseComm = parse (totParser comm)
module Eval2
  ( eval
  , State
  )
where

import           AST
import qualified Data.Map.Strict as M
import           Data.Strict.Tuple

-- Estados
type State = M.Map Variable Int -- la variable es la clave, el entero es el valor

-- Estado vacío
initState :: State
initState = M.empty

-- Busca el valor de una variable en un estado
-- Corrobora que la variable esté definida, en caso contrario
-- devuelve error por UndefVar
lookfor :: Variable -> State -> Either Error Int
lookfor v s = case M.lookup v s of
  Nothing -> Left UndefVar
  Just a  -> Right a

-- Cambia el valor de una variable en un estado
update :: Variable -> Int -> State -> State
update = M.insert

-- Evalúa un programa en el estado vacío
-- Maneja errores.
eval :: Comm -> Either Error State
eval p = stepCommStar p initState

-- Evalúa múltiples pasos de un comnado en un estado,
-- hasta alcanzar un Skip.
-- Maneja errores.
stepCommStar :: Comm -> State -> Either Error State
stepCommStar Skip s = return s
stepCommStar c    s = do
 (c' :!: s') <- stepComm c s
 stepCommStar c' s'

-- Evalúa un paso de un comando en un estado dado.
-- Se define la función por el tipo de comando que recibe.
-- Maneja errores.
stepComm :: Comm -> State -> Either Error (Pair Comm State)
stepComm Skip                 = \s -> Right (Skip :!: s) 
stepComm (Let v e)            = letComm v e
stepComm (Seq c0 c1)          = seqComm c0 c1
stepComm (IfThen b c)         = ifThenElseComm b c Skip
stepComm r@(RepeatUntil c b)  = \s -> Right (Seq c (IfThenElse b Skip r) :!: s)
stepComm (IfThenElse b c0 c1) = ifThenElseComm b c0 c1

-- Evalúa la expresión en el estado recibido, modifica el estado global
-- y devuelve el comando skip con el nuevo estado.
-- Maneja errores.
letComm :: Variable -> (Exp Int) -> State -> Either Error (Pair Comm State)
letComm v e s = case evalExp e s of
  Left e -> Left e
  Right (n :!: s') -> (Right (Skip :!: s'')) where s'' = update v n s'

-- Si el primer comando es skip, retorna el segundo comando, con el mismo estado
-- En caso contrario, evalúa el primer comando, lo reemplaza en el comando Seq, 
-- y actualiza el estado.
-- Maneja errores.
seqComm :: Comm -> Comm -> State -> Either Error (Pair Comm State)
seqComm Skip c1 s = Right (c1 :!: s)
seqComm c0 c1 s = case stepComm c0 s of
  Left e -> Left e
  Right (c0' :!: s') -> Right (Seq c0' c1 :!: s')

-- Evalúa la expresión booleana b en el estado recibido.
-- Retorna uno de los comandos junto al nuevo estado, según el resultado 
-- de evaluar la expresión booleana.
-- Maneja errores.
ifThenElseComm :: (Exp Bool) -> Comm -> Comm -> State -> Either Error (Pair Comm State)
ifThenElseComm b c0 c1 s = case evalExp b s of 
  Left e -> Left e
  Right (b' :!: s') -> (Right (c' :!: s')) where c' = if b' then c0 else c1

-- Evalúa una expresión entera o booleana.
-- Se define la función por el tipo de expresión que recibe.
-- Maneja errores.
evalExp :: Exp a -> State -> Either Error (Pair a State)
evalExp BTrue         s = Right (True  :!: s)
evalExp BFalse        s = Right (False :!: s)
evalExp (Const  n)    s = Right (n :!: s) 
evalExp (Not p)       s = case evalExp p s of
                            Left e -> Left e
                            Right (b :!: s') -> (Right (not b :!: s'))
evalExp (Var    x)    s = case lookfor x s of 
                            Left e -> Left e
                            Right a -> Right (a :!: s)
evalExp (UMinus e)    s = case evalExp e s of
                            Left e -> Left e
                            Right (n :!: s') -> (Right (-n :!: s'))
evalExp (VarInc x)    s = incDecExp x (+) s
evalExp (VarDec x)    s = incDecExp x (-) s
evalExp (Lt  e0 e1)   s = auxEvalExp e0 e1 s (<)
evalExp (Gt  e0 e1)   s = auxEvalExp e0 e1 s (>)
evalExp (Eq  e0 e1)   s = auxEvalExp e0 e1 s (==)
evalExp (NEq e0 e1)   s = auxEvalExp e0 e1 s (/=)
evalExp (And p0 p1)   s = auxEvalExp p0 p1 s (&&)
evalExp (Or  p0 p1)   s = auxEvalExp p0 p1 s (||)
evalExp (Plus  e0 e1) s = auxEvalExp e0 e1 s (+)
evalExp (Minus e0 e1) s = auxEvalExp e0 e1 s (-)
evalExp (Times e0 e1) s = auxEvalExp e0 e1 s (*)
evalExp (Div   e0 e1) s = divideExp  e0 e1 s

-- Función auxiliar para evaluar expresiones binarias
auxEvalExp :: Exp a -> Exp a -> State -> (a -> a -> b) -> Either Error (Pair b State)
auxEvalExp a b s f = case evalExp a s of 
  Left e -> Left e
  Right (n0 :!: s') -> case evalExp b s' of 
                        Left e -> Left e
                        Right (n1 :!: s'') -> Right ((f n0 n1) :!: s'')

-- Función auxiliar para evaluar la expresión Div. Maneja errores, 
-- en caso de que se intente dividir por cero, o bien alguna variable
-- no esté definida.
divideExp :: Exp Int -> Exp Int -> State -> Either Error (Pair Int State)
divideExp e0 e1 s = case (evalExp e0 s) of
  Left e -> Left e
  Right (n0 :!: s') -> case evalExp e1 s' of 
                        Left e -> Left e
                        Right (n1 :!: s'') -> if n1 == 0 then Left DivByZero else Right (div n0 n1 :!: s'')

-- Función auxiliar para evaluar las expresiones varinc y vardec.
-- Maneja errores.
incDecExp :: Variable -> (Int -> Int -> Int) -> State -> Either Error (Pair Int State)
incDecExp k f s = case lookfor k s of 
  Left e -> Left e
  Right v -> Right (v' :!: update k v' s) where v' = (f v 1)

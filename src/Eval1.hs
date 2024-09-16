module Eval1
  ( eval
  , State
  )
where

import           AST
import qualified Data.Map.Strict as M
import           Data.Strict.Tuple

-- Estados
type State = M.Map Variable Int

-- Estado vacío
initState :: State
initState = M.empty

-- Busca el valor de una variable en un estado
lookfor :: Variable -> State -> Int
lookfor v s = s M.! v

-- Cambia el valor de una variable en un estado
update :: Variable -> Int -> State -> State
update = M.insert

-- Evalúa un programa en el estado vacío
eval :: Comm -> State
eval p = stepCommStar p initState

-- Evalúa múltiples pasos de un comando en un estado,
-- hasta alcanzar un Skip
stepCommStar :: Comm -> State -> State
stepCommStar Skip s = s
stepCommStar c    s = Data.Strict.Tuple.uncurry stepCommStar $ stepComm c s

-- Evalúa un paso de un comando en un estado dado
-- Se define la función por el tipo de comando que recibe
stepComm :: Comm -> State -> Pair Comm State
stepComm Skip                 = \s -> (Skip :!: s) 
stepComm (Let v e)            = letComm v e
stepComm (Seq c0 c1)          = seqComm c0 c1
stepComm (IfThen b c)         = ifThenElseComm b c Skip
stepComm r@(RepeatUntil c b)  = \s -> (Seq c (IfThenElse b Skip r) :!: s)
stepComm (IfThenElse b c0 c1) = ifThenElseComm b c0 c1

-- Evalúa la expresión en el estado recibido, modifica el estado global
-- y devuelve el comando skip con el nuevo estado
letComm :: Variable -> (Exp Int) -> State -> Pair Comm State
letComm v e s = let (n :!: s') = evalExp e s 
                    s'' = update v n s' 
                in (Skip :!: s'') 

-- Si el primer comando es skip, retorna el segundo comando, con el mismo estado
-- En caso contrario, evalúa el primer comando, lo reemplaza en el comando Seq, 
-- y actualiza el estado.
seqComm :: Comm -> Comm -> State -> Pair Comm State
seqComm Skip c1 s = (c1 :!: s)
seqComm c0   c1 s = (Seq c0' c1 :!: s') where (c0' :!: s') = (stepComm c0 s)

-- Evalúa la expresión booleana b en el estado recibido.
-- Retorna uno de los comandos junto al nuevo estado, según el resultado 
-- de evaluar la expresión booleana.
ifThenElseComm :: (Exp Bool) -> Comm -> Comm -> State -> Pair Comm State
ifThenElseComm b c0 c1 s = let (b' :!: s') = evalExp b s
                               c' = if b' then c0 else c1
                           in (c' :!: s')

-- Evalúa una expresión entera o booleana
-- Se define la función por el tipo de expresión que recibe
evalExp :: Exp a -> State -> Pair a State
evalExp BTrue         s = (True  :!: s)
evalExp BFalse        s = (False :!: s)
evalExp (Const  n)    s = (n :!: s) 
evalExp (Var    x)    s = (lookfor x s :!: s) 
evalExp (Not p)       s = (not b :!: s') where (b :!: s') = evalExp p s
evalExp (UMinus e)    s = (-n :!: s')    where (n :!: s') = evalExp e s
evalExp (VarInc x)    s = incDecExp x (+) s
evalExp (VarDec x)    s = incDecExp x (-) s
evalExp (Lt  e0 e1)   s = auxEvalExp e0 e1 s (<)
evalExp (Gt  e0 e1)   s = auxEvalExp e0 e1 s (>)
evalExp (Eq  e0 e1)   s = auxEvalExp e0 e1 s (==)
evalExp (NEq e0 e1)   s = auxEvalExp e0 e1 s (/=)
evalExp (And p0 p1)   s = auxEvalExp p0 p1 s (&&)
evalExp (Or  p0 p1)   s = auxEvalExp p0 p1 s (||)
evalExp (Plus  e0 e1) s = auxEvalExp e0 e1 s (+ )
evalExp (Minus e0 e1) s = auxEvalExp e0 e1 s (-)
evalExp (Times e0 e1) s = auxEvalExp e0 e1 s (*)
evalExp (Div   e0 e1) s = auxEvalExp e0 e1 s (div)

-- Función auxiliar para evaluar expresiones binarias
auxEvalExp :: Exp a -> Exp a -> State -> (a -> a -> b) -> Pair b State
auxEvalExp a b s f = let (n0 :!: s') = evalExp a s
                         (n1 :!: s'') = evalExp b s'
                     in ((f n0 n1) :!: s'')

-- Función auxiliar para evaluar las expresiones varinc y vardec
incDecExp :: Variable -> (Int -> Int -> Int) -> State -> Pair Int State
incDecExp k f s = let v = lookfor k s
                      v' = (f v 1) 
                  in (v' :!: update k v' s)

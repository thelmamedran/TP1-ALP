module Eval3
  ( eval
  , State
  )
where

import           AST
import qualified Data.Map.Strict               as M
import           Data.Strict.Tuple

-- Estados 
type State = (M.Map Variable Int, String)

-- Estado vacío
initState :: State
initState = (M.empty, [])

-- Busca el valor de una variable en un estado
lookfor :: Variable -> State -> Either Error Int
lookfor v (s,_) = case M.lookup v s of
                Just n  -> Right n 
                Nothing -> Left UndefVar

-- Cambia el valor de una variable en un estado
update :: Variable -> Int -> State -> State
update v n (s,l) = (M.insert v n s, l)

-- Evalúa un programa en el estado vacío
eval :: Comm -> Either Error State
eval p = stepCommStar p initState

-- Evalúa múltiples pasos de un comando en un estado,
-- hasta alcanzar un Skip
stepCommStar :: Comm -> State -> Either Error State
stepCommStar Skip s = return s
stepCommStar c    s = do
  (c' :!: s') <- stepComm c s
  stepCommStar c' s'

addTrace :: String -> String -> String
addTrace entry l = l ++ (if null l then "" else " ") ++ entry

-- Evalúa un paso de un comando en un estado dado
stepComm :: Comm -> State -> Either Error (Pair Comm State)
stepComm Skip s                 = Right (Skip :!: s)

stepComm (Let v e) (s,l)            = do (n :!: (s', l')) <- evalExp e (s,l)
                                         return (Skip :!: (Prelude.fst (update v n (s',l')), addTrace ("Let " ++ v ++ " " ++ show n) l'))

stepComm (Seq Skip c2) (s,l)        = Right (c2 :!: (s,l))

stepComm (Seq c1 c2) (s,l)          = do (c1' :!: (s',l')) <- stepComm c1 (s,l)
                                         return ((Seq c1' c2) :!: (s',l'))

stepComm (IfThenElse e c1 c2) (s,l) = do (b :!: s') <- evalExp e (s,l)
                                         if b 
                                            then return (c1 :!: s') 
                                            else return (c2 :!: s')

stepComm (RepeatUntil c e) (s,l)    = Right ((Seq c (IfThenElse e Skip (RepeatUntil c e))) :!: (s,l))

-- Evalúa una expresión
evalExp :: Exp a -> State -> Either Error (Pair a State)

evalExp (Const n) s   = Right (n :!: s)

evalExp (Var v) s     = do n <- lookfor v s
                           return (n :!: s)

evalExp (UMinus e) s  = do (n :!: s') <- evalExp e s
                           return ((-n) :!: s')

evalExp (Plus a b) s  = do (n0 :!: s') <- evalExp a s 
                           (n1 :!: s'') <- evalExp b s'
                           return ((n0 + n1) :!: s'')

evalExp (Minus a b) s = do (n0 :!: s') <- evalExp a s 
                           (n1 :!: s'') <- evalExp b s'
                           return ((n0 - n1) :!: s'')

evalExp (Times a b) s = do (n0 :!: s') <- evalExp a s
                           (n1 :!: s'') <- evalExp b s' 
                           return ((n0 * n1) :!: s'')

evalExp (Div a b) s   = do (n0 :!: s') <- evalExp a s
                           (n1 :!: s'') <- evalExp b s'
                           if n1 == 0
                            then Left DivByZero
                            else return ((div n0 n1) :!: s'')

evalExp (VarInc v) (s,l)  = do n <- lookfor v (s,l)
                               return ((n+1) :!: (Prelude.fst (update v (n+1) (s,l)), addTrace ("Let " ++ v ++ " " ++ show n) l))

evalExp (VarDec v) (s,l)  = do n <- lookfor v (s,l)
                               return ((n-1) :!: (Prelude.fst (update v (n-1) (s,l)), addTrace ("Let " ++ v ++ " " ++ show n) l))

evalExp BTrue s       = Right (True :!: s)

evalExp BFalse s      = Right (False :!: s)

evalExp (Lt a b) s    = do (p0 :!: s') <- evalExp a s 
                           (p1 :!: s'') <- evalExp b s'
                           return ((p0 < p1) :!: s'')

evalExp (Gt a b) s    = do (p0 :!: s') <- evalExp a s
                           (p1 :!: s'') <- evalExp b s'
                           return ((p0 > p1) :!: s'')

evalExp (Eq a b) s    = do (p0 :!: s') <- evalExp a s
                           (p1 :!: s'') <- evalExp b s'
                           return ((p0 == p1) :!: s'')

evalExp (NEq a b) s   = do (p0 :!: s') <- evalExp a s
                           (p1 :!: s'') <- evalExp b s' 
                           return ((p0 /= p1) :!: s'')

evalExp (And a b) s   = do (p0 :!: s') <- evalExp a s
                           (p1 :!: s'') <- evalExp b s'
                           return ((p0 && p1) :!: s'')

evalExp (Or a b) s    = do (p0 :!: s') <- evalExp a s
                           (p1 :!: s'') <- evalExp b s'
                           return ((p0 || p1) :!: s'')

evalExp (Not p) s     = do (b :!: s') <- evalExp p s
                           return ((not b) :!: s')
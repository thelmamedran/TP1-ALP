module Eval2
  ( eval
  , State
  )
where

import           AST
import qualified Data.Map.Strict               as M
import           Data.Strict.Tuple

-- Estados
type State = M.Map Variable Int

-- Estado vacío
initState :: State
initState = M.empty

-- Busca el valor de una variable en un estado
lookfor :: Variable -> State -> Either Error Int
lookfor v s = case M.lookup v s of
                Just n  -> Right n 
                Nothing -> Left UndefVar

-- Cambia el valor de una variable en un estado
update :: Variable -> Int -> State -> State
update = M.insert

-- Evalúa un programa en el estado vacío
eval :: Comm -> Either Error State
eval p = stepCommStar p initState

-- Evalúa múltiples pasos de un comnado en un estado,
-- hasta alcanzar un Skip
stepCommStar :: Comm -> State -> Either Error State
stepCommStar Skip s = return s
stepCommStar c    s = do
  (c' :!: s') <- stepComm c s
  stepCommStar c' s'

-- Evalúa un paso de un comando en un estado dado
stepComm :: Comm -> State -> Either Error (Pair Comm State)
stepComm Skip s                 = Right (Skip :!: s)

stepComm (Let v e) s            = do (n :!: s') <- evalExp e s
                                     return (Skip :!: (update v n s'))

stepComm (Seq Skip c2) s        = Right (c2 :!: s)

stepComm (Seq c1 c2) s          = do (c1' :!: s') <- stepComm c1 s
                                     return ((Seq c1' c2) :!: s' )

stepComm (IfThenElse e c1 c2) s = do (b :!: s') <- evalExp e s
                                     if b 
                                      then return (c1 :!: s') 
                                      else return (c2 :!: s')

stepComm (RepeatUntil c e) s    = Right ((Seq c (IfThenElse e Skip (RepeatUntil c e))) :!: s)

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

evalExp (VarInc v) s  = do n <- lookfor v s
                           return ((n+1) :!: (update v (n+1) s))

evalExp (VarDec v) s  = do n <- lookfor v s
                           return ((n-1) :!: (update v (n-1) s))

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
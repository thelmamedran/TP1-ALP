module Eval1
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
-- Completar la definición
initState :: State
initState = M.empty

-- Busca el valor de una variable en un estado
-- Completar la definición
lookfor :: Variable -> State -> Int
lookfor v s = case M.lookup v s of
                Just n  -> n
                Nothing -> error ("variable no definida: " ++ v)

-- Cambia el valor de una variable en un estado
-- Completar la definición
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
-- Completar la definición
stepComm :: Comm -> State -> Pair Comm State
stepComm Skip s = Skip :!: s
stepComm (Let v expint) s = let (n :!: s') = evalExp expint s
                                  in Skip :!: (update v n s')
stepComm (Seq Skip comm2) s = comm2 :!: s
stepComm (Seq comm1 comm2) s = let (comm1' :!: s') = stepComm comm1 s
                                    in (Seq comm1' comm2) :!: s' 
stepComm (IfThenElse expbool comm1 comm2) s = let (b :!: s') = evalExp expbool s
                                  in if b then comm1 :!: s' else comm2 :!: s'
                          
stepComm (RepeatUntil comm expbool) s = (Seq comm (IfThenElse expbool Skip (RepeatUntil comm expbool))) :!: s

-- Evalúa una expresión
-- Completar la definición
evalExp :: Exp a -> State -> Pair a State
evalExp (Const n) s = n :!: s
evalExp (Var v) s = (lookfor v s) :!: s
evalExp (UMinus expint) s = let (n :!: s') = evalExp expint s
                               in (-n) :!: s'
evalExp (Plus a b) s = let (n0 :!: s') = evalExp a s 
                           (n1 :!: s'') = evalExp b s'
                            in (n0 + n1) :!: s''
evalExp (Minus a b) s = let (n0 :!: s') = evalExp a s 
                            (n1 :!: s'') = evalExp b s'
                              in (n0 - n1) :!: s''
evalExp (Times a b) s = let (n0 :!: s') = evalExp a s
                            (n1 :!: s'') = evalExp b s' 
                              in (n0 * n1) :!: s''
evalExp (Div a b) s = let (n0 :!: s') = evalExp a s
                          (n1 :!: s'') = evalExp b s'
                            in (div n0 n1) :!: s''
evalExp (VarInc v) s = let n = lookfor v s
                             in (n+1) :!: (update v (n+1) s)
evalExp (VarDec v) s = let n = lookfor v s
                             in (n-1) :!: (update v (n-1) s)
evalExp BTrue s = True :!: s
evalExp BFalse s = False :!: s
evalExp (Lt a b) s = let (p0 :!: s') = evalExp a s 
                         (p1 :!: s'') = evalExp b s'
                              in (p0 < p1) :!: s''
evalExp (Gt a b) s = let (p0 :!: s') = evalExp a s
                         (p1 :!: s'') = evalExp b s'
                              in (p0 > p1) :!: s''
evalExp (Eq a b) s = let (p0 :!: s') = evalExp a s
                         (p1 :!: s'') = evalExp b s'
                              in (p0 == p1) :!: s''
evalExp (NEq a b) s = let (p0 :!: s') = evalExp a s
                          (p1 :!: s'') = evalExp b s' 
                              in (p0 /= p1) :!: s''
evalExp (And a b) s = let (p0 :!: s') = evalExp a s
                          (p1 :!: s'') = evalExp b s'
                              in (p0 && p1) :!: s''
evalExp (Or a b) s = let (p0 :!: s') = evalExp a s
                         (p1 :!: s'') = evalExp b s'
                              in (p0 || p1) :!: s''
evalExp (Not p) s = let (b :!: s') = evalExp p s
                          in (not b) :!: s'
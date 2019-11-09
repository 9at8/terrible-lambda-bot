module LambdaCalculus
  ( Primitive
  , Term
  , lambdaTrue
  , lambdaFalse
  , SContents
  , EContents
  , CContents
  , DContents
  , SECDConfig
  , secdReduce
  , secdInit
  , secdResultToTerm
  ) where

data Primitive = Succ | IsZero deriving Show

data Term = Var String | Abs String Term | App Term Term
          | Prim Primitive | INT Int

instance Show Term where
  show (Var x) = x
  show (Abs x e) = "(\\" ++ x ++ "." ++ show e ++ ")"
  show (App e1 e2) = "(" ++ show e1 ++ " " ++ show e2 ++ ")"
  show (Prim p) = show p
  show (INT x) = show x

lambdaTrue :: Term
lambdaTrue = Abs "x" (Abs "y" (Var "x"))

lambdaFalse :: Term
lambdaFalse = Abs "x" (Abs "y" (Var "y"))

applyPrim :: Primitive -> Term -> Term
applyPrim (Succ) (INT x) = INT (x + 1)
applyPrim (IsZero) (INT 0) = lambdaTrue
applyPrim (IsZero) (INT _) = lambdaFalse

data SContents = Closure String Term [EContents] | STerm Term deriving Show
data EContents = Env String SContents deriving Show
data CContents = Apply | Comp Term deriving Show
data DContents = Ctx [SContents] [EContents] [CContents] deriving Show

data SECDConfig = SECD ([SContents], [EContents], [CContents], [DContents])
                deriving Show

lookUp :: String -> [EContents] -> SContents
lookUp x [] = error (x ++ ": Couldn't find variable in environment.")
lookUp x ((Env e t) : es) = if x == e then t else lookUp x es

secdOneStep :: SECDConfig -> SECDConfig
secdOneStep (SECD (s, e, Comp (Var x) : c', d)) =
  SECD ((lookUp x e) : s, e, c', d)
secdOneStep (SECD (s, e, Comp (Abs x m) : c', d)) =
  SECD ((Closure x m e) : s, e, c', d)
secdOneStep (SECD (s, e, Comp (App m n) : c', d)) =
  SECD (s, e, (Comp n : Comp m : Apply : c'), d)
secdOneStep (SECD (s, e, Comp i@(INT _) : c', d)) =
  SECD (STerm i : s, e, c', d)
secdOneStep (SECD (s, e, Comp p@(Prim _) : c', d)) =
  SECD (STerm p : s, e, c', d)
secdOneStep (SECD (STerm (Prim p) : STerm n : s', e, Apply : c', d)) =
  SECD (s', e, Comp (applyPrim p n) : c', d)
secdOneStep (SECD (Closure x m e1 : n : s', e, Apply : c', d)) =
  SECD ([], Env x n : e1, [Comp m], Ctx s' e c' : d)
secdOneStep (SECD (m : [], e, [], Ctx s' e' c' : d')) =
  SECD (m : s', e', c', d')
secdOneStep result@(SECD (m : [], e, [], [])) = result
secdOneStep s = error ("Something bad happened...\n" ++ show s)

secdReduce :: SECDConfig -> SECDConfig
secdReduce result@(SECD (_ : [], _, [], [])) = result
secdReduce s = secdReduce (secdOneStep s)

substituteIn :: EContents -> Term -> Term
substituteIn (Env y s) v@(Var x)
  | x == y    = stackToTerm s
  | otherwise = v
substituteIn subs@(Env y _) abs@(Abs x m)
  | x == y     = abs
  | otherwise  = Abs x (subs `substituteIn` m)
substituteIn subs (App m n) =
  App (subs `substituteIn` m) (subs `substituteIn` n)
substituteIn _ t = t

substituteAll :: [EContents] -> Term -> Term
substituteAll [] t = t
substituteAll (e : es) t = substituteAll es (e `substituteIn` t)

stackToTerm :: SContents -> Term
stackToTerm (STerm t) = t
stackToTerm (Closure x m e) = Abs x (substituteAll e m)

secdResultToTerm :: SECDConfig -> Term
secdResultToTerm (SECD (m : _, _, _, _)) = stackToTerm m

secdInit :: Term -> SECDConfig
secdInit t = SECD ([], [], [Comp t], [])

reduce :: Term -> Term
reduce t = case secdReduce (secdInit t) of
  SECD (m : [], _, _, _) -> stackToTerm m

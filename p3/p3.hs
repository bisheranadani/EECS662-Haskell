{-# LANGUAGE GADTs #-}

-- Imports for Monads

import Control.Monad

-- FAE AST and Type Definitions

data FAE where
  Num :: Int -> FAE
  Plus :: FAE -> FAE -> FAE
  Minus :: FAE -> FAE -> FAE
  Lambda :: String -> FAE -> FAE
  App :: FAE -> FAE -> FAE
  Id :: String -> FAE
  deriving (Show,Eq)

type Env = [(String,FAE)]

evalDynFAE :: Env -> FAE -> (Maybe FAE)
evalDynFAE env (Num n) = if n < 0 then Nothing else (Just(Num n))

evalDynFAE env (Plus l r) = do{
(Num l') <- (evalDynFAE env l);
(Num r') <- (evalDynFAE env r);
return (Num (l' + r'))
}

evalDynFAE env (Minus l r) = do{
(Num l') <- (evalDynFAE env l);
(Num r') <- (evalDynFAE env r);
return (Num (l' - r'))
}

evalDynFAE env (Lambda i b) = Just (Lambda i b)

evalDynFAE env (App f a) = do{
Lambda i b <- evalDynFAE env f;
a' <- evalDynFAE env a;
evalDynFAE ((i,a'):env) b
}

evalDynFAE env (Id id) = lookup id env

-- evalDynFAE _ _ = Nothing

data FAEValue where
  NumV :: Int -> FAEValue
  ClosureV :: String -> FAE -> Env' -> FAEValue
  deriving (Show,Eq)

type Env' = [(String,FAEValue)]

evalStatFAE :: Env' -> FAE -> (Maybe FAEValue)
evalStatFAE env (Num n) = if n < 0 then Nothing else (Just(NumV n))

evalStatFAE env (Plus l r) = do{
(NumV l') <- (evalStatFAE env l);
(NumV r') <- (evalStatFAE env r);
return (NumV (l' + r'))
}

evalStatFAE env (Minus l r) = do{
(NumV l') <- (evalStatFAE env l);
(NumV r') <- (evalStatFAE env r);
return (NumV (l' - r'))
}

evalStatFAE env (Lambda i b) = Just (ClosureV i b env)

evalStatFAE env (App f a) = do{
ClosureV i b env' <- evalStatFAE env f;
a' <- evalStatFAE env a;
evalStatFAE ((i,a'):env') b
}

evalStatFAE env (Id id) = lookup id env


-- evalStatFAE _ _ = Nothing


-- FBAE AST and Type Definitions

data FBAE where
  NumD :: Int -> FBAE
  PlusD :: FBAE -> FBAE -> FBAE
  MinusD :: FBAE -> FBAE -> FBAE
  LambdaD :: String -> FBAE -> FBAE
  AppD :: FBAE -> FBAE -> FBAE
  BindD :: String -> FBAE -> FBAE -> FBAE
  IdD :: String -> FBAE
  deriving (Show,Eq)

elabFBAE :: FBAE -> FAE
elabFBAE (NumD n) = (Num n)
elabFBAE (PlusD l r) = (Plus (elabFBAE l) (elabFBAE r))
elabFBAE (MinusD l r) = (Minus (elabFBAE l) (elabFBAE r))
elabFBAE (LambdaD i b) = (Lambda i (elabFBAE b))
elabFBAE (AppD f a) = (App (elabFBAE f) (elabFBAE a))
elabFBAE (BindD i v b) = (App (Lambda i (elabFBAE b)) (elabFBAE v))
elabFBAE (IdD id) = (Id id)

-- elabFBAE _ = (Num (-1))

evalFBAE :: Env' -> FBAE -> (Maybe FAEValue)
-- elabFBAE env (NumD n) = evalStatFAE env (elabFBAE (NumD n))
-- elabFBAE env (PlusD l r) = evalStatFAE env (elabFBAE (Plus l r))
-- elabFBAE env (MinusD l r) = evalStatFAE env (elabFBAE (MinusD l r))
evalFBAE env expr = (evalStatFAE env (elabFBAE expr))

-- evalFBAE _ _ = Nothing

-- FBAEC AST and Type Definitions

data FBAEC where
  NumE :: Int -> FBAEC
  PlusE :: FBAEC -> FBAEC -> FBAEC
  MinusE :: FBAEC -> FBAEC -> FBAEC
  TrueE :: FBAEC
  FalseE :: FBAEC
  AndE :: FBAEC -> FBAEC -> FBAEC
  OrE :: FBAEC -> FBAEC -> FBAEC
  NotE :: FBAEC -> FBAEC
  IfE :: FBAEC -> FBAEC -> FBAEC -> FBAEC
  LambdaE :: String -> FBAEC -> FBAEC
  AppE :: FBAEC -> FBAEC -> FBAEC
  BindE :: String -> FBAEC -> FBAEC -> FBAEC
  IdE :: String -> FBAEC
  deriving (Show,Eq)

elabFBAEC :: FBAEC -> FAE
elabFBAEC (NumE n) = (Num n)
elabFBAEC (PlusE l r) = (Plus (elabFBAEC l) (elabFBAEC r))
elabFBAEC (MinusE l r) = (Minus (elabFBAEC l) (elabFBAEC r))
elabFBAEC (TrueE) = (Lambda "t" (Lambda "f" (Id "t")))
elabFBAEC (FalseE) = (Lambda "t" (Lambda "f" (Id "f")))
elabFBAEC (AndE l r) = (App (App (Lambda "l" (Lambda "r" (App (App (Id "l") (Id "r")) (elabFBAEC FalseE)))) (elabFBAEC l)) (elabFBAEC r))
elabFBAEC (OrE l r) = (App (App (Lambda "l" (Lambda "r" (App (App (Id "l") (elabFBAEC TrueE)) (Id "r")))) (elabFBAEC l)) (elabFBAEC r))
elabFBAEC (NotE b) = (App (Lambda "b" (App (App (Id "b") (elabFBAEC FalseE)) (elabFBAEC TrueE))) (elabFBAEC b))
elabFBAEC (IfE c t e) = (Lambda "c" (Lambda "t" (Lambda "e" (App (App (Id "c") (Id "t")) (Id "e")))))
elabFBAEC (LambdaE i b) = (Lambda i (elabFBAEC b))
elabFBAEC (AppE f a) = (App (elabFBAEC f) (elabFBAEC a))
elabFBAEC (BindE i v b) = (App (Lambda i (elabFBAEC b)) (elabFBAEC v))
elabFBAEC (IdE id) = (Id id)

-- elabFBAEC _ = (Num (-1))

evalFBAEC :: Env'-> FBAEC -> Maybe FAEValue
evalFBAEC env expr = (evalStatFAE env (elabFBAEC expr))
-- evalFBAEC _ _ = Nothing

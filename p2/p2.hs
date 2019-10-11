{-# LANGUAGE GADTs,FlexibleContexts #-}

-- Imports for Monads

import Control.Monad

-- BBAE AST and Type Definitions

data TBBAE where
  TNum :: TBBAE
  TBool :: TBBAE
  deriving (Show,Eq)

data BBAE where
  Num :: Int -> BBAE
  Plus :: BBAE -> BBAE -> BBAE
  Minus :: BBAE -> BBAE -> BBAE
  Bind :: String -> BBAE -> BBAE -> BBAE
  Id :: String -> BBAE
  Boolean :: Bool -> BBAE
  And :: BBAE -> BBAE -> BBAE
  Leq :: BBAE -> BBAE -> BBAE
  IsZero :: BBAE -> BBAE
  If :: BBAE -> BBAE -> BBAE -> BBAE
  deriving (Show,Eq)

type Env = [(String,BBAE)]

type Cont = [(String,TBBAE)]

subst :: String -> BBAE -> BBAE -> BBAE
subst _ _ (Num x) = (Num x)
subst i v (Plus l r) = (Plus (subst i v l) (subst i v r))
subst i v (Minus l r) = (Minus (subst i v l) (subst i v r))
subst i v (Bind i' v' b') =
  if i==i' then (Bind i' (subst i v v') b')
    else (Bind i' (subst i v v') (subst i v b'))
subst i v (Id i') = if i==i' then v else (Id i')

evalS :: BBAE -> (Maybe BBAE)
evalS (Num n) = if n < 0 then Nothing else (Just(Num n))

evalS (Plus l r) = do{
(Num l') <- (evalS l);
(Num r') <- (evalS r);
return (Num (l' + r'))
}

evalS (Minus l r) = do{
(Num l') <- (evalS l);
(Num r') <- (evalS r);
if (l' < r') then Nothing else return (Num (l' - r'))
}

evalS (Bind i v b) = do{
v' <- (evalS v);
(evalS (subst i v' b))
}

evalS (Id id) = Nothing

evalS (Boolean b) = (Just (Boolean b))

evalS (And l r) = do{
(Boolean l') <- (evalS l);
(Boolean r') <- (evalS r);
return (Boolean (l' && r'))
}

evalS (Leq l r) = do{
(Num l') <- (evalS l);
(Num r') <- (evalS r);
return (Boolean (l' <= r'))
}

evalS (IsZero v) = do{
(Num v') <- (evalS v);
return (Boolean (v' == 0))
}

evalS (If c t e) = do{
(Boolean c') <- (evalS c);
(if c' then (evalS t) else (evalS e))
}

-- evalS _ = Nothing

evalM :: Env -> BBAE -> (Maybe BBAE)

evalM env (Num n) = if n < 0 then Nothing else (Just(Num n))

evalM env (Plus l r) = do{
(Num l') <- (evalM env l);
(Num r') <- (evalM env r);
return (Num (l' + r'))
}

evalM env (Minus l r) = do{
(Num l') <- (evalM env l);
(Num r') <- (evalM env r);
if (l' < r') then Nothing else return (Num (l' - r'))
}

evalM env (Bind i v b) = do{
v' <- (evalM env v);
(evalM ((i,v'):env) b)
}

evalM env (Id id) = (lookup id env)

evalM env (Boolean b) = (Just (Boolean b))

evalM env (And l r) = do{
(Boolean l') <- (evalM env l);
(Boolean r') <- (evalM env r);
return (Boolean (l' && r'))
}

evalM env (Leq l r) = do{
(Num l') <- (evalM env l);
(Num r') <- (evalM env r);
return (Boolean (l' <= r'))
}

evalM env (IsZero v) = do{
(Num v') <- (evalM env v);
return (Boolean (v' == 0))
}

evalM env (If c t e) = do{
(Boolean c') <- (evalM env c);
(if c' then (evalM env t) else (evalM env e))
}

-- evalM _ _ = Nothing

testBBAE :: BBAE -> Bool
testBBAE x = if (evalS x == evalM [] x) then True else False
-- testBBAE _ = True

typeofM :: Cont -> BBAE -> (Maybe TBBAE)

typeofM cont (Num n) = (Just TNum)

typeofM cont (Plus l r) = do{
l' <- (typeofM cont l);
r' <- (typeofM cont r);
if (l'==TNum && r'==TNum) then return TNum else Nothing
}

typeofM cont (Minus l r) = do{
l' <- (typeofM cont l);
r' <- (typeofM cont r);
if (l'==TNum && r'==TNum) then return TNum else Nothing
}

typeofM cont (Bind i v b) = do{
v' <- typeofM cont v;
typeofM ((i,v'):cont) b
}

typeofM cont (Id id) = (lookup id cont)

typeofM cont (Boolean b) = Just TBool

typeofM cont (And l r) = do{
TBool <- (typeofM cont l);
TBool <- (typeofM cont r);
return TBool
}

typeofM cont (Leq l r) = do{
TNum <- (typeofM cont l);
TNum <- (typeofM cont r);
return TBool
}

typeofM cont (IsZero v) = do{
TNum <- (typeofM cont v);
return TBool
}

typeofM cont (If c t e) = do{
c' <- (typeofM cont c);
t' <- (typeofM cont t);
e' <- (typeofM cont e);
if t'==e' then return t' else Nothing
}

-- typeofM _ _ = Nothing

evalT :: BBAE -> (Maybe BBAE)

evalT x = do{
(typeofM [] x);
(evalM [] x)
}

-- evalT _ = Nothing

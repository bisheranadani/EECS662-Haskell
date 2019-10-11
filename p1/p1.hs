{-# LANGUAGE GADTs, FlexibleContexts #-}

-- Imports for Monads

import Control.Monad

-- AST Definition

data TABE where
  TNum :: TABE
  TBool :: TABE
  deriving (Show,Eq)

data ABE where
  Num :: Int -> ABE
  Plus :: ABE -> ABE -> ABE
  Minus :: ABE -> ABE -> ABE
  Mult :: ABE -> ABE -> ABE
  Div :: ABE -> ABE -> ABE
  Boolean :: Bool -> ABE
  And :: ABE -> ABE -> ABE
  Leq :: ABE -> ABE -> ABE
  IsZero :: ABE -> ABE
  If :: ABE -> ABE -> ABE -> ABE
  deriving (Show,Eq)

-- Evaluation Functions

evalM :: ABE -> (Maybe ABE)

evalM (Num n) = if n < 0 then Nothing else (Just(Num n))

evalM (Plus l r) = do{
(Num l') <- (evalM l);
(Num r') <- (evalM r);
return (Num (l' + r'))
}

evalM (Minus l r) = do{
(Num l') <- (evalM l);
(Num r') <- (evalM r);
if (l' < r') then Nothing else return (Num (l' - r'))
}

evalM (Mult l r) = do{
(Num l') <- (evalM l);
(Num r') <- (evalM r);
return (Num (l' * r'))
}

evalM (Div l r) = do{
(Num l') <- (evalM l);
(Num r') <- (evalM r);
if r' == 0 then Nothing else return (Num (l' `div` r'))
}

evalM (Boolean b) = (Just (Boolean b))

evalM (And l r) = do{
(Boolean l') <- (evalM l);
(Boolean r') <- (evalM r);
return (Boolean (l' && r'))
}

evalM (Leq l r) = do{
(Boolean l') <- (evalM l);
(Boolean r') <- (evalM r);
return (Boolean (l' <= r'))
}

evalM (IsZero n) = do{
(Num n') <- (evalM n);
return (Boolean (n'== 0))
}

evalM (If c t e) = do{
(Boolean c') <- (evalM c);
if c' then (evalM t) else (evalM e)
}

-- evalM _ = Nothing -- Replace this with your interpreter

evalErr :: ABE -> (Maybe ABE)

evalErr (Num n) = if n < 0 then Nothing else (return (Num n))
evalErr (Boolean b) = (return (Boolean b))
evalErr (IsZero n) = do{
n' <- (evalErr n);
case n of
  (Num x) -> return (Boolean (x == 0))
  _ -> Nothing
}

evalErr (Plus l r) = do{
l' <- (evalErr l);
r' <- (evalErr r);
case l' of
  (Num l1) -> case r' of
    (Num r1) -> (return (Num (l1 + r1)))
    _ -> Nothing
  _ -> Nothing
}

evalErr (Minus l r) = do{
l' <- (evalErr l);
r' <- (evalErr r);
case l' of
  (Num l1) -> case r' of
    (Num r1) -> (if (l1 < r1) then Nothing else return (Num (l1 - r1)))
    _ -> Nothing
  _ -> Nothing
}

evalErr (Mult l r) = do{
l' <- (evalErr l);
r' <- (evalErr r);
case l' of
  (Num l1) -> case r' of
    (Num r1) -> (return (Num (l1 * r1)))
    _ -> Nothing
  _ -> Nothing
}

evalErr (Div l r) = do{
l' <- (evalErr l);
r' <- (evalErr r);
case l' of
  (Num l1) -> case r' of
    (Num r1) -> (if r1==0 then Nothing else return (Num (l1 `div` r1)))
    _ -> Nothing
  _ -> Nothing
}

evalErr (And l r) = do{
l' <- (evalErr l);
r' <- (evalErr r);
case l' of
  (Boolean l1) -> case r' of
    (Boolean r1) -> (return (Boolean (l1 && r1)))
    _ -> Nothing
  _ -> Nothing
}

evalErr (Leq l r) = do{
l' <- (evalErr l);
r' <- (evalErr r);
case l' of
  (Boolean l1) -> case r' of
    (Boolean r1) -> (return (Boolean (l1 <= r1)))
    _ -> Nothing
  _ -> Nothing
}

evalErr (If c t e) = do{
c' <- (evalErr c);
case c' of
  (Boolean c1) -> if c1 then (evalErr t) else (evalErr e)
  _ -> Nothing
}

-- evalErr _ = Nothing -- Replace this with your interpreter

-- Type Derivation Function

typeofM :: ABE -> Maybe TABE
typeofM (Num n) =  return TNum

typeofM (Plus l r) = do{
l' <- (typeofM l);
r' <- (typeofM r);
if l'==TNum && r'==TNum
  then return TNum
  else Nothing
}

typeofM (Minus l r) = do{
l' <- (typeofM l);
r' <- (typeofM r);
if l'==TNum && r'==TNum
  then return TNum
  else Nothing
}

typeofM (Mult l r) = do{
l' <- (typeofM l);
r' <- (typeofM r);
if l'==TNum && r'==TNum
  then return TNum
  else Nothing
}

typeofM (Div l r) = do{
l' <- (typeofM l);
r' <- (typeofM r);
if l'==TNum && r'==TNum
  then return TNum
  else Nothing
}

typeofM (Boolean b) = return TBool

typeofM (And l r) = do{
l' <- (typeofM l);
r' <- (typeofM r);
if l'==TBool && r'==TBool
  then return TBool
  else Nothing
}

typeofM (Leq l r) = do{
l' <- (typeofM l);
r' <- (typeofM r);
if l'==TBool && r'==TBool
  then return TBool
  else Nothing
}

typeofM (IsZero n) = do {
n' <- (typeofM n);
if n' == TNum
  then (return TBool)
  else Nothing
}

typeofM (If c t e) = do{
c' <- (typeofM c);
t' <- (typeofM t);
e' <- (typeofM e);
if c'==TBool && t'==e'
  then (return  t')
  else Nothing
}


-- typeofM _ = Nothing

-- Combined interpreter

evalTypeM :: ABE -> Maybe ABE

evalTypeM e = do{
typeofM e;
evalM e
}

-- evalTypeM _ = Nothing

-- Optimizer

optimize :: ABE -> ABE


optimize e =
  case e of
    (Plus l (Num 0)) -> optimize l
    (Plus (Num 0) r) -> optimize r
    (If (Boolean True) t l) -> optimize t
    (If (Boolean False) t l) -> optimize l
    _ -> e

-- optimize e = e

interpOptM :: ABE -> Maybe ABE

interpOptM e = evalM (optimize e)


-- interpOptM _ = Nothing

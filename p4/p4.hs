{-# LANGUAGE GADTs #-}

-- import Control.Monad

-- Calculator language extended with an environment to hold defined variables

data TFBAE where
  TNum :: TFBAE
  TBool :: TFBAE
  (:->:) :: TFBAE -> TFBAE -> TFBAE
  deriving (Show,Eq)

data FBAE where
  Num :: Int -> FBAE
  Plus :: FBAE -> FBAE -> FBAE
  Minus :: FBAE -> FBAE -> FBAE
  Mult :: FBAE -> FBAE -> FBAE
  Div :: FBAE -> FBAE -> FBAE
  Bind :: String -> FBAE -> FBAE -> FBAE
  Lambda :: String -> TFBAE -> FBAE -> FBAE
  App :: FBAE -> FBAE -> FBAE
  Id :: String -> FBAE
  Boolean :: Bool -> FBAE
  And :: FBAE -> FBAE -> FBAE
  Or :: FBAE -> FBAE -> FBAE
  Leq :: FBAE -> FBAE -> FBAE
  IsZero :: FBAE -> FBAE
  If :: FBAE -> FBAE -> FBAE -> FBAE
  Fix :: FBAE -> FBAE
  deriving (Show,Eq)

-- Value defintion for statically scoped eval

data FBAEVal where
  NumV :: Int -> FBAEVal
  BooleanV :: Bool -> FBAEVal
  ClosureV :: String -> FBAE -> Env -> FBAEVal
  deriving (Show,Eq)

-- Enviornment for statically scoped eval

type Env = [(String,FBAEVal)]

-- Substitution
subst :: String -> FBAE -> FBAE -> FBAE
subst _ _ (Num x) = (Num x)
subst i v (Plus l r) = (Plus (subst i v l) (subst i v r))
subst i v (Minus l r) = (Minus (subst i v l) (subst i v r))
subst i v (Mult l r) = (Mult (subst i v l) (subst i v r))
subst i v (Div l r) = (Div (subst i v l) (subst i v r))
subst i v (Bind i' v' b') =
  if i==i' then (Bind i' (subst i v v') b')
    else (Bind i' (subst i v v') (subst i v b'))
subst i v (Lambda i' t b) = (Lambda i' t (subst i v b))
subst i v (Id i') = if i==i' then v else (Id i')
subst _ _ (Boolean x) = (Boolean x)
subst i v (And l r) = (And (subst i v l) (subst i v r))
subst i v (Or l r) = (Or (subst i v l) (subst i v r))
subst i v (Leq l r) = (Leq (subst i v l) (subst i v r))
subst i v (IsZero x) = (IsZero (subst i v x))
subst i v (If c t e) = (If (subst i v c) (subst i v t) (subst i v e))
subst i v (App f a) = (App (subst i v f) (subst i v a))
subst i v (Fix f) = (Fix (subst i v f))


-- Statically scoped eval

evalM :: Env -> FBAE -> (Maybe FBAEVal)

evalM env (Num n) = if n < 0 then Nothing else (Just(NumV n))

evalM env (Plus l r) = do{
(NumV l') <- (evalM env l);
(NumV r') <- (evalM env r);
return (NumV (l' + r'))
}

evalM env (Minus l r) = do{
(NumV l') <- (evalM env l);
(NumV r') <- (evalM env r);
if (l' < r') then Nothing else return (NumV (l' - r'))
}

evalM env (Mult l r) = do{
(NumV l') <- (evalM env l);
(NumV r') <- (evalM env r);
return (NumV (l' * r'))
}

evalM env (Div l r) = do{
(NumV l') <- (evalM env l);
(NumV r') <- (evalM env r);
return (NumV (l' `div` r'))
}

evalM env (Bind i v b) = do{
v' <- (evalM env v);
(evalM ((i,v'):env) b)
}

evalM env (Lambda i t b) = return (ClosureV i b env)


evalM env (App f a) = do { (ClosureV i b e) <- (evalM env f);
                          a' <- (evalM env a);
                          evalM ((i,a'):e) b }

evalM env (Id id) = (lookup id env)

evalM env (Boolean b) = (Just (BooleanV b))

evalM env (And l r) = do{
(BooleanV l') <- (evalM env l);
(BooleanV r') <- (evalM env r);
return (BooleanV (l' && r'))
}

evalM env (Or l r) = do{
(BooleanV l') <- (evalM env l);
(BooleanV r') <- (evalM env r);
return (BooleanV (l' || r'))
}

evalM env (Leq l r) = do{
(NumV l') <- (evalM env l);
(NumV r') <- (evalM env r);
return (BooleanV (l' <= r'))
}

evalM env (IsZero v) = do{
(NumV v') <- (evalM env v);
return (BooleanV (v' == 0))
}

evalM env (If c t e) = do{
(BooleanV c') <- (evalM env c);
(if c' then (evalM env t) else (evalM env e))
}

evalM env (Fix f) = do {
(ClosureV i b e) <- (evalM env f) ;
--Do we just assume the recursion is for one type only????
evalM e (subst i (Fix (Lambda i (TNum) b)) b)
}

-- evalM _ _ = Nothing

-- Type inference function

type Cont = [(String,TFBAE)]

typeofM :: Cont -> FBAE -> (Maybe TFBAE)

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

typeofM cont (Mult l r) = do{
l' <- (typeofM cont l);
r' <- (typeofM cont r);
if (l'==TNum && r'==TNum) then return TNum else Nothing
}

typeofM cont (Div l r) = do{
l' <- (typeofM cont l);
r' <- (typeofM cont r);
if (l'==TNum && r'==TNum) then return TNum else Nothing
}

typeofM cont (Bind i v b) = do{
v' <- typeofM cont v;
typeofM ((i,v'):cont) b
}

typeofM cont (Lambda i t b) = do{
t' <- typeofM ((i,t):cont) b;
return (t:->:t')
}

typeofM cont (App x y) = do {
tyXd :->: tyXr <- (typeofM cont x);
tyY <- typeofM cont y;
if tyXd==tyY then (return tyXr) else Nothing
}

typeofM cont (Id id) = (lookup id cont)

typeofM cont (Boolean b) = Just TBool

typeofM cont (And l r) = do{
TBool <- (typeofM cont l);
TBool <- (typeofM cont r);
return TBool
}

typeofM cont (Or l r) = do{
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

typeofM cont (Fix t) = do {
(d :->: r) <- typeofM cont t;
return r
}

-- typeofM _ _ = Nothing


-- Interpreter

interp :: FBAE -> (Maybe FBAEVal)

interp x = do{
(typeofM [] x);
(evalM [] x)
}

-- interp _ = Nothing

-- Factorial function for testing evalM and typeofM.  the type of test1 should
-- be TNum and the result of evaluating test1`should be (NumV 6).  Remember
-- that Just is used to return both the value and type.

test1 = (Bind "f" (Lambda "g" ((:->:) TNum TNum)
                    (Lambda "x" TNum (If (IsZero (Id "x")) (Num 1)
                                         (Mult (Id "x")
                                               (App (Id "g")
                                                    (Minus (Id "x")
                                                           (Num 1)))))))
         (App (Fix (Id "f")) (Num 3)))

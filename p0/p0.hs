{-# LANGUAGE GADTs, FlexibleContexts #-}

-- Imports for Parsec
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Token

--
-- Simple caculator over naturals with no identifiers
--
-- Author: Perry Alexander
-- Date: Tue Jan 23 17:54:44 CST 2018
--
-- Source files for the Arithmetic Expressions (AE) language from PLIH
--

-- AST Definition

data AE where
  Num :: Int -> AE
  Plus :: AE -> AE -> AE
  Minus :: AE -> AE -> AE
  Mult :: AE -> AE -> AE
  Div :: AE -> AE -> AE
  If0 :: AE -> AE -> AE -> AE
  deriving (Show,Eq)

-- AE Parser (Requires ParserUtils and Parsec included above)

languageDef =
  javaStyle { identStart = letter
            , identLetter = alphaNum
            , reservedNames = [ "if0"
                              , "then"
                              , "else"
                              ]
            , reservedOpNames = [ "+","-","*","/"]
            }

lexer = makeTokenParser languageDef

inFix o c a = (Infix (reservedOp lexer o >> return c) a)
preFix o c = (Prefix (reservedOp lexer o >> return c))
postFix o c = (Postfix (reservedOp lexer o >> return c))

parseString p str =
  case parse p "" str of
    Left e -> error $ show e
    Right r -> r

expr :: Parser AE
expr = buildExpressionParser operators term

operators = [
  [ inFix "*" Mult AssocLeft
    , inFix "/" Div AssocLeft ]
  , [ inFix "+" Plus AssocLeft
  , inFix "-" Minus AssocLeft ]
  ]

numExpr :: Parser AE
numExpr = do i <- integer lexer
             return (Num (fromInteger i))

ifExpr :: Parser AE
ifExpr  = do reserved lexer "if0"
             c <- expr
             reserved lexer "then"
             t <- expr
             reserved lexer "else"
             e <- expr
             return (If0 c t e)


term = parens lexer expr
       <|> numExpr
       <|> ifExpr

-- Parser invocation
-- Call parseAE to parse a string into the AE data structure.

parseAE = parseString expr

-- Evaluation Functions
-- Replace the bodies of these functions with your implementations for
-- Exercises 1-4.  Feel free to add utility functions or testing functions as
-- you see fit, but do not change the function signatures.  Note that only
-- Exercise 4 requires you to integrate the parser above.

evalAE :: AE -> Int

evalAE (Num n) = if n < 0 then error "Num less than 0" else n

evalAE (Plus l r) = (evalAE l) + (evalAE r)

evalAE (Minus l r) =
  let f = (evalAE l) - (evalAE r)
  in if f < 0 then error "Minus less than 0" else f

evalAE (Mult l r) = (evalAE l) * (evalAE r)

evalAE (Div l r) =
  let l' = (evalAE l)
      r' = (evalAE r)
  in if r' == 0 then error "Div by 0" else (l' `div` r')

evalAE (If0 c t e) =
  let c' = (evalAE c)
  in if c' == 0 then (evalAE t) else (evalAE e)

-- evalAE _ = 0

evalAEMaybe :: AE -> Maybe Int

evalAEMaybe (Num n) = if n < 0 then Nothing else (Just n)

evalAEMaybe (Plus l r) = case (evalAEMaybe l) of
  (Just l') -> case (evalAEMaybe r) of
    (Just r') -> Just (l' + r')
    Nothing -> Nothing
  Nothing -> Nothing

evalAEMaybe (Minus l r) = case (evalAEMaybe l) of
  (Just l') -> case (evalAEMaybe r) of
    (Just r') -> if (l' < r') then Nothing else Just (l' - r')
    Nothing -> Nothing
  Nothing -> Nothing

evalAEMaybe (Mult l r) = case (evalAEMaybe l) of
  (Just l') -> case (evalAEMaybe r) of
    (Just r') -> Just (l' * r')
    Nothing -> Nothing
  Nothing -> Nothing

evalAEMaybe (Div l r) = case (evalAEMaybe l) of
  (Just l') -> case (evalAEMaybe r) of
    (Just r') -> if r' == 0 then Nothing else Just (l' `div` r')
    Nothing -> Nothing
  Nothing -> Nothing

evalAEMaybe (If0 c t e) = case (evalAEMaybe c) of
  (Just c') -> if c' == 0 then (evalAEMaybe t) else (evalAEMaybe e)
  Nothing -> Nothing

-- evalAEMaybe _ = Nothing

evalM :: AE -> Maybe Int

evalM (Num n) = if n < 0 then Nothing else (Just n)

evalM (Plus l r) = do{
l' <- (evalM l);
r' <- (evalM r);
return (l' + r')
}

evalM (Minus l r) = do{
l' <- (evalM l);
r' <- (evalM r);
if (l' - r') < 0 then Nothing else return (l' - r')
}

evalM (Mult l r) = do{
l' <- (evalM l);
r' <- (evalM r);
return (l' * r')
}

evalM (Div l r) = do{
l' <- (evalM l);
r' <- (evalM r);
if r' == 0 then Nothing else return (l' `div` r')
}

evalM (If0 c t e) = do{
c' <- (evalM c);
if c' == 0 then (evalM t) else (evalM e)
}

-- evalM _ = Nothing

interpAE :: String -> Maybe Int

interpAE = evalM . parseAE

-- interpAE _ = Nothing

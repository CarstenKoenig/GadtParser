module ADT
  ( Expr (..)
  , eval
  , exprP
  ) where

import ADT.Internal
import ADT.Parsing

eval :: Expr -> Maybe (Either Bool Int) 
eval (IntE i) = 
  Just (Right i)
eval (AddE a b) = do
  aVal <- eval a
  bVal <- eval b
  case (aVal, bVal) of 
    (Right aInt, Right bInt) -> Just (Right (aInt + bInt))
    _ -> Nothing
eval (BoolE b) = 
  Just (Left b)
eval (IsNullE e) = do
  eVal <- eval e
  case eVal of
    Right eNum -> Just (Left $ eNum == 0)
    _ -> Nothing
eval (IfE b t e) = do
  bVal <- eval b
  case bVal of
    Left bBool
      | bBool -> eval t
      | otherwise -> eval e
    _ -> Nothing
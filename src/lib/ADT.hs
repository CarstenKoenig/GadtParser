{-|
Module      : ADT
Description : small DSL defined as an ADT
Copyright   : (c) Carsten König, 2019
License     : GPL-3
Maintainer  : Carsten.Koenig@hotmail.de
Stability   : experimental
Portability : POSIX

This module defines a simple expression language with

  - 'Int'-Values
  - 'Bool'-VAlues
  - addition of two sub-expressions
  - 'IsNullE' to check if it's subexpression is @0@
  - an @if ... then ... else ...@ conditional

You can use 'exprP' to parse such expressions and 'eval' to
evaluate parsed expressions.

The goal is to contrast this with the similar definitions in "GADT".
This one is quite simple to parse but can yield /misstyped/ expressions
whereas the "GADT" approach has more gurantees but is harder to parse.

-}
module ADT
  ( Expr (..)
  , eval
  , exprP
  ) where

import ADT.Internal
import ADT.Parsing

-- | evaluates an 'Expr' to either an boolean or an integer value
-- as expressions might be misstyped (for example addint an bool and an int)
-- this might fail with 'Nothing'
--
-- >>> eval (AddE (IntE 10) (IfE (BoolE False) (IntE 0) (IntE 32)))
-- Just (Right 42)
--
-- >>> eval (BoolE False)
-- Just (Left False)
--
-- >>> eval (AddE (IntE 10) (BoolE False) ) 
-- Nothing
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
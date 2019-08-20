{-# LANGUAGE GADTs, KindSignatures, RankNTypes, FlexibleInstances #-}

{-|
Module      : GADT.Internal
Description : definition of the GADT-style domain specific language 
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

Here we explore the GADT-approach where the expression-type 'Expr'
is annotated by the type 'evalExpr' will be returning (a Haskell-Type)

This guarantees that we cannot form semantically __invalid__ expressions like
@2 + false@ - for example the 'AddE' constructor only accepts sub-expression
that will return 'Int's.

This makes the 'evalExpr' function much nicer compared to it's cousin 
'ADT.Internal.eval'.

The downside is that it makes parsing harder. For example what type should
be returned by a parser @String -> ?@

@parse "5"@ would need to be an @Expr Int@ but @parse "false"@ needs the
type @Expr Bool@ (or some variant with 'Maybe').

We just wrap both cases into seperate constructors of 'WrappedExpr'
which automatically gives us an run-time representation of the type
as well which enables __intelligent__ parsing.

Finally 'wrap' is used as a nice way to lift 'Expr' values into 'WrappedExpr'
without having to manually tag the resulting types.

-}
module GADT.Internal
  ( Expr (..)
  , WrappedExpr (..)
  , KnownResType(..)
  , eval
  , evalExpr
  ) where

import Data.Kind (Type)


-- | AST representation of our little domain language tagged by the
--   value-type the expression is representing (see 'evalExpr')
data Expr (res :: Type) where
  -- | int-value
  IntE :: Int -> Expr Int
  -- | binary addition of two __int__-expressions
  AddE :: Expr Int -> Expr Int -> Expr Int
  -- | bool-value
  BoolE :: Bool -> Expr Bool
  -- | check if sub-expression of type __int__ is @0@
  IsNullE :: Expr Int -> Expr Bool
  -- | if-expression - condition-expression has to be of type Bool,
  --   __then__ and __else__ expression have to be the same type
  IfE :: Expr Bool -> Expr res -> Expr res -> Expr res


-- | evaluates an 'Expr' to the tagged type
evalExpr :: Expr res -> res
evalExpr (IntE i) = i
evalExpr (AddE a b) = evalExpr a + evalExpr b
evalExpr (BoolE b) = b
evalExpr (IsNullE i) = evalExpr i == 0
evalExpr (IfE b t e)
  | evalExpr b = evalExpr t
  | otherwise = evalExpr e


instance Show (Expr res) where
  show (IntE i) = show i
  show (AddE a b) = "( " ++ show a ++ " ) + ( " ++ show b ++ " )"
  show (BoolE b) = show b
  show (IsNullE i) = "isNull (" ++ show i ++ ")"
  show (IfE b t e) = "if " ++ show b ++ " then " ++ show t ++ " else " ++ show e


instance Eq res => Eq (Expr res) where
  (IntE a) == (IntE b) = a==b
  (AddE a b) == (AddE a' b') = a==a' && b==b'
  (BoolE a) == (BoolE b) = a==b
  (IsNullE a) == (IsNullE b) = a==b
  (IfE b t e) == (IfE b' t' e') = b==b' && t==t' && e==e'
  _ == _ = False


-- | runtime representation of either an __int__ or __bool__-Expression
--   mainly useful for parsing
data WrappedExpr where
  IntExpr :: Expr Int -> WrappedExpr
  BoolExpr :: Expr Bool -> WrappedExpr

instance Show WrappedExpr where
  show (IntExpr e) = show e
  show (BoolExpr e) = show e

instance Eq WrappedExpr where
  IntExpr a == IntExpr b = a == b
  BoolExpr a == BoolExpr b = a == b
  _ == _ = False

-- | evaluates an 'WrappedExpr' to either a 'Bool' or a 'Int' value
eval :: WrappedExpr -> Either Bool Int
eval (BoolExpr b) = Left (evalExpr b)
eval (IntExpr i) = Right (evalExpr i)


-- | helps wrapping 'Expr' to the right
--   'WrappedExpr' constructor
class KnownResType a where
  wrap :: Expr a -> WrappedExpr

instance KnownResType Int where
  wrap = IntExpr

instance KnownResType Bool where
  wrap = BoolExpr
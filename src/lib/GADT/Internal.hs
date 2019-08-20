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
  - 'IsNullE' to check if it's subexpression is `0`
  - an `if ... then ... else ...` conditional

Here we explore the GADT-approach where the expression-type 'Expr'
is annotated by the type 'evalExpr' will be returning (a Haskell-Type)

This guarantees that we cannot form semantically *invalid* expressions like
`2 + false` - for example the 'AddE' constructor only accepts sub-expression
that will return 'Int's.

This makes the 'evalExpr' function much nicer compared to it's cousin 
'ADT.Internal.eval'.

The downside is that it makes parsing harder. For example what type should
be returned by a parser `String -> ?`

`parse "5"` would need to be an `Expr Int` but `parse "false"` needs the
type `Expr Bool` (or some variant with 'Maybe').

We explore an approach where we wrap both (or really any) `Expr a` into
an *existential*  type 'WrappedExpr' which solves this problem nicely.

Now as types are erased we loose the ability to quickly check during parsing
if the types match. For example the parser for `if` needs a way to ensure that
both the `then` and `else` branch where parsed into the same expression-types
in order to use the 'IfE' constructor.
So we need a way to pass around information about the used types - which is
the job of 'ResType' (a singleton representation of 'Int' and 'Bool') and
the first parameter of the 'Wrap' constructor.

Finally 'wrap' is used as a nice way to lift 'Expr' values into 'WrappedExpr'
without having to manually tag the 'ResType', by using the 'KnownResType' class.

The 'KnownResType' class might be controversial, as you could achieve the same
with a simple evaluater that tracks the type by walking along an expression-tree
but this gives less boilerplate and next to no runtime cost - it's also a nice
way to constraint the type-argument of 'Expr' to sensible values - hence the
name.
-}
module GADT.Internal
  ( Expr (..)
  , WrappedExpr (..)
  , ResType (..)
  , KnownResType(..)
  , eval
  , evalExpr
  , unwrap
  , wrap
  ) where

import Data.Kind (Type)


data Expr (res :: Type) where
  IntE :: Int -> Expr Int
  AddE :: Expr Int -> Expr Int -> Expr Int
  BoolE :: Bool -> Expr Bool
  IsNullE :: Expr Int -> Expr Bool
  IfE :: Expr Bool -> Expr res -> Expr res -> Expr res

eval :: WrappedExpr -> Maybe (Either Bool Int)
eval = Just . unwrap (Left . evalExpr) (Right . evalExpr)


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


data WrappedExpr where
  Wrap :: ResType res -> Expr res -> WrappedExpr

instance Show WrappedExpr where
  show = unwrap show show 

instance Eq WrappedExpr where
  a == b = unwrapMatching False (==) (==) a b

data ResType a where
  BoolRes :: ResType Bool
  IntRes :: ResType Int

wrap :: KnownResType a => Expr a -> WrappedExpr
wrap = Wrap getResType

class KnownResType a where
  getResType :: ResType a

instance KnownResType Int where
  getResType = IntRes

instance KnownResType Bool where
  getResType = BoolRes


unwrap :: (Expr Bool -> b) -> (Expr Int -> b) -> WrappedExpr -> b
unwrap useBool _ (Wrap BoolRes boolExpr) = useBool boolExpr
unwrap _ useInt (Wrap IntRes intExpr) = useInt intExpr


unwrapMatching :: b -> (Expr Bool -> Expr Bool -> b) -> (Expr Int -> Expr Int -> b) -> WrappedExpr -> WrappedExpr -> b
unwrapMatching _ useBools _ (Wrap BoolRes boolExpr1) (Wrap BoolRes boolExpr2) = useBools boolExpr1 boolExpr2
unwrapMatching _ _ useInts (Wrap IntRes intExpr1) (Wrap IntRes intExpr2) = useInts intExpr1 intExpr2
unwrapMatching notMatching _ _ _ _ = notMatching
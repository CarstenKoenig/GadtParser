{-# LANGUAGE GADTs, KindSignatures, RankNTypes, FlexibleInstances #-}
module GADT.Internal
  ( Expr (..)
  , WrappedExpr (..)
  , ResType (..)
  , KnownResType(..)
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
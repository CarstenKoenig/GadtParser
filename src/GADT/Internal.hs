{-# LANGUAGE GADTs, KindSignatures, RankNTypes #-}
module GADT.Internal
  ( Expr (..)
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
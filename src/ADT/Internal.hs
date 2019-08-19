module ADT.Internal
  ( Expr (..)
  ) where

data Expr
  = IntE Int
  | AddE Expr Expr
  | BoolE Bool
  | IsNullE Expr
  | IfE Expr Expr Expr
  deriving (Eq, Show)
{-# LANGUAGE GADTs, RankNTypes #-}
module GADT
  ( Expr(..)
  , WrappedExpr
  , ResType (..)
  , eval
  , unwrap
  , exprP
  ) where

import GADT.Internal
import GADT.Parsing


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
{-# LANGUAGE GADTs, RankNTypes #-}
module GADT
  ( GExpr(..)
  , WrappedExpr
  , ResType (..)
  , gEval
  , unwrap
  , gExprP
  ) where

import GADT.Internal
import GADT.Parsing

gEval :: GExpr res -> res
gEval (GIntE i) = i
gEval (GAddE a b) = gEval a + gEval b
gEval (GBoolE b) = b
gEval (GIsNullE i) = gEval i == 0
gEval (GIfE b t e)
  | gEval b = gEval t
  | otherwise = gEval e
{-# LANGUAGE GADTs, RankNTypes #-}
{-|
Module      : GADT
Description : small DSL defined as a GADT parametrized by an expressions result-type
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

-}
module GADT
  ( Expr (..)
  , WrappedExpr
  , KnownResType (..)
  , eval
  , exprP
  ) where

import GADT.Internal
import GADT.Parsing


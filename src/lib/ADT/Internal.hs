{-|
Module      : ADT.Internal
Description : definition of the ADT-style domain specific language 
Copyright   : (c) Carsten König, 2019
License     : GPL-3
Maintainer  : Carsten.Koenig@hotmail.de
Stability   : experimental
Portability : POSIX

This module defines an ADT for a simple expression language with

  - 'Int'-Values
  - 'Bool'-VAlues
  - addition of two sub-expressions
  - 'IsNullE' to check if it's subexpression is @0@
  - an @if ... then ... else ...@ conditional
-}
module ADT.Internal
  ( Expr (..)
  ) where

-- | AST representation of our little domain language tagged by the
--   value-type the expression is representing (see 'ADT.eval')
data Expr
  = IntE Int
  | AddE Expr Expr
  | BoolE Bool
  | IsNullE Expr
  | IfE Expr Expr Expr
  deriving (Eq, Show)
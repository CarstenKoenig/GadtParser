{-# LANGUAGE GADTs, KindSignatures, RankNTypes #-}
module GADT.Internal
  ( GExpr (..)
  ) where

import Data.Kind (Type)


data GExpr (res :: Type) where
  GIntE :: Int -> GExpr Int
  GAddE :: GExpr Int -> GExpr Int -> GExpr Int
  GBoolE :: Bool -> GExpr Bool
  GIsNullE :: GExpr Int -> GExpr Bool
  GIfE :: GExpr Bool -> GExpr res -> GExpr res -> GExpr res


instance Show (GExpr res) where
  show (GIntE i) = show i
  show (GAddE a b) = "( " ++ show a ++ " ) + ( " ++ show b ++ " )"
  show (GBoolE b) = show b
  show (GIsNullE i) = "isNull (" ++ show i ++ ")"
  show (GIfE b t e) = "if " ++ show b ++ " then " ++ show t ++ " else " ++ show e
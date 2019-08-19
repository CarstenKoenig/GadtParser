{-# LANGUAGE GADTs, RankNTypes #-}
module GADT.Parsing 
  ( WrappedExpr
  , ResType (..)
  , unwrap
  , gExprP
  ) where

import Control.Applicative ((<|>))
import Data.Void (Void)
import Text.Megaparsec (Parsec)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as PC
import GADT.Internal


type Parser = Parsec Void String


data WrappedExpr where
  Wrap :: ResType res -> GExpr res -> WrappedExpr

instance Show WrappedExpr where
  show = unwrap show show 

data ResType a where
  BoolRes :: ResType Bool
  IntRes :: ResType Int


unwrap :: (GExpr Bool -> b) -> (GExpr Int -> b) -> WrappedExpr -> b
unwrap useBool _ (Wrap BoolRes boolExpr) = useBool boolExpr
unwrap _ useInt (Wrap IntRes intExpr) = useInt intExpr


unwrapMatching :: b -> (GExpr Bool -> GExpr Bool -> b) -> (GExpr Int -> GExpr Int -> b) -> WrappedExpr -> WrappedExpr -> b
unwrapMatching _ useBools _ (Wrap BoolRes boolExpr1) (Wrap BoolRes boolExpr2) = useBools boolExpr1 boolExpr2
unwrapMatching _ _ useInts (Wrap IntRes intExpr1) (Wrap IntRes intExpr2) = useInts intExpr1 intExpr2
unwrapMatching elseRes _ _ _ _ = elseRes


gExprP :: Parser WrappedExpr
gExprP = gIfExprP <|> gTermExprP


gIfExprP :: Parser WrappedExpr
gIfExprP = do
  _ <- PC.string "if" <* PC.space
  b <- gTermExprP <* PC.space
  unwrap rest (const $ fail "type-error Bool expected") b
  where
    rest :: GExpr Bool -> Parser WrappedExpr
    rest b = do
      _ <- PC.string "then" <* PC.space
      t <- gTermExprP <* PC.space
      _ <- PC.string "else" <* PC.space
      e <- gTermExprP <* PC.space
      unwrapMatching (fail "then and else types do not match") (\t e -> pure $ Wrap BoolRes (GIfE b t e)) (\t e -> pure $ Wrap IntRes (GIfE b t e)) t e

gTermExprP :: Parser WrappedExpr
gTermExprP = P.try gAddExprP <|> gValueExprP

gAddExprP :: Parser WrappedExpr
gAddExprP = do
  first <- gValueExprP 
  unwrap (const $ fail "type-error Int expected") (fmap (Wrap IntRes) . more) first
    where
      more :: GExpr Int -> Parser (GExpr Int)
      more acc =
        do
          _ <- PC.char '+' <* P.hidden PC.space
          next <- gValueExprP
          unwrap (const $ fail "type-error Int expected") (more . GAddE acc) next
        <|> pure acc


gValueExprP :: Parser WrappedExpr
gValueExprP = gIsNullP valueExprP' <|> valueExprP'
    where
      valueExprP' = P.choice
        [ brace gExprP
        , gIntExprP
        , gBoolExprP
        ]

gIsNullP :: Parser WrappedExpr -> Parser WrappedExpr
gIsNullP valP = do
  _ <- P.label "isNull" $ P.hidden $ PC.string "isNull " <* PC.space
  v <- valP
  unwrap (const $ fail "type-error Int expected") unwrapInt v
  where
    unwrapInt intExp = pure (Wrap BoolRes $ GIsNullE intExp)

gIntExprP :: Parser WrappedExpr
gIntExprP = Wrap IntRes . GIntE <$> numberP <* PC.space

gBoolExprP :: Parser WrappedExpr
gBoolExprP = Wrap BoolRes . GBoolE <$> boolP <* PC.space

numberP :: Parser Int
numberP = P.label "number" $ P.hidden $
  read <$> P.some PC.numberChar


boolP :: Parser Bool
boolP = P.label "boolean" $ P.hidden $ P.choice
    [ True <$ PC.string "true"
    , False <$ PC.string "false" 
    ]

brace :: Parser p -> Parser p
brace p = do
  _ <- PC.char '(' <* P.hidden PC.space
  res <- p
  _ <- PC.char ')' <* P.hidden PC.space
  pure res 
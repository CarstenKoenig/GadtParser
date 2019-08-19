{-# LANGUAGE GADTs, RankNTypes #-}
module GADT.Parsing 
  ( WrappedExpr
  , ResType (..)
  , unwrap
  , exprP
  ) where

import Control.Applicative ((<|>))
import Control.Monad (void)
import Data.Void (Void)
import Text.Megaparsec (Parsec)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as PC
import GADT.Internal


type Parser = Parsec Void String


data WrappedExpr where
  Wrap :: ResType res -> Expr res -> WrappedExpr

instance Show WrappedExpr where
  show = unwrap show show 

data ResType a where
  BoolRes :: ResType Bool
  IntRes :: ResType Int


unwrap :: (Expr Bool -> b) -> (Expr Int -> b) -> WrappedExpr -> b
unwrap useBool _ (Wrap BoolRes boolExpr) = useBool boolExpr
unwrap _ useInt (Wrap IntRes intExpr) = useInt intExpr


unwrapMatching :: b -> (Expr Bool -> Expr Bool -> b) -> (Expr Int -> Expr Int -> b) -> WrappedExpr -> WrappedExpr -> b
unwrapMatching _ useBools _ (Wrap BoolRes boolExpr1) (Wrap BoolRes boolExpr2) = useBools boolExpr1 boolExpr2
unwrapMatching _ _ useInts (Wrap IntRes intExpr1) (Wrap IntRes intExpr2) = useInts intExpr1 intExpr2
unwrapMatching elseRes _ _ _ _ = elseRes


exprP :: Parser WrappedExpr
exprP = (gIfExprP <|> gTermExprP) <* (void PC.eol <|> void P.eof)


gIfExprP :: Parser WrappedExpr
gIfExprP = do
  _ <- PC.string "if" <* PC.space
  off <- P.getOffset
  b <- gTermExprP <* PC.space
  unwrap rest (const $ P.setOffset off >> fail "type-error Bool expected") b
  where
    rest :: Expr Bool -> Parser WrappedExpr
    rest b = do
      _ <- PC.string "then" <* PC.space
      t <- gTermExprP <* PC.space
      _ <- PC.string "else" <* PC.space
      off <- P.getOffset
      e <- gTermExprP <* PC.space
      unwrapMatching (P.setOffset off >> fail "then and else types do not match") (\t e -> pure $ Wrap BoolRes (IfE b t e)) (\t e -> pure $ Wrap IntRes (IfE b t e)) t e

gTermExprP :: Parser WrappedExpr
gTermExprP = gAddExprP <|> gValueExprP

gAddExprP :: Parser WrappedExpr
gAddExprP = do
  first <- gValueExprP 
  unwrap (pure . Wrap BoolRes) (fmap (Wrap IntRes) . more) first
    where
      more :: Expr Int -> Parser (Expr Int)
      more acc =
        do
          _ <- P.try $ PC.char '+' <* P.hidden PC.space
          off <- P.getOffset
          next <- gValueExprP
          unwrap (const $ P.setOffset off >> fail "type-error Int expected") (more . AddE acc) next
        <|> pure acc


gValueExprP :: Parser WrappedExpr
gValueExprP = gIsNullP valueExprP' <|> valueExprP'
    where
      valueExprP' = P.choice
        [ brace exprP
        , gIntExprP
        , gBoolExprP
        ]

gIsNullP :: Parser WrappedExpr -> Parser WrappedExpr
gIsNullP valP = do
  _ <- P.label "isNull" $ P.hidden $ PC.string "isNull " <* PC.space
  off <- P.getOffset
  v <- valP
  unwrap (const $ P.setOffset off >> fail "type-error Int expected") unwrapInt v
  where
    unwrapInt intExp = pure (Wrap BoolRes $ IsNullE intExp)

gIntExprP :: Parser WrappedExpr
gIntExprP = Wrap IntRes . IntE <$> numberP <* PC.space

gBoolExprP :: Parser WrappedExpr
gBoolExprP = Wrap BoolRes . BoolE <$> boolP <* PC.space

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
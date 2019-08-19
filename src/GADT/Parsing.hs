{-# LANGUAGE GADTs, RankNTypes #-}
module GADT.Parsing 
  ( exprP
  ) where

import Control.Applicative ((<|>))
import Control.Monad (void)
import Data.Void (Void)
import Text.Megaparsec (Parsec)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as PC
import GADT.Internal


type Parser = Parsec Void String


exprP :: Parser WrappedExpr
exprP = (gIfExprP <|> gTermExprP) <* (void PC.eol <|> void P.eof)


gIfExprP :: Parser WrappedExpr
gIfExprP = do
  _ <- PC.string "if" <* PC.space
  b <- gBoolValueExprP <* PC.space
  rest b
  where
    rest :: Expr Bool -> Parser WrappedExpr
    rest b = do
      _ <- PC.string "then" <* PC.space
      t <- gTermExprP <* PC.space
      _ <- PC.string "else" <* PC.space
      unwrap (\tExpr -> gBoolValueExprP >>= \eExpr -> pure . Wrap BoolRes $ IfE b tExpr eExpr) (\tExpr -> gAddExprP >>= \eExpr -> pure . Wrap IntRes $ IfE b tExpr eExpr) t

gTermExprP :: Parser WrappedExpr
gTermExprP = (Wrap IntRes <$> gAddExprP) <|> gValueExprP

gAddExprP :: Parser (Expr Int)
gAddExprP = chainL1 (pure AddE <$> PC.char '+' <* P.hidden PC.space) gIntValueExprP

gValueExprP :: Parser WrappedExpr
gValueExprP = (Wrap BoolRes <$> gIsNullP) <|> valueExprP'
    where
      valueExprP' = P.choice
        [ brace exprP
        , Wrap IntRes <$> gIntExprP
        , Wrap BoolRes <$> gBoolExprP
        ]

gIntValueExprP :: Parser (Expr Int)
gIntValueExprP = P.choice
  [ P.getOffset >>= (\off -> brace exprP >>= unwrap (const $ P.setOffset off >> fail "int type expected") pure)
  , gIntExprP
  ]

gBoolValueExprP :: Parser (Expr Bool)
gBoolValueExprP = P.choice
  [ P.getOffset >>= (\off -> brace exprP >>= unwrap pure (const $ P.setOffset off >> fail "bool type expected"))
  , gBoolExprP
  , gIsNullP 
  ]

gIsNullP :: Parser (Expr Bool)
gIsNullP = do
  _ <- P.label "isNull" $ P.hidden $ PC.string "isNull " <* PC.space
  v <- gIntValueExprP
  pure $ IsNullE v

gIntExprP :: Parser (Expr Int)
gIntExprP = IntE <$> numberP <* PC.space

gBoolExprP :: Parser (Expr Bool)
gBoolExprP = BoolE <$> boolP <* PC.space

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

chainL1 :: Parser (a -> a -> a) -> Parser a -> Parser a
chainL1 pOp pVal = do
  first <- pVal 
  more first
  where
    more acc =
      do
        op <- pOp
        val <- pVal
        more (op acc val)
      <|> pure acc


{-# LANGUAGE GADTs, RankNTypes #-}

{-|
Module      : GADT.Parsing
Description : "Text.Megaparsec" parser for 'GADT.Internal.Expr' expressions
Copyright   : (c) Carsten König, 2019
License     : GPL-3
Maintainer  : Carsten.Koenig@hotmail.de
Stability   : experimental
Portability : POSIX

defines a parser for this syntax:

@
<expr>        ::= <if-expr> | <term-expr>
<if-expr>     ::= "if" <bool-expr> "then" <term-expr : a> "else" <term-expr : a>
<term-expr>   ::= <add-expr> | <value-expr>
<add-expr>    ::= <int-expr> | <int-expr> "+" <add-expr>
<value-expr>  ::= <isNull-expr> | "(" <expr> ")" | <number> | <bool>
<bool-expr>   ::= "(" <expr : Bool> ")" | <isNull-expr> | <bool>
<isNull-expr> ::= "isNull" <int-expr>
<int-expr>    ::= "(" <expr : Int> ")" | <number>
@

-}

module GADT.Parsing 
  ( exprP
  ) where

import Control.Applicative ((<|>))
import Control.Monad (void)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as PC
import CommonParsers
import GADT.Internal


-- | defines a Megaparsec-Parser for 'WrappedExpr' ended by EOL or EOF
--
-- >>> import qualified Text.Megaparsec as P
-- >>> P.parse exprP "" "3+4"
-- Right (IntExpr (AddE (IntE 3) (IntE 4)))
exprP :: Parser WrappedExpr
exprP = exprP' <* (void PC.eol <|> void P.eof)


exprP' :: Parser WrappedExpr
exprP' = gIfExprP <|> gTermExprP


gIfExprP :: Parser WrappedExpr
gIfExprP = do
  _ <- PC.string "if" <* P.hidden PC.space
  b <- gBoolValueExprP
  rest b
  where
    rest :: Expr Bool -> Parser WrappedExpr
    rest b = do
      _ <- PC.string "then" <* P.hidden PC.space
      t <- gTermExprP
      _ <- PC.string "else" <* P.hidden PC.space
      case t of
        IntExpr tExpr ->
          wrap . IfE b tExpr <$> gAddExprP
        BoolExpr tExpr ->
          wrap . IfE b tExpr <$> gBoolValueExprP

gTermExprP :: Parser WrappedExpr
gTermExprP = (wrap <$> gAddExprP) <|> gValueExprP

gAddExprP :: Parser (Expr Int)
gAddExprP = chainL1 (pure AddE <$> PC.char '+' <* P.hidden PC.space) gIntValueExprP

gValueExprP :: Parser WrappedExpr
gValueExprP = (wrap <$> gIsNullP) <|> valueExprP'
    where
      valueExprP' = P.choice
        [ brace exprP'
        , wrap <$> gIntExprP
        , wrap <$> gBoolExprP
        ]

gIntValueExprP :: Parser (Expr Int)
gIntValueExprP = P.choice
  [ P.getOffset >>= (\off -> brace exprP' >>= braced off)
  , gIntExprP
  ]
  where
    braced _ (IntExpr i) = pure i
    braced off (BoolExpr _) = P.setOffset off >> fail "int type expected"

gBoolValueExprP :: Parser (Expr Bool)
gBoolValueExprP = P.choice
  [ P.getOffset >>= (\off -> brace exprP' >>= braced off)
  , gBoolExprP
  , gIsNullP
  ]
  where
    braced _ (BoolExpr b) = pure b
    braced off (IntExpr _) = P.setOffset off >> fail "bool type expected"

gIsNullP :: Parser (Expr Bool)
gIsNullP = do
  _ <- P.label "isNull" $ P.hidden $ PC.string "isNull" <* PC.space
  v <- gIntValueExprP
  pure $ IsNullE v

gIntExprP :: Parser (Expr Int)
gIntExprP = IntE <$> numberP <* P.hidden PC.space

gBoolExprP :: Parser (Expr Bool)
gBoolExprP = BoolE <$> boolP <* P.hidden PC.space

module ADT.Parsing
  ( exprP
  ) where

import Control.Applicative ((<|>))
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as PC
import CommonParsers
import ADT.Internal


exprP :: Parser Expr
exprP = ifExprP <|> termExprP

ifExprP :: Parser Expr
ifExprP = do
  _ <- PC.string "if" <* P.hidden PC.space
  b <- termExprP
  _ <- PC.string "then" <* P.hidden PC.space
  t <- termExprP
  _ <- PC.string "else" <* P.hidden PC.space
  e <- termExprP
  pure $ IfE b t e

termExprP :: Parser Expr
termExprP = addExprP <|> valueExprP

addExprP :: Parser Expr
addExprP = chainL1 opAddP valueExprP
    where
      opAddP = const AddE <$> (PC.char '+' <* P.hidden PC.space)

valueExprP :: Parser Expr
valueExprP = isNullP valueExprP' <|> valueExprP'
    where
      valueExprP' = P.choice
        [ brace exprP
        , intExprP
        , boolExprP
        ]

isNullP :: Parser Expr -> Parser Expr
isNullP valP = do
  _ <- P.label "isNull" $ P.hidden $ PC.string "isNull" <* PC.space
  v <- valP
  pure $ IsNullE v

intExprP :: Parser Expr
intExprP = IntE <$> numberP <* P.hidden PC.space

boolExprP :: Parser Expr
boolExprP = BoolE <$> boolP <* P.hidden PC.space

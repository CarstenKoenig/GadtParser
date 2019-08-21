{-|
Module      : CommonParsers
Description : helper-functions/combinators for parsers
Copyright   : (c) Carsten König, 2019
License     : GPL-3
Maintainer  : Carsten.Koenig@hotmail.de
Stability   : experimental
Portability : POSIX

-}
module CommonParsers 
  ( Parser
  , numberP
  , boolP
  , brace
  , chainL1
  ) where

import Control.Applicative ((<|>))
import Data.Void (Void)
import Text.Megaparsec (Parsec)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as PC

-- $setup
-- >>> import qualified Text.Megaparsec as P
-- >>> import qualified Text.Megaparsec.Char as PC

-- | type-synonym for a simple parser over a 'String'/'Char'-stream
type Parser = Parsec Void String


-- | this parser will parse at least one digits into a 'Int'
--
-- >>> P.parse numberP "" "1234" 
-- Right 1234
numberP :: Parser Int
numberP = P.label "number" $ P.hidden $
  read <$> P.some PC.numberChar


-- | this parser will parse @"true"@ and @"false"@ into their 'Bool' values
--
-- >>> P.parse boolP "" "true" 
-- Right True
boolP :: Parser Bool
boolP = P.label "boolean" $ P.hidden $ P.choice
    [ True <$ PC.string "true"
    , False <$ PC.string "false" 
    ]


-- | this parser-combinator parses @p@ inside parentheses and returns it's result
--
-- >>> P.parse (brace boolP) "" "(true)" 
-- Right True
brace :: Parser p -> Parser p
brace p = do
  _ <- PC.char '(' <* P.hidden PC.space
  res <- p
  _ <- PC.char ')' <* P.hidden PC.space
  pure res


-- | this parser-combinator parses occurences of @pVal@ seperated by @pOp@
-- reducing the results of the @pVal@s using @pOp@s result
--
-- >>> P.parse (chainL1 (const (+) <$> PC.char '+') numberP) "" "1+2+3+4" 
-- Right 10
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


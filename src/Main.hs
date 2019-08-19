module Main where

import Control.Applicative ((<|>))
import Data.Void (Void)
import Text.Megaparsec (Parsec)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as PC
import System.IO (hSetBuffering, BufferMode(NoBuffering), stdout)

main :: IO ()
main = do 
  hSetBuffering stdout NoBuffering
  loop
  where
    loop = do
      putStr "> "
      input <- getLine
      case P.parse exprP "Input" input of
        Left err -> do
          putStrLn "ERROR"
          putStr (P.errorBundlePretty err)
        Right exp -> do
          putStr "Expression: "
          print exp
          putStr "evaluated Value: "
          print (eval exp)
      loop


data Expr
  = IntE Int
  | AddE Expr Expr
  | BoolE Bool
  | IsNullE Expr
  | IfE Expr Expr Expr
  deriving (Eq, Show)


eval :: Expr -> Maybe (Either Bool Int) 
eval (IntE i) = 
  Just (Right i)
eval (AddE a b) = do
  aVal <- eval a
  bVal <- eval b
  case (aVal, bVal) of 
    (Right aInt, Right bInt) -> Just (Right (aInt + bInt))
    _ -> Nothing
eval (BoolE b) = 
  Just (Left b)
eval (IsNullE e) = do
  eVal <- eval e
  case eVal of
    Right eNum -> Just (Left $ eNum == 0)
    _ -> Nothing
eval (IfE b t e) = do
  bVal <- eval b
  case bVal of
    Left bBool
      | bBool -> eval t
      | otherwise -> eval e
    _ -> Nothing


type Parser = Parsec Void String


exprP :: Parser Expr
exprP = ifExprP <|> termExprP

ifExprP :: Parser Expr
ifExprP = do
  _ <- PC.string "if" <* PC.space
  b <- termExprP <* PC.space
  _ <- PC.string "then" <* PC.space
  t <- termExprP <* PC.space
  _ <- PC.string "else" <* PC.space
  e <- termExprP <* PC.space
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
  _ <- P.label "isNull" $ P.hidden $ PC.string "isNull " <* PC.space
  v <- valP
  pure $ IsNullE v

intExprP :: Parser Expr
intExprP = IntE <$> numberP <* PC.space

boolExprP :: Parser Expr
boolExprP = BoolE <$> boolP <* PC.space

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

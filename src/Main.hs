{-# LANGUAGE GADTs, KindSignatures, RankNTypes #-}
module Main where

import Control.Applicative ((<|>))
import Data.Kind
import Data.Proxy (Proxy(..))
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
      case P.parse gExprP "Input" input of
        Left err -> do
          putStrLn "ERROR"
          putStr (P.errorBundlePretty err)
        Right exp -> do
          putStr "Expression: "
          print exp
          putStr "evaluated Value: "
          unwrap (print . gEval)  (print . gEval) exp
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

gEval :: GExpr res -> res
gEval (GIntE i) = i
gEval (GAddE a b) = gEval a + gEval b
gEval (GBoolE b) = b
gEval (GIsNullE i) = gEval i == 0
gEval (GIfE b t e)
  | gEval b = gEval t
  | otherwise = gEval e


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

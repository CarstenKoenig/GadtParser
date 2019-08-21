module GADT.ParserSpec where

import Test.Hspec
import qualified Text.Megaparsec as P
import GADT

spec :: Spec
spec = do
  describe "can parse a string into a GADT-Expression for" $ do
    it "a simple number" $
      "42" `shouldParseTo` IntE 42
    it "false into False" $
      "false" `shouldParseTo` BoolE False
    it "true into True" $
      "true" `shouldParseTo` BoolE True
    it "isNull expressions" $
      "isNull 0" `shouldParseTo` IsNullE (IntE 0)
    it "complex isNull expressions" $
      "isNull (if true then 0 else 1)" `shouldParseTo` IsNullE (IfE (BoolE True) (IntE 0) (IntE 1))
    it "if expressions" $
      "if false then 0 else 1" `shouldParseTo` IfE (BoolE False) (IntE 0) (IntE 1)
    it "can handle additions" $
      "2+3" `shouldParseTo` AddE (IntE 2) (IntE 3)
    it "can handle expressions in parentheses" $
      "5+(1+3)" `shouldParseTo` AddE (IntE 5) (AddE (IntE 1) (IntE 3))
    it "can handle spaces in expressions" $
      "5 + ( 1 + 3 ) " `shouldParseTo` AddE (IntE 5) (AddE (IntE 1) (IntE 3))
  describe "will not parse miss-typed expressions" $ do
    it "will not add booleans" $
      shouldNotParse "false+true"
    it "will not add booleans with ints" $
      shouldNotParse "4+true"
    it "will not accept boolean parameters to isNull" $
      shouldNotParse "isNull false"
    it "will not accept complex boolean parameters to isNull" $
      shouldNotParse "isNull (if isNull 0 then true else false)"
    it "will not accept ints to the first argument of if" $
      shouldNotParse "if 5 then 0 else 1"
    it "will not accept then/else parameters in if expressions of different type" $
      shouldNotParse "if false then isNull 0 else 1"


shouldParseTo :: KnownResType a => String -> Expr a -> Expectation
shouldParseTo input expr =
  parse input `shouldReturn` wrap expr

shouldNotParse :: String -> Expectation
shouldNotParse input =
  parse input `shouldThrow` anyException

parse :: String -> IO WrappedExpr
parse input =
  case P.parse exprP "input" input of
    Left err ->
      fail (P.errorBundlePretty err)
    Right expr -> return expr
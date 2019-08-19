{-# LANGUAGE GADTs, RankNTypes #-}
module Main where

import qualified Text.Megaparsec as P
import System.IO (hSetBuffering, BufferMode(NoBuffering), stdout)
import GADT

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
        Right expr -> do
          putStr "Expression: "
          print expr
          putStr "evaluated Value: "
          unwrap (print . gEval)  (print . gEval) expr
      loop
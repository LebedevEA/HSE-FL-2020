module Main where

import CombinatorLib
import PrologParser

data Expr = Conj Expr Expr
          | Disj Expr Expr
          | Jst String
          deriving Show

main :: IO ()
main = do
  testParser atom "a"

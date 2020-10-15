module Main where

import CombinatorLib

testParser :: Show a => Parser a -> String -> IO ()
testParser parser string = print $ runParser parser $ Text string (1, 1)

parseComment = commentmulti (string "/*") $ string "*/"
parseArray = arrayBr (string "[") (char 'e') (string ",") (string "]")
parseSingleComment = commentsingle $ string "--"

data Expr = Conj Expr Expr
          | Disj Expr Expr
          | Jst String
          deriving Show

parseConj = binopl Conj parseJst (char ';')
parseJst  = CombinatorLib.fmap Jst $ many1 alphanum

main :: IO ()
main = do
  testParser parseConj "a;b;c."

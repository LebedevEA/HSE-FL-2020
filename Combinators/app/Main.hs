module Main where

import CombinatorLib

testParser :: Show a => Parser a -> String -> IO ()
testParser parser string = print $ runParser parser $ Text string (1, 1)

data Expr = Conj Expr Expr
          | Disj Expr Expr
          | Jst String
          deriving Show


parseSpaces = (CombinatorLib.fmap (safeFoldl1 (++)) $ many1 $ commentsingle (string "--")) <|> (CombinatorLib.fmap (safeFoldl1 (++)) $ many1 $ commentmulti (string "/*") (string "*/")) <|> spaces
parse1 = skip parseSpaces (string "a")
parse2 = skip parseSpaces (string ",")
parsE = list parse1 parse2
parseConj = binopl parseSpaces Conj parseJst (char ';')
parseJst  = CombinatorLib.fmap Jst $ many1 alphanum

main :: IO ()
main = do
  testParser parseSpaces "/**/a"
  testParser parseConj "/**//**/a ; b ; c."

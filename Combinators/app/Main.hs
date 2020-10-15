module Main where

import CombinatorLib

testParser :: Show a => Parser a -> String -> IO ()
testParser parser string = print $ runParser parser $ Text string (1, 1)

parseArray = arrayBr (string "[") (char 'e') (string ",") (string "]")

main :: IO ()
main = do
  testParser parseArray "[e,   e, e, e]"

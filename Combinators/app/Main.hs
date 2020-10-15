module Main where

import CombinatorLib

testParser :: Show a => Parser a -> String -> IO ()
testParser parser string = print $ runParser parser $ Text string (1, 1)

main :: IO ()
main = do
  testParser alphanum "a123"
  testParser alphanum "1a23"

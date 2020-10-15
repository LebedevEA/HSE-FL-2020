module Main where

import CombinatorLib

testParser :: Show a => Parser a -> String -> IO ()
testParser parser string = print $ runParser parser $ Text string (1, 1)

parseComment = commentmulti (string "/*") $ string "*/"
parseArray = arrayBr (string "[") (char 'e') (string ",") (string "]")
parseSingleComment = commentsingle $ string "--"

main :: IO ()
main = do
  testParser parseSingleComment "-- hello\nfdsf"

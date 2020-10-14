module Main where

import PrologParser
import System.IO
import System.Environment
import Text.ParserCombinators.Parsec

doParseFromFile :: Show a => FilePath -> (String -> Either ParseError a) -> IO ()
doParseFromFile path parser = do
  input <- readFile path
  case parser input of
    Left err -> print err
    Right r -> do
      writeFile (path ++ ".out") (show r)


main :: IO ()
main = do
  args <- getArgs
  let arg      = head args
  let filename = head $ tail args
  case arg of
       "--atom"     -> doParseFromFile filename parserAtom
       "--typeexpr" -> doParseFromFile filename parserTypeExpr
       "--type"     -> doParseFromFile filename parserType
       "--module"   -> doParseFromFile filename parserModule
       "--relation" -> doParseFromFile filename parserRelation
       "--list"     -> doParseFromFile filename parserList
       "--prog"     -> doParseFromFile filename parserProg
       otherwise    -> undefined

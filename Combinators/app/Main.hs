module Main where

import System.IO
import System.Environment
import PrologParser
import PrologAst
import CombinatorLib hiding (list)

-- ввод-вывод из файла я не тестировал, но я надеюсь, что он работает..
doParseFromFile :: Show a => FilePath -> (Text -> Either SyntaxError (a, Text)) -> IO ()
doParseFromFile path parser = do
  input <- readFile path
  case parser $ makeText input of
    Left err -> do
      writeFile (path ++ ".out") (show err)
    Right r -> do
      writeFile (path ++ ".out") (show r)


main :: IO ()
main = do
  args <- getArgs
  let arg      = head args
  let filename = head $ tail args
  case arg of
       "--atom"     -> doParseFromFile filename $ runParser atom
       "--typeexpr" -> doParseFromFile filename $ runParser typeExpr
       "--type"     -> doParseFromFile filename $ runParser typ
       "--module"   -> doParseFromFile filename $ runParser parseModule
       "--relation" -> doParseFromFile filename $ runParser relation
       "--list"     -> doParseFromFile filename $ runParser list
       "--prog"     -> doParseFromFile filename $ runParser prog
       otherwise    -> undefined

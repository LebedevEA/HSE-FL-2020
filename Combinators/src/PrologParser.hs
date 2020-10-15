module PrologParser where

import Prelude hiding (seq, fmap, array, any)
import CombinatorLib hiding (list)
import PrologAst

testParser :: Show a => Parser a -> String -> IO ()
testParser parser string = print $ runParser parser $ Text string (1, 1)

-- TODO (fmap (safeFoldl1 (++))) to one func
mspaces = (fconcat $ many1 $ commentsingle (string "--")) <|> (fconcat $ many1 $ commentmulti (string "/*") (string "*/")) <|> spaces

m1spaces = (fconcat $ many1 $ commentsingle (string "--")) <|> (fconcat $ many1 $ commentmulti (string "/*") (string "*/")) <|> (many1 $ foldl1 (<|>) $ map char "\t\n ")

br = bracketed' mspaces (string "(") (string ")")

mbbr :: Parser a -> Parser a
mbbr p = Parser $ \t ->
  case runParser (br p) t of
    Left  _ -> runParser p t
    Right e -> runParser (mbbr (br p)) t
  
prog :: Parser PrologProgram
prog =
  parseModule `seq` \m ->
  (many1 typ) `seq` \ts ->
  (many1 relation) `seq` \rs ->
  ret $ Program (Just m) ts rs

parseModule :: Parser String
parseModule = 
  mspaces `seq` \_ ->
  string "module" `seq` \_ ->
  m1spaces `seq` \_ ->
  ident `seq` \i ->
  mspaces `seq` \_ ->
  char '.' `seq` \_ ->
  ret i

typ :: Parser TypeDef
typ =
  mspaces `seq` \_ ->
  string "type" `seq` \_ ->
  ident `seq` \i ->
  typeExpr `seq` \tb ->
  mspaces `seq` \_ ->
  char '.' `seq` \_ ->
  ret $ TypeDef i tb

typeExpr :: Parser Type
typeExpr = binopr mspaces Arrow te (string "->") <|> mbbr (br typeExpr)
         where te = fmap Var var <|> fmap TAtom atom <|> (mbbr $ br typeExpr)

atom :: Parser Atom
atom =
  ident `seq` \i ->
  many atomtail `seq` \at ->
  ret $ Atom i at

atomtail :: Parser (Either Atom String)
atomtail = (fmap Left $ mbbr $ br atom) <|>
           (fmap Right var) <|>
           (fmap Left $ fmap (\x -> Atom x []) ident)
  
relation :: Parser Relation
relation =
  atom `seq` \a ->
  mspaces `seq` \_ ->
  mrb `seq` \rb ->
  mspaces `seq` \_ ->
  char '.' `seq` \_ ->
  ret $ Relation a rb
    where mrb = quest $
            mspaces `seq` \_ ->
            string ":-" `seq` \_ ->
            relationbody `seq` \rb ->
            ret rb

relationbody :: Parser RelationBody
relationbody = disj

disj :: Parser RelationBody
disj = binopr mspaces Disj conj (char ';')

conj :: Parser RelationBody
conj = binopr mspaces Conj expr (char ',')

expr :: Parser RelationBody
expr = br disj <|> fmap RAtom atom

list :: Parser Atom
list = (fmap makelist $ arrayBr mspaces (string "[") el (string ",") (string "]")) <|>
       (fmap makelist $ bracketed' mspaces (string "[") (string "]") htlist)
      where el = fmap Left atom <|> fmap Right var <|> fmap Left list
            htlist =
              el `seq` \el1 ->
              mspaces `seq` \_ ->
              char '|' `seq` \_ ->
              var `seq` \v ->
              ret [el1, Right v]
              
makelist :: [Either Atom String] -> Atom
makelist []     = nil
makelist (a:as) = cons a $ Left $ makelist as

ident :: Parser String
ident =
  mspaces `seq` \_ ->
  (lower <|> char '_') `seq` \b ->
  many (alphanum <|> char '_') `seq` \e ->
  (guard (\x -> (x /= "type" && x /= "module")) % b:e) `seq` \_ ->
  ret $ b:e

var :: Parser String
var =
  mspaces `seq` \_ ->
  upper `seq` \b ->
  many (alphanum <|> char '_') `seq` \e ->
  ret (b:e)


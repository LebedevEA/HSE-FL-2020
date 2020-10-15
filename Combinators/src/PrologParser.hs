module PrologParser where

import Prelude hiding (seq, fmap, array, any)
import CombinatorLib
import PrologAst

testParser :: Show a => Parser a -> String -> IO ()
testParser parser string = print $ runParser parser $ Text string (1, 1)


-- TODO (fmap (safeFoldl1 (++))) to one func
mspaces = (fconcat $ many1 $ commentsingle (string "--")) <|> (fconcat $ many1 $ commentmulti (string "/*") (string "*/")) <|> spaces

m1spaces = (fconcat $ many1 $ commentsingle (string "--")) <|> (fconcat $ many1 $ commentmulti (string "/*") (string "*/")) <|> (many1 $ foldl1 (<|>) $ map char "\t\n ")

token :: Parser Token
token = (
    ident `seq` \i ->
    ret $ TIdent i
  ) <|> (
    var `seq` \v ->
    ret $ TVar v
  ) <|> (
    char ',' `seq` \_ ->
    ret Comma
  ) <|> (
    char ';' `seq` \_ ->
    ret Semi
  ) <|> (
    char '(' `seq` \_ ->
    ret Lbr
  ) <|> (
    char ')' `seq` \_ ->
    ret Rbr
  ) <|> (
    char '.' `seq` \_ ->
    ret Dot
  ) <|> (
    string ":-" `seq` \_ ->
    ret Cork
  )

  
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
typeExpr = (
    mspaces `seq` \_ ->
    char '(' `seq` \_ ->
    typeExpr `seq` \te ->
    mspaces `seq` \_ ->
    char ')' `seq` \_ ->
    ret te
  ) <|> (
    binopl mspaces Arrow typeExpr (skip mspaces $ string "->")
  )

atom :: Parser Atom
atom =
  ident `seq` \i ->
  atomtail `seq` \at ->
  ret $ Atom i at

atomtail :: Parser [Either Atom String]
atomtail = (
    mspaces `seq` \_ ->
    char '(' `seq` \_ ->
    atomtail `seq` \at ->
    mspaces `seq` \_ ->
    char ')' `seq` \_ ->
    mspaces `seq` \_ ->
    (quest atomtail) `seq` \atl ->
    case atl of
      Nothing -> ret at
      Just as -> ret (at ++ as)
  ) <|> (
    array mspaces eas mspaces
  )
    where eas = Parser $ \t ->
            case runParser atom' t of
              Left _          -> case runParser var t of
                                   Left  err       -> Left err
                                   Right (res, t') -> Right ((Right res), t')
              Right (res, t') -> Right ((Left res), t')
          atom' = (
              mspaces `seq` \_ ->
              char '(' `seq` \_ ->
              atom' `seq` \a ->
              mspaces `seq` \_ ->
              char ')' `seq` \_ ->
              ret a
            ) <|> (
              atom `seq` \a ->
              ret a
            )
  
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
disj = binopl mspaces Disj conj (char ';')

conj :: Parser RelationBody
conj = binopl mspaces Conj expr (char ',')

expr :: Parser RelationBody
expr = (
    mspaces `seq` \_ ->
    char '(' `seq` \_ ->
    disj `seq` \a ->
    mspaces `seq` \_ ->
    char ')' `seq` \_ ->
    ret a
  ) <|> (
    fmap RAtom atom
  )

ident :: Parser String
ident =
  mspaces `seq` \_ ->
  (lower <|> char '_') `seq` \b ->
  many (alphanum <|> char '_') `seq` \e ->
  ret (b:e)

var :: Parser String
var =
  mspaces `seq` \_ ->
  upper `seq` \b ->
  many (alphanum <|> char '_') `seq` \e ->
  ret (b:e)


module PrologParser where

import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec

import PrologAst

parserProg :: String -> Either ParseError Prolog
parserProg = parse (do r <- progParser; eof; return r) ""

parserAtom :: String -> Either ParseError Atom
parserAtom = parse (do r <- parseAtom; eof; return r) ""

parserTypeExpr :: String -> Either ParseError TypeBody
parserTypeExpr = parse (do r <- parseTypeBody; eof; return r) ""

parserType :: String -> Either ParseError Type
parserType = parse (do r <- parseType; eof; return r) ""

parserModule :: String -> Either ParseError Module
parserModule = parse (do r <- parseModule; eof; return r) ""

parserList :: String -> Either ParseError List
parserList = parse (do r <- parseList; eof; return r) ""

parserRelation :: String -> Either ParseError Def
parserRelation = parse (do r <- parseRel; eof; return r) ""

parseIdent :: Parser Ident
parseIdent = try $ do
  b <- spaces >> (letter <|> char '_');
  e <- many (alphaNum <|> char '_')
  spaces
  guard $ "type" /= (b:e)
  guard $ "module" /= (b:e)
  return (b:e)

parseIdentNotVar :: Parser Ident
parseIdentNotVar = try $ do
  b <- spaces >> (lower <|> char '_')
  e <- many (alphaNum <|> char '_')
  spaces
  guard $ "type" /= (b:e)
  guard $ "module" /= (b:e)
  return (b:e)

parseVar :: Parser Var
parseVar = try $ do
  b <- spaces >> upper
  e <- many (alphaNum <|> char '_')
  spaces
  return (Variable $ b:e)

parseAtom :: Parser Atom
parseAtom = parseAtom' parseIdentNotVar

parseAtom' :: Parser Ident -> Parser Atom
parseAtom' parseIdent' =
  try (do
      id <- spaces >> parseIdent'
      at <- spaces >> parseAtomTail
      spaces
      return (Seq (Id id) at)
  ) <|>
  (do
      a <- spaces >> parseIdent'
      spaces
      return (Id a)
  )

parseAtomTail :: Parser Atom
parseAtomTail =
  try (parseAtom' parseIdent) <|>
  try (do
      ba <- spaces >> char '(' >> spaces >> parseBracketedAtom
      at <- spaces >> char ')' >> spaces >> parseAtomTail
      spaces
      return (Seq ba at)
  ) <|>
  try (do
      a <- spaces >> char '(' >> spaces >> parseBracketedAtom
      spaces >> char ')' >> spaces
      return a
  ) <|>
  try (do
       l <- spaces >> parseList
       a <- spaces >> (parseAtom' parseIdent)
       spaces
       return (Seq (AtList l) a)
  ) <|>
  (do
       l <- spaces >> parseList
       spaces
       return (AtList l)
  )

parseBracketedAtom :: Parser Atom
parseBracketedAtom =
  try parseAtom <|>
  (do
      ba <- spaces >> char '(' >> spaces >> parseBracketedAtom
      spaces >> char ')' >> spaces
      return ba
  )

parseDef :: Parser Def
parseDef =
  try (do
     h <- parseHead
     char '.'
     return (DefJust h)
  ) <|>
  parseRel

parseRel :: Parser Def
parseRel = try $ do
  h <- spaces >> parseHead
  b <- spaces >> string ":-" >> spaces >> parseBody
  spaces >> char '.' >> spaces
  return (Rel h b)

parseBody :: Parser Body
parseBody = parseDisj

parseDisj :: Parser Body
parseDisj =
  try (do
      c <- spaces >> parseConj
      d <- spaces >> char ';' >> spaces >> parseDisj
      spaces
      return (Disj c d)
  ) <|>
  (do
      c <- spaces >> parseConj
      spaces
      return c
  )

parseConj :: Parser Body
parseConj =
  try (do
      e <- spaces >> parseExpr
      c <- spaces >> char ',' >> spaces >> parseConj
      spaces
      return (Conj e c)
  ) <|>
  (do
      e <- spaces >> parseExpr
      return e
  )

parseExpr :: Parser Body
parseExpr =
  try (do
      d <- spaces >> char '(' >> spaces >> parseDisj
      spaces >> char ')' >> spaces
      return d
  ) <|>
  (do
     a <- spaces >> parseAtom
     spaces
     return (BodyJust a)
  )

parseModule :: Parser Module
parseModule = try $ do
  id <- spaces >> string "module" >> many1 space >> parseIdent
  spaces >> char '.' >> spaces
  return (Name id)

parseTypeBody :: Parser TypeBody
parseTypeBody =
  try (do
      f <- spaces >> parseAtom
      s <- spaces >> string "->" >> spaces >> parseTypeBody
      spaces
      return (TypeSeq (TypeJust f) s)
  ) <|>
  try (do
      a <- spaces >> parseAtom
      spaces
      return (TypeJust a)
  ) <|>
  try (do
      f <- spaces >> char '(' >> spaces >> parseTypeBody
      s <- spaces >> char ')' >> spaces >> string "->" >> parseTypeBody
      spaces
      return (TypeSeq f s)
  ) <|>
  (do
      b <- spaces >> char '(' >> spaces >> parseTypeBody
      spaces >> char ')' >> spaces
      return b
  )
      
parseType :: Parser Type
parseType = try $ do
  n <- spaces >> string "type" >> spaces >> parseIdent
  b <- spaces >> parseTypeBody
  spaces >> char '.' >> spaces
  return (TypeDef n b)

parseList :: Parser List
parseList =
  try (do
      l <- parseHTList
      return (ListDefHT l)
  ) <|>
  (do
      l <- parseDefList
      return (ListDefDef l)
  )

parseHTList :: Parser HTList
parseHTList =
  try (do
      a <- spaces >> char '[' >> spaces >> parseAtom
      v <- spaces >> char '|' >> spaces >> parseVar
      spaces >> char ']' >> spaces
      return (HTListAtomTail a v)
  ) <|>
  try (do
      v1 <- spaces >> char '[' >> spaces >> parseVar
      v2 <- spaces >> char '|' >> spaces >> parseVar
      spaces >> char ']' >> spaces
      return (HTListVarTail v1 v2)
  ) <|>
  (do
      l <- spaces >> char '[' >> spaces >> parseList
      v <- spaces >> char '|' >> spaces >> parseVar
      spaces >> char ']' >> spaces
      return (HTListListTail l v)
  )

parseDefList :: Parser DefList
parseDefList = try $ do
  l <- spaces >> char '[' >> spaces >> parseDefList'
  spaces
  return l

parseDefList' :: Parser DefList
parseDefList' =
  try (do
      spaces >> char ']' >> spaces
      return DefNil
  ) <|>
  try (do
      e <- spaces >> parseVar
      t <- spaces >> parseListTail
      spaces
      return (DefConsV e t)
  ) <|>
  try (do
      e <- spaces >> parseAtom
      t <- spaces >> parseListTail
      spaces
      return (DefConsA e t)
  ) <|>
  try (do
      e <- spaces >> parseList
      t <- spaces >> parseListTail
      spaces
      return (DefConsL e t)
  )

parseListTail :: Parser DefList
parseListTail =
  try (do
      spaces >> char ']' >> spaces
      return DefNil
  ) <|>
  try (do
      spaces >> char ','
      e <- spaces >> parseVar
      l <- spaces >> parseListTail
      spaces
      return (DefConsV e l)
  ) <|>
  try (do
      spaces >> char ','
      e <- spaces >> parseAtom
      l <- spaces >> parseListTail
      spaces
      return (DefConsA e l)
  ) <|>
  (do
      spaces >> char ','
      e <- spaces >> parseList
      l <- spaces >> parseListTail
      spaces
      return (DefConsL e l)
  )

parseProlog :: Parser Prolog
parseProlog = try $ do
  m <- spaces >> parseModule
  t <- spaces >> many1 (spaces >> parseType)
  d <- spaces >> many1 (spaces >> parseDef)
  spaces
  return (Pr m t d)
  
parseHead = parseAtom
progParser = parseProlog

module CombinatorLib where

import Prelude hiding (seq, fmap, array, any)

newtype Parser a = Parser { runParser :: Text -> Either SyntaxError (a, Text) }

data SyntaxError = SyntaxError { errPos :: Pos, errName :: String }
                 deriving Show

type Col = Integer
type Line = Integer

type Pos = (Col, Line)

data Text = Text { str :: String, pos :: Pos }
          deriving Show

infixl 3 <|>

incPos :: Char -> Pos -> Pos
incPos x (col, line) | x == '\n' = (1, line + 1)
                     | otherwise = (col + 1, line)

char :: Char -> Parser Char
char c = Parser $ \t ->
  case str t of
    (x:xs) | x == c -> Right (c, Text xs $ incPos x $ pos t)
    (x:_)           -> Left $ SyntaxError (pos t) $ "Unexpected symbol " ++ [x]
    []              -> Left $ SyntaxError (pos t) $ "Unexpected EoF"

string :: String -> Parser String
string []     = Parser $ \t -> Right ("", t)
string (x:xs) = Parser $ \t ->
  case runParser (char x) t of
    Left err        -> Left err
    Right (res, t') -> runParser (fmap (res:) $ string xs) t'
                             
(<|>) :: Parser a -> Parser a -> Parser a
(<|>) p q = Parser $ \t ->
  case runParser p t of
    Right res -> Right res
    Left _    -> runParser q t

digit :: Parser Char
digit = foldl1 (<|>) $ map char "0123456789"

alpha :: Parser Char
alpha = foldl1 (<|>) $ map char "abcdefghijklmnopqrstuvwxyz"

alphanum :: Parser Char
alphanum = digit <|> alpha

seq :: Parser a -> (a -> Parser b) -> Parser b
seq p f = Parser $ \t ->
  case runParser p t of
    Left e          -> Left e
    Right (res, t') -> runParser (f res) t'

ret :: a -> Parser a
ret x = Parser $ \t -> Right (x, t)

many :: Parser a -> Parser [a]
many p = Parser $ \t ->
  case runParser p t of
    Right (res, t') ->
      case runParser (many p) t' of
        Right (res', t'') -> Right ((res:res'), t'')
        Left _            -> Right ([res], t')
    Left _          -> Right ([], t)

many1 :: Parser a -> Parser [a]
many1 p =
  p `seq` \h ->
  fmap (h:) $ many p

fmap :: (a -> b) -> Parser a -> Parser b
fmap f p = Parser $ \t ->
  case runParser p t of
    Right (res, t') -> Right (f res, t')
    Left e          -> Left e


list :: Parser elem -> Parser sep -> Parser [elem]
list elem sep =
  elem `seq` \h ->
  fmap (h:) $ (many $ sep `seq` \_ -> elem)
  
arrayBr :: Parser String -> Parser el -> Parser Char -> Parser String -> Parser String -> Parser [el]
arrayBr lbr el ws sep rbr =
  lbr `seq` \_ ->
  (many ws) `seq` \_ ->
  array el ws sep `seq` \res ->
  (many ws) `seq` \_ ->
  rbr `seq` \_ ->
  ret res

array :: Parser el -> Parser Char -> Parser String -> Parser [el]
array el' ws' sep' =
  list el sep
    where el  = ws `seq` \_ ->
                el' `seq` \res ->
                ws `seq` \_ ->
                ret res
          sep = ws `seq` \_ ->
                sep' `seq` \res ->
                ws `seq` \_ ->
                ret res
          ws  = many ws'

commentmulti :: Parser String -> Parser String -> Parser String
commentmulti open close =
  open `seq` \o ->
  (fmap (foldl1 (++)) $ many inner) `seq` \res ->
  close `seq` \c ->
  ret $ o ++ res ++ c
  where inner = anyBut `seq` \l ->
                fmap (foldl1 (++)) $ many $ commentmulti open close `seq` \m ->
                anyBut `seq` \r ->
                ret $ (l:m) ++ [r]
        anyBut = Parser $ \t -> case runParser close t of -- TODO выделить в отдельную функцию
                                  Left _  -> case runParser open t of
                                               Left _  ->
                                                 runParser any t
                                               Right _ ->
                                                 Left $ SyntaxError (pos t) "Missmatch brackets in comments!"
                                  Right _ -> Left $ SyntaxError (pos t) "Missmatch brackets in comments!"

any :: Parser Char
any = Parser $ \(Text (x:xs) pos) -> Right (x, Text xs $ incPos x pos)

-- skipuntil :: Parser a -> Parser String
-- skipuntil endl = Parser $ \t ->
--   case runParser endl t of
--     Left _ -> 

-- commentsingle :: Parser String -> ParserString
-- commentsingle beg =
--   beg `seq` \b ->
--   skipuntil '\n' `seq`

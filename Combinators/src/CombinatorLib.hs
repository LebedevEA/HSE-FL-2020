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

spaces :: Parser String
spaces = many $ foldl1 (<|>) $ map char "\t\n "

skip ws p = ws `seq` \_ ->
            p `seq` \r ->
            ws `seq` \_ ->
            ret r

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
  
arrayBr :: Parser String -> Parser String -> Parser el -> Parser String -> Parser String -> Parser [el]
arrayBr userspaces lbr el sep rbr =
  lbr `seq` \_ ->
  (skip userspaces $ array userspaces el sep) `seq` \res ->
  rbr `seq` \_ ->
  ret res

array :: Parser String -> Parser el -> Parser String -> Parser [el]
array userspaces el' sep' = list el sep
    where el  = skip userspaces el'
          sep = skip userspaces sep'

commentmulti :: Parser String -> Parser String -> Parser String
commentmulti open close =
  open `seq` \o ->
  (fmap (safeFoldl1 (++)) $ many inner) `seq` \res ->
  close `seq` \c ->
  ret $ o ++ res ++ c
  where inner = anyBut' `seq` \l ->
                (fmap (safeFoldl1 (++)) $ many $ commentmulti open close) `seq` \m ->
                ret $ (l:m)
        anyBut' = anyBut (open <|> close)


safeFoldl1 :: ([a] -> [a] -> [a]) -> [[a]] -> [a]
safeFoldl1 _ [] = []
safeFoldl1 f xs = foldl1 f xs

anyBut :: Parser String -> Parser Char
anyBut s = Parser $ \t ->
  case runParser s t of
    Left  _        -> runParser any t
    Right (str, _) -> Left $ SyntaxError (pos t) $ "String " ++ str ++ " is unexpected!"

any :: Parser Char
any = Parser $ \(Text str pos) ->
  case str of
    x:xs -> Right (x, Text xs $ incPos x pos)
    []   -> Left $ SyntaxError (pos) $ "Unexpected EoF"

-- including endl
skipuntil :: Parser String -> Parser String
skipuntil endl =
  (many $ anyBut endl) `seq` \r ->
  endl `seq` \_ ->
  ret r

commentsingle :: Parser String -> Parser String
commentsingle beg =
  beg `seq` \b ->
  (skipuntil $ string "\n") `seq` \e ->
  ret $ b ++ e

binop :: ((expr -> expr -> expr) -> [expr] -> expr) -> Parser String -> (expr -> expr -> expr) -> Parser expr -> Parser op -> Parser expr
binop fold1 userspaces f expr' op' = fmap (fold1 f) $ list expr op
                               where expr = skip userspaces expr'
                                     op   = skip userspaces op'

binopl = binop foldl1
binopr = binop foldr1


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

incCol :: Pos -> Pos
incCol (col, line) = (col + 1, line)

char :: Char -> Parser Char
char c = Parser $ \t ->
  case str t of
    (x:xs) | x == c -> Right (c, Text xs $ incCol $ pos t)
    (x:_)           -> Left $ SyntaxError (pos t) $ "Unexpected symbol " ++ [x]
    []              -> Left $ SyntaxError (pos t) $ "Unexpected EoF"
                             
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
  
arrayBr :: Parser lbr -> Parser el -> Parser ws -> Parser sep -> Parser rbr -> Parser [el]
arrayBr lbr el ws sep rbr =
  lbr `seq` \_ ->
  ws `seq` \_ ->
  array el ws sep `seq` \res ->
  ws `seq` \_ ->
  rbr `seq` \_ ->
  ret res

--                   TODO  Single
array :: Parser el -> Parser ws -> Parser sep -> Parser [el]
array el' ws sep' = Parser $ \t ->
  case runParser el t of
      Left  _         -> Right ([], t)
      Right (res, t') ->
        runParser (fmap (res:) $ (many (sep `seq` \_ -> el))) t'
    where el  = ws `seq` \_ ->
                el' `seq` \res ->
                ws `seq` \_ ->
                ret res
          sep = ws `seq` \_ ->
                sep' `seq` \res ->
                ws `seq` \_ ->
                ret res 

--                                Single
comment :: Parser String -> Parser Char -> Parser String -> Parser String
comment open any' close =
  open `seq` \o ->
  (fmap (foldl1 (++)) $ many inner) `seq` \res ->
  close `seq` \c ->
  ret $ o ++ res ++ c
  where inner = anyBut `seq` \l ->
                fmap (foldl1 (++)) $ many $ comment open any' close `seq` \m ->
                anyBut `seq` \r ->
                ret $ l ++ m ++ r
        anyBut = Parser $ \t -> case runParser close t of
                                  Left _  -> case runParser open t of
                                               Left _  ->
                                                 runParser any t
                                               Right _ ->
                                                 Left $ SyntaxError (pos t) "Missmatch brackets in comments!"
                                  Right _ -> Left $ SyntaxError (pos t) "Missmatch brackets in comments!"
        any = many any'



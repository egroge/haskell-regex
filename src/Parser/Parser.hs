{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Parser.Parser (
    Parser, 
    parse,
    anyChar,
    eof,
    satisfy,
    pos,
    choice,
    oneOf,
    noneOf,
    char,
    string,
    branch,
    select,
    (>?>),
    option,
    maybeP
) where
import Control.Applicative
import Control.Monad.State

newtype Parser a = P (StateT (String, Int) Maybe a) deriving (Functor, Applicative, Alternative, Monad, MonadState (String, Int), MonadFail)

runParser :: Parser a -> String -> Maybe (a, (String, Int))
runParser (P p) s = runStateT p (s, 0)

parse :: Parser a -> String -> Maybe a
parse p input = fmap fst (runParser p input)

eof :: Parser ()
eof = 
  do [] <- gets fst
     return ()

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = 
  do (input, offset) <- get
     case input of
       c:cs | f c -> put (cs, offset + 1) >> return c
       _          -> empty

anyChar :: Parser Char
anyChar = satisfy (const True)

pos :: Parser Int
pos = gets snd

choice :: [Parser a] -> Parser a
choice = foldr (<|>) empty

oneOf :: [Char] -> Parser Char
oneOf = choice . map char

noneOf :: [Char] -> Parser Char
noneOf cs = satisfy (not . flip elem cs)

char :: Char -> Parser Char
char c = satisfy (== c) -- anyChar >?> (== c)

string :: String -> Parser String
string = traverse char

branch :: Parser (Either a b) -> Parser (a -> c) -> Parser (b -> c) -> Parser c
branch b l r =
  do e <- b
     case e of
       Left x -> l <*> pure x
       Right y -> r <*> pure y

select :: Parser (Either a b) -> Parser (a -> b) -> Parser b
select p q = branch p q (pure id)

option :: Parser a -> a -> Parser a
option p x = p <|> pure x

maybeP :: (a -> b) -> b -> Parser a -> Parser b
maybeP f x p = option (f <$> p) x

infixl 4 >?>
(>?>) :: Parser a -> (a -> Bool) -> Parser a
p >?> f = select (g <$> p) empty
  where
    g x = if f x then Right x else Left ()





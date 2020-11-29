-- TODO exposing all the internals like this just for testing is bad
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ApplicativeDo #-}
module 
Parser (Expr, Term (..), Op (..), Atom (..), CharClass (..), parse, parseRegex, parseRange, parseClassMember, parseCharacterClass, parseAtom, parseTerm) 
where

import Control.Applicative
import Control.Monad.State
import Data.Functor
import Data.List

-- TODO make CClass have a set not a list
type Expr = [Term]
data Term = TAtom Atom | TOp Atom Op deriving (Eq, Show)
data Atom = C Char | CClass Bool CharClass | Sub Expr deriving (Eq, Show)
data Op = Plus | Star | Optional deriving (Eq, Show)

data CharClass = Unrestricted | Restricted [Char]
  deriving (Show)

instance Eq CharClass where
  Unrestricted == Unrestricted = True
  Restricted cs == Restricted cs' = sort cs == sort cs'

newtype Parser a = P (StateT (String, Int) Maybe a) deriving (Functor, Applicative, Alternative, Monad, MonadState (String, Int), MonadFail)

runParser :: Parser a -> String -> Maybe (a, (String, Int))
runParser (P p) s = runStateT p (s, 0)

infixr 4 <:>
(<:>) :: Applicative f => f a -> f [a] -> f [a]
(<:>) = liftA2 (:)

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

infixl 4 >?>
(>?>) :: Parser a -> (a -> Bool) -> Parser a
p >?> f = select (g <$> p) empty
  where
    g x = if f x then Right x else Left ()

parseRange :: Parser CharClass
parseRange = do
  start <- anyChar
  char '-'
  end <- anyChar
  pure $ Restricted [start .. end]

parseMetaChar :: Parser CharClass
parseMetaChar = char '\\' *> (char 'w' $> Restricted alphaNumeric
                          <|> char 'd' $> Restricted numeric)
  where
    numeric      = ['0'..'9']
    alphaNumeric = concat [numeric, ['a'..'z'], ['_'], ['A'..'Z']]

parseBackslashedChar :: Parser Atom
parseBackslashedChar = CClass False <$> parseMetaChar <|> char '\\' *> (C <$> anyChar)

parseUnrestricted :: Parser CharClass
parseUnrestricted = char '.' $> Unrestricted

parseClassMember :: Parser CharClass
parseClassMember = parseRange <|> parseMetaChar <|> (Restricted . pure) <$> noneOf "[]"

{-newtype Parsec a = Parsec (forall r.
     (a -> String -> r) -- cok
  -> (a -> r)           -- eok
  -> (String -> r)      -- cerr
  -> r                  -- eerr
  -> String
  -> r
)-}

option :: Parser a -> a -> Parser a
option p x = p <|> pure x

maybeP :: (a -> b) -> b -> Parser a -> Parser b
maybeP f x p = option (f <$> p) x

parseCharacterClass :: Parser Atom
parseCharacterClass =
        CClass False <$> (parseUnrestricted <|> parseMetaChar <|> parseRange)
    <|> char '[' *> (combine <$> maybeP (const True) False (char '^') <*> many parseClassMember) <* char ']'
  where
    combine inverted ms = CClass inverted (foldl' merge (Restricted []) ms)

    -- Cannot get unrestricted when creating a character class
    merge (Restricted cs) (Restricted cs') = Restricted $ nub (cs' ++ cs)
    merge _ _ = undefined


{-chainPre :: Parser (b -> b) -> Parser a -> (a -> b) -> Parser b
chainPre op p f = flip (foldr ($)) <$> many op <*> (f <$> p)

chainPost :: Parser a -> Parser (b -> b) -> (a -> b) -> Parser b
chainPost p op f = foldl' (flip ($)) <$> (f <$> p) <*> many op

pfoldr :: (a -> b -> b) -> b -> Parser a -> Parser b
pfoldr f k p = chainPre (f <$> p) (pure k) id

pfoldl :: (b -> a -> b) -> b -> Parser a -> Parser b
pfoldl f k p = chainPost (pure k) (flip f <$> p) id

--nat = pfoldl (\n d -> n * 10 + charToDigit d) 0 digit

chainl1 :: Parser a -> Parser (b -> a -> b) -> (a -> b) -> Parser b
chainl1 p op f = chainPost p (flip <$> op <*> p) f

chainr1 :: Parser a -> Parser (a -> b -> b) -> (a -> b) -> Parser b
chainr1 p op f = chainPre (p <**> op) p f
-}

parseAtom :: Parser Atom
parseAtom = char '(' *> (Sub <$> parseExpr) <* char ')' <|> parseCharacterClass <|> C <$> noneOf "[]()*+?."

parseTerm :: Parser Term
parseTerm = parseAtom <**> maybeP (flip TOp) TAtom parseOp
  where
    parseOp = char '+' $> Plus <|> char '*' $> Star <|> char '?' $> Optional

parseExpr :: Parser Expr
parseExpr = many parseTerm
  
{-class Regex r where
  star :: r -> r
  --or :: r -> r -> r
  comp :: r -> r -> r
  char :: Char -> r
  runRegex :: r -> String -> Maybe Int

instance Regex Expr where
  ...

pos :: Parser Int

instance Regex (Parser ()) where
  star = skipMany
  atom = item
  comp = (*>)
  runRegex p = runParser (p *> pos)

parseRegex :: Regex r => String -> Maybe r-}

parseRegex :: String -> Maybe Expr
parseRegex = parse parseExpr
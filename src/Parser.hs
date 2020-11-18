-- TODO exposing all the internals like this just for testing is bad
module 
Parser (Expr, Term (..), Op (..), Atom (..), CharClass (..), parse, parseRegex, parseRange, parseClassMember, parseCharacterClass, parseAtom, parseOp, parseTerm) 
where

import Control.Applicative
import Data.Functor
import Data.List

type Expr = [Term]

-- TODO make CClass have a set not a list
data Term = TAtom Atom | TOp Op
  deriving (Eq, Show)

data Atom = C Char | CClass Bool CharClass | Sub Expr
  deriving (Eq, Show)

data Op = Plus Atom | Star Atom | Optional Atom
  deriving (Eq, Show)

-- Not having the inverted Bool type here seems bad
data CharClass = Unrestricted | Restricted [Char]
  deriving (Show)

instance Eq CharClass where
  Unrestricted == Unrestricted = True
  Restricted cs == Restricted cs' = sort cs == sort cs'

newtype Parser a = P (String -> Maybe (a, String))

pureParser :: a -> Parser a
pureParser x = P (\input -> Just (x, input))

instance Functor Parser where
  fmap f p =
    P
      ( \input -> case runParser p input of
          Just (res, rem) -> Just (f res, rem)
          _ -> Nothing
      )

instance Applicative Parser where
  pure = pureParser
  p' <*> p'' = P p
    where
      p input = do
        (f, rem) <- runParser p' input
        (x, rem') <- runParser p'' rem
        return (f x, rem')

instance Monad Parser where
  return = pureParser
  p' >>= f = P p
    where
      p input = do
        (res, rem) <- runParser p' input
        runParser (f res) rem

failingParser :: Parser a
failingParser =
  let p _ = Nothing
   in P p

instance Alternative Parser where
  empty = failingParser
  p1 <|> p2 = P p
    where
      p input = case runParser p1 input of
        Just res -> Just res
        Nothing -> runParser p2 input

runParser :: Parser a -> String -> Maybe (a, String)
runParser (P f) = f

parse :: Parser a -> String -> Maybe a
parse p input = case runParser p input of
  Just (res, "") -> Just res
  _ -> Nothing

anyCharButA :: Char -> Parser Atom
anyCharButA c = C <$> anyCharBut c

anyCharA :: Parser Atom
anyCharA = C <$> anyChar

parseRange :: Parser CharClass
parseRange = do
  start <- anyChar
  char '-'
  end <- anyChar
  return $ Restricted [start .. end]

parseMetaChar :: Parser CharClass
parseMetaChar = char '\\' *> P p
  where
    numeric      = "0123456789"
    alphaNumeric = numeric ++ "abcdefghijklmnopqrstuvwxyz_ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    p ('w' : cs) = Just (Restricted alphaNumeric, cs)
    p ('d' : cs) = Just (Restricted numeric, cs)
    p _ = Nothing

parseBackslashedChar :: Parser Atom
parseBackslashedChar = CClass False <$> parseMetaChar <|> (char '\\' *> anyCharA)

parseUnrestricted :: Parser CharClass
parseUnrestricted = char '.' $> Unrestricted

parseClassMember :: Parser CharClass
parseClassMember = parseRange <|> parseMetaChar <|> Restricted <$> ((: []) <$> anyCharBut '[')

parseCharacterClass :: Parser Atom
parseCharacterClass =
  CClass False <$> parseUnrestricted
    <|> CClass False <$> parseMetaChar
    <|> CClass False <$> parseRange
    <|> char '[' *> char '^' *> (combine True <$> many parseClassMember) <* char ']'
    <|> char '[' *> (combine False <$> many parseClassMember) <* char ']'
  where
    combine inverted ms =
      CClass inverted (foldl merge (Restricted []) ms)

    -- Cannot get unrestricted when creating a character class
    merge (Restricted cs) ((Restricted cs')) =
      Restricted $ nub (cs' ++ cs)
    merge _ _ =
      undefined

parseOp :: Parser (Atom -> Op)
parseOp = char '+' $> Plus <|> char '*' $> Star <|> char '?' $> Optional

parseAtom :: Parser Atom
parseAtom = parseCharacterClass <|> char '(' *> (Sub <$> parseExpr) <|> anyCharA

parseTerm :: Parser Term
parseTerm = TOp <$> (parseOp <*> parseAtom) <|> TAtom <$> parseAtom

anyChar :: Parser Char
anyChar =
  let p (c : cs) = Just (c, cs)
      p "" = Nothing
   in P p

parseExpr :: Parser Expr
parseExpr = char ')' $> [] <|> (:) <$> parseTerm <*> (parseExpr <|> pure [])

parseRegex :: String -> Maybe Expr
parseRegex = parse parseExpr

anyCharBut :: Char -> Parser Char
anyCharBut c = do
  c' <- anyChar
  if c == c' then failingParser else return c'

char :: Char -> Parser ()
char c = do
  c' <- anyChar
  if c == c' then return () else failingParser
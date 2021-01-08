{-# LANGUAGE ApplicativeDo #-}
module ParseRegex where

import Control.Applicative
    ( Applicative(liftA2), (<**>), Alternative(many, (<|>)) )
import Data.Functor
import Data.List
import Parser.Parser

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

infixr 4 <:>
(<:>) :: Applicative f => f a -> f [a] -> f [a]
(<:>) = liftA2 (:)

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
parseClassMember = parseRange <|> parseMetaChar <|> Restricted . pure <$> noneOf "[]"

parseCharacterClass :: Parser Atom
parseCharacterClass =
        CClass False <$> (parseUnrestricted <|> parseMetaChar <|> parseRange)
    <|> char '[' *> (combine <$> maybeP (const True) False (char '^') <*> many parseClassMember) <* char ']'
  where
    combine inverted ms = CClass inverted (foldl' merge (Restricted []) ms)

    -- Cannot get unrestricted when creating a character class
    merge (Restricted cs) (Restricted cs') = Restricted $ nub (cs' ++ cs)
    merge _ _ = undefined

parseAtom :: Parser Atom
parseAtom = char '(' *> (Sub <$> parseExpr) <* char ')' <|> parseCharacterClass <|> C <$> noneOf "[]()*+?."

parseTerm :: Parser Term
parseTerm = parseAtom <**> maybeP (flip TOp) TAtom parseOp
  where
    parseOp = char '+' $> Plus <|> char '*' $> Star <|> char '?' $> Optional

parseExpr :: Parser Expr
parseExpr = many parseTerm
  
parseRegex :: String -> Maybe Expr
parseRegex = parse parseExpr
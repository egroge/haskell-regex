module Lib (findMatch, RegexError) where

import ParseRegex (parseRegex)
import MatchRegex (find)

type RegexError = String

findMatch :: String -> String -> Either RegexError (Maybe (String, Int))
findMatch rawRegex searchStr 
  = case parseRegex rawRegex of 
      Just regex -> Right $ find regex searchStr
      Nothing    -> Left "Could not compile regular expression"


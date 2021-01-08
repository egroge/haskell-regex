{-# LANGUAGE TupleSections #-}

module MatchRegex where 

import ParseRegex

matchCharacter :: Atom -> String -> Maybe (String, String)
matchCharacter (C c) (c' : cs) = if c == c' then Just ([c], cs) else Nothing
matchCharacter _ _             = Nothing

matchCharacterClass :: Atom -> String -> Maybe (String, String)
matchCharacterClass (CClass False (Restricted allowed)) (c : str)
  = if c `elem` allowed then Just ([c], str) else Nothing
matchCharacterClass (CClass True (Restricted disAllowed)) (c : str)
  = if c `elem` disAllowed then Nothing else Just ([c], str)
matchCharacterClass (CClass False Unrestricted) (c : str)
  = Just ([c], str)
matchCharacterClass _ _ 
  = Nothing

matchAtom :: Atom -> String -> Maybe (String, String)
matchAtom a@(C _) str           = matchCharacter a str
matchAtom a@(CClass _ _) str    = matchCharacterClass a str
matchAtom (Sub expr) ('(' : cs) = case matchExpression expr cs of 
                                    Just (matched, ')' : rem) -> Just (matched, rem)
                                    other                     -> other
matchAtom _ _                   = Nothing

matchOp :: Term -> String -> Maybe (String, String)
matchOp (TOp a Optional) s 
  = case matchAtom a s of 
      Nothing             -> Just ("", s)
      matched             -> matched
matchOp op@(TOp a Star) s 
  = case matchAtom a s of 
      Nothing             -> Just ("", s)
      -- We know this pattern will never fail
      Just (matched, rem) -> let Just (matched', rem') = matchOp op rem 
                             in  Just (matched ++ matched', rem')
matchOp (TOp a Plus) s 
  = case matchAtom a s of 
      Just (matched, rem) -> case matchOp (TOp a Star) rem of 
                               Just (matched', rem') -> Just (matched ++ matched', rem')
                               Nothing               -> Just (matched, rem)
      Nothing             -> Nothing
                         

matchTerm :: Term -> String -> Maybe (String, String)
matchTerm (TAtom a) s    = matchAtom a s
matchTerm op@(TOp _ _) s = matchOp op s

matchExpression :: Expr -> String -> Maybe (String, String) 
matchExpression [] s = Just ("", s)
matchExpression (t : ts) s 
  = do 
      (firstTerm, rem) <- matchTerm t s 
      (remainingTerms, rem') <- matchExpression ts rem 
      return (firstTerm ++ remainingTerms, rem')

match :: Expr -> String -> Maybe String 
match e s = case matchExpression e s of 
              Just (matched, _) -> Just matched 
              Nothing           -> Nothing

find :: Expr -> String -> Maybe (String, Int)
find e s = find' e s 0
  where 
    find' e "" n = (, n) <$> match e ""
    find' e s n  = case match e s of 
                     Just matched -> Just (matched, n)
                     Nothing      -> find' e (tail s) (n + 1)
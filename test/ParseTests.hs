module ParseTests (regexTests) where

import Parser.Parser
import ParseRegex
import Test.HUnit

latin :: [Char]
latin = "abcdefghijklmnopqrstuvwxyz0123456789_ABCDEFGHIJKLMNOPQRSTUVWXYZ"

parseRangeTest = TestCase (assertEqual "parseRange failed" expected (parse parseRange "A-K"))
  where
    expected = Just (Restricted "ABCDEFGHIJK")

parseClassMemberTest1 = TestCase (assertEqual "parseClassMember1 failed" expected (parse parseClassMember "m"))
  where
    expected = Just (Restricted "m")

parseClassMemberTest2 = TestCase (assertEqual "parseClassMember2 failed" expected (parse parseClassMember "\\w"))
  where
    expected = Just (Restricted latin)

parseClassTest1 = TestCase (assertEqual "parseClassTest1 failed" expected (parse parseCharacterClass "[^)8]"))
  where
    expected = Just (CClass True (Restricted ")8"))

parseClassTest2 = TestCase (assertEqual "parseClassTest2 failed" expected (parse parseCharacterClass "[?a-d]"))
  where
    expected = Just (CClass False (Restricted "?abcd"))

parseAtomTest1 = TestCase (assertEqual "parseAtomTest1 failed" expected (parse parseAtom "\\w"))
  where
    expected = Just (CClass False (Restricted latin))

parseAtomTest2 = TestCase (assertEqual "parseAtomTest2 failed" expected (parse parseAtom "a"))
  where
    expected = Just (C 'a')

parseAtomTest3 = TestCase (assertEqual "parseAtomTest3 failed" expected (parse parseAtom "[\\w?]"))
  where
    expected = Just (CClass False (Restricted ('?' : latin)))

parseAtomTest4 = TestCase (assertEqual "parseAtomTest4 failed" expected (parse parseAtom "."))
  where
    expected = Just (CClass False Unrestricted)

parseAtomTest5 = TestCase (assertEqual "parseAtomTest5 failed" expected (parse parseAtom "(ICL?)"))
  where
    expected = Just (Sub [TAtom (C 'I'), TAtom (C 'C'), TOp (C 'L') Optional])

parseRegexTest = TestCase (assertEqual "parseRegexTest failed" expected (parseRegex "a[0-2]+"))
  where 
    expected = Just [TAtom (C 'a'), TOp (CClass False (Restricted "012")) Plus]

regexTests = test [
    parseRangeTest, 
    parseClassMemberTest1, 
    parseClassMemberTest2, 
    parseClassTest1, 
    parseClassTest2, 
    parseAtomTest1, 
    parseAtomTest2, 
    parseAtomTest3, 
    parseAtomTest4, 
    parseAtomTest5,
    parseRegexTest
  ]
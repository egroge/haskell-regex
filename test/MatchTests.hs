module MatchTests (matchTests) where

import ParseRegex
import MatchRegex
import Test.HUnit
import Data.Maybe (fromJust)

matchCharTest1 
  = TestCase (assertEqual "charTest1 failed" (Just ("a", "pple")) (matchCharacter (C 'a') "apple"))

matchCharTest2 
  = TestCase (assertEqual "charTest2 failed" Nothing (matchCharacter (C 'l') "apple"))

allowed = "iclICL"

matchCharClassTest1
  = TestCase (assertEqual "charClassTest1 failed" (Just ("i", "cl")) (matchCharacterClass cls "icl"))
  where 
    cls = CClass False (Restricted allowed)

matchCharClassTest2
  = TestCase (assertEqual "charClassTest2 failed" Nothing (matchCharacterClass cls "icl"))
  where 
    cls = CClass True (Restricted allowed)

matchCharClassTest3
  = TestCase (assertEqual "charClassTest3 failed" (Just ("i", "cl")) (matchCharacterClass cls "icl"))
  where 
    cls = CClass False Unrestricted

matchCharClassTest4
  = TestCase (assertEqual "charClassTest4 failed" Nothing (matchCharacterClass cls "(icl)"))
  where 
    cls = CClass False (Restricted allowed)

matchAtomTest1
  = TestCase (assertEqual "atomTest1 failed" (Just ("a", "pple")) (matchAtom (C 'a') "apple"))

matchAtomTest2
  = TestCase (assertEqual "atomTest2 failed" (Just ("i", "carus")) (matchAtom a "icarus"))
  where 
    a = CClass False (Restricted allowed)

matchAtomTest3
  = TestCase (assertEqual "atomTest3 failed" (Just ("car", "us")) (matchAtom a "carus"))
  where 
    a = Sub [TAtom (C 'c'), TAtom (C 'a'), TAtom (C 'r')]

subOpTest1 
  = TestCase (assertEqual "subOpTest1 failed" 
               (Just ("I am very hungry", " now"))
               (matchExpression expr "I am very hungry now"))
  where 
    expr = fromJust $ parseRegex "I am (very)* hungry"

plusOpTest1
  = TestCase (assertEqual "plus1 test failed" (Just ("aa", "b")) (matchOp op "aab"))
  where
    op = TOp (C 'a') Plus

plusOpTest2
  = TestCase (assertEqual "plus2 test failed" (Just ("a", "b")) (matchOp op "ab"))
  where
    op = TOp (C 'a') Plus

plusOpTest3
  = TestCase (assertEqual "plus3 test failed" Nothing (matchOp op "b"))
  where
    op = TOp (C 'a') Plus

starOpTest1
  = TestCase (assertEqual "star1 test failed" (Just ("aa", "b")) (matchOp op "aab"))
  where
    op = TOp (C 'a') Star

starOpTest2
  = TestCase (assertEqual "star2 test failed" (Just ("a", "b")) (matchOp op "ab"))
  where
    op = TOp (C 'a') Star

starOpTest3
  = TestCase (assertEqual "star3 test failed" (Just ("", "b")) (matchOp op "b"))
  where
    op = TOp (C 'a') Star

optionOpTest1
  = TestCase (assertEqual "option1 test failed" (Just ("a", "b")) (matchOp op "ab"))
  where
    op = TOp (C 'a') Optional

optionOpTest2
  = TestCase (assertEqual "option2 test failed" (Just ("", "b")) (matchOp op "b"))
  where
    op = TOp (C 'a') Optional

yeetRegex = [TAtom (C 'y'), TAtom (C 'e'), TOp (C 'e') Plus, TAtom (C 't')]

exprTest1 
  = TestCase (assertEqual "exprTest1 failed" 
               (Just ("yeeeeeeeeeeeet", " is a thing lame people say")) 
               (matchExpression yeetRegex "yeeeeeeeeeeeet is a thing lame people say"))

exprTest2 
  = TestCase (assertEqual "exprTest2 failed" 
               Nothing
               (matchExpression yeetRegex "yet is a thing normal people say"))

findTest1 
  = TestCase (assertEqual "findTest1 failed" 
               (Just ("yeet", 10)) 
               (find yeetRegex "The word 'yeet' is a thing lame people say"))

findTest2 
  = TestCase (assertEqual "findTest1 failed" 
               Nothing 
               (find yeetRegex "The word 'yet' is a thing normal people say"))

matchTests = test [
    matchCharTest1,
    matchCharTest2,
    matchCharClassTest1,
    matchCharClassTest2,
    matchCharClassTest3,
    matchCharClassTest4,
    matchAtomTest1, 
    matchAtomTest2, 
    matchAtomTest3, 
    plusOpTest1,
    plusOpTest2,
    plusOpTest3,
    starOpTest1,
    starOpTest2,
    starOpTest3,
    optionOpTest1,
    optionOpTest2,
    exprTest1,
    exprTest2,
    findTest1,
    findTest2,
    subOpTest1
  ]

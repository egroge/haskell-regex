module IntegrationTests (integrationTests) where

import Lib
import Test.HUnit

test1 = TestCase (assertEqual "test1 failed" 
                   (Right (Just ("yet", 7))) 
                   (findMatch "yee*t" "better yet, jam!"))

kenobiRegex :: String 
kenobiRegex = "(hello)? there[!?]*"

test2 = TestCase (assertEqual "test2 failed" 
                   (Right (Just ("hello there", 0))) 
                   (findMatch kenobiRegex "hello there. general kenobi."))
                  
test3 = TestCase (assertEqual "test3 failed" 
                   (Right (Just ("hello there!!!", 4))) 
                   (findMatch kenobiRegex "why hello there!!!. general kenobi."))
                  
test4 = TestCase (assertEqual "test4 failed" 
                   (Right (Just (" there?!", 5))) 
                   (findMatch kenobiRegex "is he there?! general kenobi."))
                  
test5 = TestCase (assertEqual "test5 failed" 
                   (Right Nothing)
                   (findMatch kenobiRegex "is he here?! general kenobi?"))

integrationTests = test [
    test1,
    test2,
    test3,
    test4,
    test5
  ]
                  

import Test.HUnit
import Parser

parseRangeTest = TestCase (assertEqual "parseRange failed" expected (parse parseRange "A-K"))
  where expected = Just (Restricted "ABCDEFGHIJK")

parseClassMemberTest1 = TestCase (assertEqual "parseClassMember1 failed" expected (parse parseClassMember "m"))
  where expected = Just (Restricted "m")

parseClassMemberTest2 = TestCase (assertEqual "parseClassMember2 failed" expected (parse parseClassMember "\\w"))
  where expected = Just (Restricted "abcdefghijklmnopqrstuvwxyz0123456789_ABCDEFGHIJKLMNOPQRSTUVWXYZ")

main :: IO ()
main = do 
    runTestTT parseRangeTest
    runTestTT parseClassMemberTest1
    runTestTT parseClassMemberTest2
    putStrLn "Tests finished"
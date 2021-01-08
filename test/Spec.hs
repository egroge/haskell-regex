import ParseTests
import Test.HUnit

main :: IO ()
main = do
  runTestTT regexTests 
  putStrLn "Tests finished"
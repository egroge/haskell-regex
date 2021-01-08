import ParseTests
import MatchTests
import Test.HUnit

main :: IO ()
main = do
  runTestTT parseTests 
  runTestTT matchTests 
  putStrLn "Tests finished"
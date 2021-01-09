import ParseTests
import MatchTests
import IntegrationTests
import Test.HUnit

main :: IO ()
main = do
  runTestTT parseTests 
  runTestTT matchTests
  runTestTT integrationTests 
  putStrLn "Tests finished"
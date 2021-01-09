module Main where

import Lib
import System.IO (hPutStrLn, stderr)
import System.Environment (getArgs)

printResult :: Either RegexError (Maybe (String, Int)) -> IO ()
printResult (Left e) 
  = hPutStrLn stderr e
printResult (Right Nothing) 
  = putStrLn "No matches found"
printResult (Right (Just (match, pos))) 
  = putStrLn $ "Match: " ++ match ++ " at character offset " ++ show pos

helpMsg :: String 
helpMsg = "haskell-regex [regex] [filePath | rawStr] opt(--raw-string)"

main :: IO ()
main = do 
  args <- getArgs
  case args of 
    [rawRegex, filePath] -> do 
      contents <- readFile filePath
      printResult $ findMatch rawRegex contents
    [rawRegex, rawStr, "--raw-string"] -> printResult $ findMatch rawRegex rawStr
    _ -> do 
      hPutStrLn stderr "Malformed input:"
      hPutStrLn stderr helpMsg
      

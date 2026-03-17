module Main where

import Converter (convert)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [inputFile, outputFile] -> do
      content <- readFile inputFile
      writeFile outputFile (convert content)
      putStrLn $ "Converted " ++ inputFile ++ " → " ++ outputFile
    _ -> putStrLn "Usage: md-to-html <input.md> <output.html>"
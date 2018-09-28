module Main where

import System.Environment

countData :: String -> (Int, Int, Int)
countData s = (length s, length (words s), length (lines s))

countStrFmt :: (Int, Int, Int) -> String
countStrFmt (chars, words, lines) =
  unwords ["chars:", show chars, "words:", show words, "lines:", show lines]

main :: IO ()
main = do
  args <- getArgs
  fileData <- readFile (head args)
  putStrLn $ (countStrFmt . countData) fileData

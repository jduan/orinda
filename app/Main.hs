module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment

countData :: T.Text -> (Int, Int, Int)
countData s = (charCount, wordCount, lineCount)
  where
    charCount = T.length s
    wordCount = (length . T.words) s
    lineCount = (length . T.lines) s

countStrFmt :: (Int, Int, Int) -> T.Text
countStrFmt (chars, words, lines) =
  T.pack
    (unwords ["chars:", show chars, "words:", show words, "lines:", show lines])

main :: IO ()
main = do
  args <- getArgs
  let fileName = head args
  input <- TIO.readFile fileName
  let summary = (countStrFmt . countData) input
  TIO.appendFile "stats.dat" summary
  TIO.putStrLn summary

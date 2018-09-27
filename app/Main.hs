module Main where

import Lib
import System.Environment
import System.IO

main :: IO ()
main = do
  args <- getArgs
  myFile <- openFile (head args) ReadMode
  hClose myFile
  putStrLn "done!"

module Main where

import Control.Monad (foldM)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import System.Environment
import System.Random (randomRIO)

main :: IO ()
main = do
  print "hello!"

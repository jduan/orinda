{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (foldM)
import qualified Data.ByteString as B
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.Text.IO as TIO
import System.Environment

type Author = T.Text

type Title = T.Text

data Book = Book
  { author :: Author
  , title :: Title
  } deriving (Show, Eq)

type Html = T.Text

-- Format a single book
bookToHtml :: Book -> Html
bookToHtml book = mconcat ["<p>\n", titleInTags, authorInTags, "</p>\n"]
  where
    titleInTags = mconcat ["<strong>", title book, "</strong>\n"]
    authorInTags = mconcat ["<em>", author book, "</em>\n"]

booksToHtml :: [Book] -> Html
booksToHtml books =
  mconcat
    [ "<html>\n"
    , "<head><title>books</title>"
    , "<meta charset='utf-8'/>"
    , "</head>\n"
    , "<body>\n"
    , booksHtml
    , "\n</body>\n"
    , "</html>"
    ]
  where
    booksHtml = (mconcat . map bookToHtml) books

book1 :: Book
book1 =
  Book
    { title = "The Conspiracy Against the Human Race"
    , author = "Ligotti, Thomas"
    }

book2 :: Book
book2 = Book {title = "A Short History of Decay", author = "Ciroan, Emil"}

book3 :: Book
book3 = Book {title = "The Tears of Eros", author = "Bataille, Georges"}

books = [book1, book2, book3]

type MarcRecordRaw = B.ByteString

type MarcLeaderRaw = B.ByteString

-- The MARC record consists of 3 main parts:
-- * the leader: length of the record, where to find the base record, etc
-- * the directory: where to find author for example
-- * the base record: where all the info you need is located
-- The leader is the first 24 bytes of the record.
leaderLength :: Int
leaderLength = 24

getLeader :: MarcRecordRaw -> MarcLeaderRaw
getLeader record = B.take leaderLength record

rawToInt :: B.ByteString -> Int
rawToInt = (read . T.unpack . E.decodeUtf8)

-- The first 5 bytes of the leader tells the length of the record
getRecordLength :: MarcLeaderRaw -> Int
getRecordLength leader = rawToInt (B.take 5 leader)

-- Break a ByteString into the first record and the remaining ByteString
nextAndRest :: B.ByteString -> (MarcRecordRaw, B.ByteString)
nextAndRest bytes = B.splitAt length bytes
  where
    length = (getRecordLength . getLeader) bytes

allRecords :: B.ByteString -> [MarcRecordRaw]
allRecords bytes =
  if bytes == B.empty
    then []
    else let (record, rest) = nextAndRest bytes
          in record : allRecords rest

-- You can download the MARC data here: (ohsu_ncnm_wscc_bibs.mrc)
-- https://archive.org/download/marc_oregon_summit_records/catalog_files/
main :: IO ()
main = do
  args <- getArgs
  marcData <- B.readFile (head args)
  let marcRecords = allRecords marcData
  print (length marcRecords)

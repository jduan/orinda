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

type MarcLeaderRaw = B.ByteString

type MarcDirectoryRaw = B.ByteString

type MarcBaseRecordRaw = B.ByteString

type MarcRecordRaw = B.ByteString

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

-- the 12th to the 16th bytes of the leader is the address of the base
-- record. Index is 0 based.
getBaseAddress :: MarcLeaderRaw -> Int
getBaseAddress leader = (rawToInt . B.take 5 . B.drop 12) leader

getDirectoryLength :: MarcLeaderRaw -> Int
getDirectoryLength leader = getBaseAddress leader - (leaderLength + 1)

getDirectory :: MarcRecordRaw -> MarcDirectoryRaw
getDirectory record = (B.take dirLength . B.drop leaderLength) record
  where
    dirLength = (getDirectoryLength . getLeader) record

getBaseRecord :: MarcRecordRaw -> MarcBaseRecordRaw
getBaseRecord record = B.drop baseAddress record
  where
    leader = getLeader record
    baseAddress = getBaseAddress leader

type MarcDirectoryEntryRaw = B.ByteString

-- each field in the directory is 12 bytes
-- tag of the field (first 3 bytes)
-- length of the field (next 4 bytes)
-- where the field starts relative to the base address (rest of the bytes)
dirEntryLength :: Int
dirEntryLength = 12

splitDirectory :: MarcDirectoryRaw -> [MarcDirectoryEntryRaw]
splitDirectory dir =
  if dir == B.empty
    then []
    else let (first, rest) = B.splitAt dirEntryLength dir
          in first : splitDirectory rest

data FieldMetadata = FieldMetadata
  { tag :: T.Text
  , fieldLength :: Int
  , fieldStart :: Int
  } deriving (Show, Eq)

makeFieldMetadata :: MarcDirectoryEntryRaw -> FieldMetadata
makeFieldMetadata entry = FieldMetadata tag length start
  where
    (rawTag, rest) = B.splitAt 3 entry
    tag = E.decodeUtf8 rawTag
    (rawLength, rawStart) = B.splitAt 4 rest
    length = rawToInt rawLength
    start = rawToInt rawStart

getFieldMetadata :: [MarcDirectoryEntryRaw] -> [FieldMetadata]
getFieldMetadata = map makeFieldMetadata

type FieldText = T.Text

-- Retrieve a field based on FieldMetadata
getTextField :: MarcRecordRaw -> FieldMetadata -> FieldText
getTextField record metadata = E.decodeUtf8 bytes
  where
    baseAtEntry = B.drop (fieldStart metadata) (getBaseRecord record)
    bytes = B.take (fieldLength metadata) baseAtEntry

fieldDelimiter :: Char
fieldDelimiter = toEnum 31

titleTag :: T.Text
titleTag = "245"

titleSubfield :: Char
titleSubfield = 'a'

authorTag :: T.Text
authorTag = "100"

authorSubfield :: Char
authorSubfield = 'a'

lookupFieldMetadata :: T.Text -> MarcRecordRaw -> Maybe FieldMetadata
lookupFieldMetadata aTag record =
  if null results
    then Nothing
    else Just (head results)
  where
    metadata = (getFieldMetadata . splitDirectory . getDirectory) record
    results = filter ((== aTag) . tag) metadata

lookupSubfield :: Maybe FieldMetadata -> Char -> MarcRecordRaw -> Maybe T.Text
lookupSubfield Nothing _ _ = Nothing
lookupSubfield (Just metadata) subfield record =
  if null results
    then Nothing
    else Just ((T.drop 1 . head) results)
  where
    rawField = getTextField record metadata
    subfields = T.split (== fieldDelimiter) rawField
    results = filter ((== subfield) . T.head) subfields

lookupValue :: T.Text -> Char -> MarcRecordRaw -> Maybe T.Text
lookupValue aTag subfield record = lookupSubfield entryMetadata subfield record
  where
    entryMetadata = lookupFieldMetadata aTag record

lookupAuthor :: MarcRecordRaw -> Maybe Author
lookupAuthor = lookupValue authorTag authorSubfield

lookupTitle :: MarcRecordRaw -> Maybe Title
lookupTitle = lookupValue titleTag titleSubfield

marcToPairs :: B.ByteString -> [(Maybe Title, Maybe Author)]
marcToPairs marcStream = zip titles authors
  where
    records = allRecords marcStream
    titles = map lookupTitle records
    authors = map lookupAuthor records

pairsToBooks :: [(Maybe Title, Maybe Author)] -> [Book]
pairsToBooks pairs =
  map
    (\(title, author) -> Book {title = fromJust title, author = fromJust author})
    justPairs
  where
    justPairs = filter (\(title, author) -> isJust title && isJust author) pairs

processRecords :: Int -> B.ByteString -> Html
processRecords n = booksToHtml . pairsToBooks . take n . marcToPairs

-- You can download the MARC data here: (ohsu_ncnm_wscc_bibs.mrc)
-- https://archive.org/download/marc_oregon_summit_records/catalog_files/
--
-- This project shows that despite doing a fairly intensive IO task, the
-- final "main" is remarkabley minimal.
main :: IO ()
main = do
  args <- getArgs
  marcData <- B.readFile (head args)
  -- The generated HTML file won't have 500 entries because some entries
  -- in the original file have empty authors or titles.
  let processed = processRecords 500 marcData
  TIO.putStrLn processed

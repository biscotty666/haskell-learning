module Main (main) where

import Data.Aeson
import Data.Text as T
import Data.ByteString.Lazy as B
import Data.ByteString.Lazy.Char8 as BC
import GHC.Generics
import Control.Monad

data Book = Book
            { title :: T.Text
            , author :: T.Text
            , year :: Int
            } deriving (Show, Generic)

instance FromJSON Book
instance ToJSON Book

myBook :: Book
myBook = Book { author = "Will Kurt"
              , title = "Learn Haskell"
              , year = 2017 }

myBookJSON :: BC.ByteString
myBookJSON = encode myBook

rawJSON :: BC.ByteString
rawJSON = "{\"author\":\"Emil Ciroan\",\"title\":\"A Short History of Decay\",\"year\":1949}"

bookFromJSON :: Maybe Book
bookFromJSON = decode rawJSON

data ErrorMessage = ErrorMessage
                    { message :: T.Text
                    , error :: Int
                    } deriving Show

instance FromJSON ErrorMessage where
    parseJSON (Object v) =
      ErrorMessage <$> v .: "message"
                   <*> v .: "error"

exampleMessage :: Maybe T.Text
exampleMessage = Just "Opps"

exampleError :: Maybe Int
exampleError = Just 123

-- ghci> ErrorMessage <$> exampleMessage <*> exampleError
-- Just (ErrorMessage {message = "Opps", error = 123})
--
sampleError :: BC.ByteString
sampleError = "{\"message\":\"oops!\",\"error\": 123}"

sampleErrorMessage :: Maybe ErrorMessage
sampleErrorMessage = decode sampleError

instance ToJSON ErrorMessage where
  toJSON (ErrorMessage message errorCode) =
    object [ "message" .= message
            , "error" .= errorCode
           ]

anErrorMessage :: ErrorMessage
anErrorMessage = ErrorMessage "Everything is Okay" 0

-- ghci> encode anErrorMessage
-- "{\"error\":0,\"message\":\"Everything is Okay\"}"
data NOAAResult = NOAAResult
                  { uid :: T.Text
                  , mindate :: T.Text
                  , maxdate :: T.Text
                  , name :: T.Text
                  , datacoverage :: Int
                  , resultId :: T.Text
                  } deriving Show

instance FromJSON NOAAResult where
  parseJSON (Object v) =
    NOAAResult <$> v .: "uid"
               <*> v .: "mindate"
               <*> v .: "maxdate"
               <*> v .: "name"
               <*> v .: "datacoverage"
               <*> v .: "id"

data Resultset = Resultset
                 { offset :: Int
                 , count :: Int
                 , limit :: Int
                 } deriving (Show, Generic)

instance FromJSON Resultset

data Metadata = Metadata
                { resultset :: Resultset
                } deriving (Show, Generic)

instance FromJSON Metadata

data NOAAResponse = NOAAResponse
                    { metadata :: Metadata
                    , results :: [NOAAResult]
                    } deriving (Show,Generic)

instance FromJSON NOAAResponse

printResults :: Maybe [NOAAResult] -> IO ()
printResults Nothing = print "error loading data"
printResults (Just results) = do
  forM_ results (print . name)
    -- print dataName

main :: IO ()
main = do
    jsonData <- B.readFile "data.json"
    let noaaResponse = decode jsonData :: Maybe NOAAResponse
    let noaaResults = results <$> noaaResponse
    printResults noaaResults

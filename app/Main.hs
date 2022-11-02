{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, ToJSON)
import Data.Set (fromList)
import GHC.Generics
import Lib
import System.Random
import Web.Scotty

data MoveRequest = MoveRequest {field :: [String], usedWords :: [String]} deriving (Show, Generic)

data MoveResponse = MoveResponse {updatedField :: [String], word :: String, x :: Int, y :: Int, letter :: String} deriving (Show, Generic)

instance ToJSON MoveRequest

instance FromJSON MoveRequest

instance ToJSON MoveResponse

instance FromJSON MoveResponse

extractField :: MoveRequest -> [String]
extractField r@MoveRequest {field = answer} = answer

extractUsedWords :: MoveRequest -> [String]
extractUsedWords r@MoveRequest {usedWords = answer} = answer

--main = startGame

createNewField :: [String] -> IO Field
createNewField dictionary = do
  let initWords = wordsOfLength 5 dictionary
  initWordIndex <- randomRIO (0, length initWords) :: IO Int
  let initWord = initWords !! initWordIndex
  return (createField initWord)

main :: IO ()
main = do
  dictionary <- readDictionary
  let dictionarySet = fromList dictionary

  scotty 3000 $ do
    get "/api/field" $ do
      f <- liftIO (createNewField dictionary)
      json f
    post "/api/move-requests" $ do
      moveRequest <- jsonData :: ActionM MoveRequest
      let field = extractField moveRequest
      let usedWords = extractUsedWords moveRequest
      (updatedField, word, ((x, y), letter)) <- liftIO (makeMove dictionarySet usedWords field)
      json (MoveResponse updatedField word x y [letter])

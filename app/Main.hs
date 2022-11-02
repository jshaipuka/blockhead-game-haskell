{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics
import Lib
import System.Random
import Web.Scotty

data GameDto = GameDto {field :: [String], move :: String} deriving (Show, Generic)

instance ToJSON GameDto

instance FromJSON GameDto

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

  scotty 3000 $ do
    get "/api/field" $ do
      f <- liftIO (createNewField dictionary)
      json f
    post "/api/games" $ do
      game <- jsonData :: ActionM GameDto
      json game

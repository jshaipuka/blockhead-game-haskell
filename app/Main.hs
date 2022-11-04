{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, ToJSON)
import Data.Set (fromList)
import GHC.Generics
import Lib
import Web.Scotty
import Network.Wai.Middleware.Cors

data MoveRequest = MoveRequest {field :: [String], usedWords :: [String]} deriving (Show, Generic)

data MoveResponse = MoveResponse {updatedField :: [String], path :: [(Int, Int)], word :: String, cell :: (Int, Int), letter :: Char} deriving (Show, Generic)

instance ToJSON MoveRequest

instance FromJSON MoveRequest

instance ToJSON MoveResponse

instance FromJSON MoveResponse

main :: IO ()
main = do
  dictionary <- readDictionary
  let dictionarySet = fromList dictionary

  scotty 3001 $ do
    get "/api/field" $ do
      field <- liftIO (createNewField dictionary)
      addHeader "Access-Control-Allow-Origin" "*"
      json field
    options "/api/move-requests" $ do
      addHeader "Access-Control-Allow-Origin" "*"
      addHeader "Access-Control-Allow-Headers" "*"
    post "/api/move-requests" $ do
      MoveRequest {field, usedWords} <- jsonData :: ActionM MoveRequest
      (updatedField, path, word, (cell, letter)) <- liftIO (makeMove dictionarySet usedWords field)
      addHeader "Access-Control-Allow-Origin" "*"
      addHeader "Access-Control-Allow-Headers" "*"
      json (MoveResponse updatedField path word cell letter)

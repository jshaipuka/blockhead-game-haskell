{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, ToJSON)
import Data.Set (fromList)
import GHC.Generics
import Lib
import Network.Wai (Middleware)
import Network.Wai.Middleware.Cors (CorsResourcePolicy, cors, corsMethods, corsRequestHeaders, simpleCorsResourcePolicy)
import Web.Scotty

data MoveRequest = MoveRequest {field :: [String], usedWords :: [String]} deriving (Show, Generic)

data MoveResponse = MoveResponse {updatedField :: [String], path :: [(Int, Int)], word :: String, cell :: (Int, Int), letter :: Char} deriving (Show, Generic)

instance ToJSON MoveRequest

instance FromJSON MoveRequest

instance ToJSON MoveResponse

instance FromJSON MoveResponse

allowCors :: Middleware
allowCors = cors (const $ Just appCorsResourcePolicy)

appCorsResourcePolicy :: CorsResourcePolicy
appCorsResourcePolicy =
  simpleCorsResourcePolicy
    { corsMethods = ["OPTIONS", "GET", "PUT", "POST"],
      corsRequestHeaders = ["Authorization", "Content-Type"]
    }

main :: IO ()
main = do
  dictionary <- readDictionary
  let dictionarySet = fromList dictionary

  scotty 8080 $ do
    middleware allowCors
    get "/api/field" $ do
      field <- liftIO (createNewField dictionary)
      json field
    post "/api/move-requests" $ do
      MoveRequest {field, usedWords} <- jsonData :: ActionM MoveRequest
      (updatedField, path, word, (cell, letter)) <- liftIO (makeMove dictionarySet usedWords field)
      json (MoveResponse updatedField path word cell letter)

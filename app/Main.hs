{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, ToJSON)
import Data.HashSet (fromList)
import GHC.Generics
import Lib
import Network.Wai (Middleware)
import Network.Wai.Middleware.Cors (CorsResourcePolicy, cors, corsMethods, corsRequestHeaders, simpleCorsResourcePolicy, simpleHeaders, simpleMethods)
import Web.Scotty

data MoveRequest = MoveRequest {field :: [String], usedWords :: [String]} deriving (Show, Generic)

data MoveResponse = MoveResponse {success :: Bool, updatedField :: [String], path :: [(Int, Int)], word :: String, cell :: (Int, Int), letter :: Char} deriving (Show, Generic)

instance ToJSON MoveRequest

instance FromJSON MoveRequest

instance ToJSON MoveResponse

instance FromJSON MoveResponse

corsWithPreflightResourcePolicy :: CorsResourcePolicy
corsWithPreflightResourcePolicy = simpleCorsResourcePolicy {corsMethods = "OPTIONS" : simpleMethods, corsRequestHeaders = simpleHeaders}

allowCorsWithPreflight :: Middleware
allowCorsWithPreflight = cors (const $ Just corsWithPreflightResourcePolicy)

main :: IO ()
main = do
  dictionary <- readDictionary
  let dictionarySet = fromList dictionary

  scotty 8080 $ do
    middleware allowCorsWithPreflight
    get "/api/field/:size" $ do
      size <- param "size"
      field <- liftIO $ createNewField dictionary size
      json field
    post "/api/move-requests" $ do
      MoveRequest {field, usedWords} <- jsonData :: ActionM MoveRequest
      (success, updatedField, path, word, (cell, letter)) <- liftIO $ makeMove dictionarySet usedWords field
      json $ MoveResponse success updatedField path word cell letter

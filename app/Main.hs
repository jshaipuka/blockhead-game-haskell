{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as A
import qualified Data.HashSet as S
import GHC.Generics (Generic)
import qualified Lib as L
import Network.Wai (Middleware)
import Network.Wai.Middleware.Cors (CorsResourcePolicy, cors, corsMethods, corsRequestHeaders, simpleCorsResourcePolicy, simpleHeaders, simpleMethods)
import Web.Scotty (ActionM, get, json, jsonData, middleware, param, post, scotty)

data DifficultyDto = Easy | Medium | Hard deriving (Show, Generic)

difficultyFromDto :: DifficultyDto -> L.Difficulty
difficultyFromDto Easy = L.Easy
difficultyFromDto Medium = L.Medium
difficultyFromDto Hard = L.Hard

data MoveRequest = MoveRequest {field :: [String], usedWords :: [String], difficulty :: DifficultyDto} deriving (Show, Generic)

data MoveResponse = MoveResponse {success :: Bool, updatedField :: [String], path :: [(Int, Int)], word :: String, cell :: (Int, Int), letter :: Char} deriving (Show, Generic)

instance A.ToJSON DifficultyDto

instance A.FromJSON DifficultyDto

instance A.ToJSON MoveRequest

instance A.FromJSON MoveRequest

instance A.ToJSON MoveResponse

instance A.FromJSON MoveResponse

corsWithPreflightResourcePolicy :: CorsResourcePolicy
corsWithPreflightResourcePolicy = simpleCorsResourcePolicy {corsMethods = "OPTIONS" : simpleMethods, corsRequestHeaders = simpleHeaders}

allowCorsWithPreflight :: Middleware
allowCorsWithPreflight = cors (const $ Just corsWithPreflightResourcePolicy)

main :: IO ()
main = do
  dictionary <- L.readDictionary
  let dictionarySet = S.fromList dictionary
  let prefixSet = L.toPrefixDictionarySet dictionary

  scotty 8080 $ do
    middleware allowCorsWithPreflight
    get "/api/field/:size" $ do
      size <- param "size"
      field <- liftIO $ L.createNewField dictionary size
      json field
    post "/api/move-requests" $ do
      MoveRequest {field, usedWords, difficulty} <- jsonData :: ActionM MoveRequest
      (success, updatedField, path, word, (cell, letter)) <- liftIO $ L.makeMove prefixSet dictionarySet (difficultyFromDto difficulty) usedWords field
      json $ MoveResponse success updatedField path word cell letter

module Dictionary.Internal where

import qualified Data.HashSet as S
import Data.List.Split (splitOn)
import Paths_blockhead_game (getDataFileName)

type Dictionary = S.HashSet String

type PrefixDictionary = S.HashSet String

readDictionary :: IO Dictionary
readDictionary = do
  dictionaryFileName <- getDataFileName "dictionary.txt"
  contents <- readFile dictionaryFileName
  return $ S.fromList $ splitOn "\n" contents

toPrefixDictionary :: Dictionary -> PrefixDictionary
toPrefixDictionary dictionary = S.fromList $ concatMap prefixes dictionary
  where
    prefixes :: String -> [String]
    prefixes word = map (`take` word) [1 .. length word]

{-# LANGUAGE TupleSections #-}

module Lib (Field, createEmptyField, readDictionary, wordsOfLength, createNewField, makeMove, toPrefixDictionarySet, Difficulty (Easy, Medium, Hard)) where

import qualified Data.HashSet as S
import Data.Hashable (Hashable)
import Data.List (sortBy)
import Data.List.Split (splitOn)
import Paths_blockhead_game (getDataFileName)
import System.Random (randomRIO)

type Field = [[Char]]

type Cell = (Int, Int)

type Path = [Cell]

type WordPath = (String, Path)

type Move = (Cell, Char)

data Difficulty = Easy | Medium | Hard

-- | The bigger the value the easier to play.
wordPickRange :: Difficulty -> Int
wordPickRange Easy = 30
wordPickRange Medium = 15
wordPickRange Hard = 0

createNewField :: [String] -> Int -> IO Field
createNewField dictionary size = do
  let initWords = wordsOfLength size dictionary
  initWordIndex <- randomRIO (0, length initWords - 1) :: IO Int
  let initWord = initWords !! initWordIndex
  return (createField size initWord)

createField :: Int -> String -> Field
createField size = replaceRow (createEmptyField size) $ size `div` 2

createEmptyField :: Int -> Field
createEmptyField size = replicate size $ replicate size '.'

replaceChar :: Field -> Cell -> Char -> Field
replaceChar field (x, y) letter = replaceRow field x $ replaceChar' (field !! x) y letter

replaceRow :: Field -> Int -> [Char] -> Field
replaceRow field x newRow = take x field ++ [newRow] ++ drop (x + 1) field

replaceChar' :: [Char] -> Int -> Char -> [Char]
replaceChar' field x letter = take x field ++ [letter] ++ drop (x + 1) field

readDictionary :: IO [String]
readDictionary = do
  dictionaryFileName <- getDataFileName "dictionary.txt"
  contents <- readFile dictionaryFileName
  return (splitOn "\n" contents)

toPrefixDictionarySet :: [String] -> S.HashSet String
toPrefixDictionarySet dictionary = S.fromList $ concatMap prefixes dictionary

prefixes :: String -> [String]
prefixes w = map (`take` w) [1 .. length w]

wordsOfLength :: Int -> [String] -> [String]
wordsOfLength n = filter $ \w -> length w == n

getAvailableCells :: Field -> [Cell]
getAvailableCells field = filterCells field $ allCells field

allCells :: Field -> [Cell]
allCells field = concatMap (\i -> map (i,) [0 .. length (field !! i) - 1]) [0 .. length field - 1]

cellsWithLetters :: Field -> [Cell]
cellsWithLetters field = filter (hasLetter field) $ allCells field

filterCells :: Field -> [Cell] -> [Cell]
filterCells field = filter $ \candidate -> isEmpty field candidate && hasNeighboursWithLetter field candidate

hasNeighboursWithLetter :: Field -> Cell -> Bool
hasNeighboursWithLetter field cell = any (hasLetter field) $ field `neighboursOf` cell

neighboursOf :: Field -> Cell -> [Cell]
neighboursOf field (x, y) = filter (\(a, b) -> 0 <= a && a < length field && 0 <= b && b < length (field !! a)) neighbours
  where
    neighbours = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

isEmpty :: Field -> Cell -> Bool
isEmpty field cell = field `charAt` cell == '.'

hasLetter :: Field -> Cell -> Bool
hasLetter field cell = not (isEmpty field cell)

reachableCells :: Field -> Cell -> S.HashSet Cell -> [Cell]
reachableCells field cell visited =
  filter isNotVisitedLetter $ field `neighboursOf` cell
  where
    isNotVisitedLetter :: Cell -> Bool
    isNotVisitedLetter c = not (c `S.member` visited) && hasLetter field c

charAt :: Field -> Cell -> Char
charAt field (x, y) = (field !! x) !! y

paths :: S.HashSet String -> Field -> Cell -> [WordPath]
paths prefixSet field start = paths' prefixSet field start (S.singleton start) ([field `charAt` start], [start])

paths' :: S.HashSet String -> Field -> Cell -> S.HashSet Cell -> WordPath -> [WordPath]
paths' prefixSet field current visited wordPathSoFar@(word, _)
  | word `S.member` prefixSet = wordPathSoFar : concatMap (\cell -> paths' prefixSet field cell (cell `S.insert` visited) (appendCell field wordPathSoFar cell)) (reachableCells field current visited)
  | otherwise = []

appendCell :: Field -> WordPath -> Cell -> WordPath
appendCell field (word, path) cell = (word ++ [field `charAt` cell], path ++ [cell])

alphabet :: String
alphabet = ['А' .. 'Е'] ++ ['Ё'] ++ ['Ж' .. 'Я']

getAvailableMoves :: Field -> [Move]
getAvailableMoves field = concatMap (\cell -> map (cell,) alphabet) $ getAvailableCells field

getWords :: S.HashSet String -> Field -> [(Path, String, Move)]
getWords prefixSet field = getWords' prefixSet field (getAvailableMoves field)

getWords' :: S.HashSet String -> Field -> [Move] -> [(Path, String, Move)]
getWords' prefixSet field moves = mkUniq $ concatMap (getWords'' prefixSet field) moves

getWords'' :: S.HashSet String -> Field -> Move -> [(Path, String, Move)]
getWords'' prefixSet field (cell, letter) = map (\(word, path) -> (path, word, (cell, letter))) (getWords''' prefixSet fieldAfterMove cell)
  where
    fieldAfterMove = replaceChar field cell letter

getWords''' :: S.HashSet String -> Field -> Cell -> [WordPath]
getWords''' prefixSet field updatedCell = filter (\(_, path) -> updatedCell `elem` path) $ concatMap (paths prefixSet field) $ cellsWithLetters field

mkUniq :: (Eq a, Hashable a) => [a] -> [a]
mkUniq = S.toList . S.fromList

makeMove :: S.HashSet String -> S.HashSet String -> Difficulty -> [String] -> Field -> IO (Bool, Field, Path, String, Move)
makeMove prefixSet dictionarySet difficulty usedWords field = do
  let foundWords = filter (\(_, w, _) -> w `S.member` dictionarySet && (w `notElem` usedWords)) $ getWords prefixSet field
  if null foundWords
    then do
      return (False, field, [], "", ((0, 0), ' '))
    else do
      let longestWordsFirst = sortBy (\(_, a, _) (_, b, _) -> compare (length b) (length a)) foundWords
      wordIndex <- randomRIO (0, min (wordPickRange difficulty) $ length longestWordsFirst - 1) :: IO Int
      let oneOfLongestWord = longestWordsFirst !! wordIndex
      let (path, word, (cell, letter)) = oneOfLongestWord
      let updatedField = replaceChar field cell letter
      return (True, updatedField, path, word, (cell, letter))

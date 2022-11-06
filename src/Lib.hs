{-# LANGUAGE TupleSections #-}

module Lib (Field, createEmptyField, readDictionary, wordsOfLength, createNewField, makeMove, toPrefixDictionarySet) where

import Data.HashSet (HashSet, fromList, insert, member, singleton, toList)
import Data.Hashable (Hashable)
import Data.List (sortBy)
import Data.List.Split
import Paths_blockhead_game (getDataFileName)
import System.Random

type Field = [[Char]]

type Cell = (Int, Int)

type Path = [Cell]

type WordPath = (String, Path)

type Move = (Cell, Char)

-- | Should be in range from 0 to 9 (inclusively). The bigger the value the more difficult to play.
difficulty :: Int
difficulty = 3

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

toPrefixDictionarySet :: [String] -> HashSet String
toPrefixDictionarySet dictionary = fromList $ concatMap prefixes dictionary

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
hasNeighboursWithLetter field cell = any (hasLetter field) $ getNeighbours field cell

getNeighbours :: Field -> Cell -> [Cell]
getNeighbours field (x, y) = filter (\(a, b) -> 0 <= a && a < length field && 0 <= b && b < length (field !! a)) neighbours
  where
    neighbours = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

isEmpty :: Field -> Cell -> Bool
isEmpty field (a, b) = (field !! a) !! b == '.'

hasLetter :: Field -> Cell -> Bool
hasLetter field cell = not (isEmpty field cell)

reachable :: Field -> Cell -> HashSet Cell -> [Cell]
reachable field (x, y) visited =
  filter (\(a, b) -> not ((a, b) `member` visited) && hasLetter field (a, b)) $ getNeighbours field (x, y)

paths :: HashSet String -> Field -> Cell -> [WordPath]
paths prefixSet field start = paths' prefixSet field start (singleton start) (pathToWord field [start], [start])

paths' :: HashSet String -> Field -> Cell -> HashSet Cell -> WordPath -> [WordPath]
paths' prefixSet field start visited pathSoFar =
  pathSoFar : concatMap (\cell -> paths' prefixSet field cell (cell `insert` visited) (pathToWord field (snd pathSoFar ++ [cell]), snd pathSoFar ++ [cell])) (filter (\cell -> pathToWord field (snd pathSoFar ++ [cell]) `member` prefixSet) $ reachable field start visited)

alphabet :: String
alphabet = ['А' .. 'Е'] ++ ['Ё'] ++ ['Ж' .. 'Я']

getAvailableMoves :: Field -> [Move]
getAvailableMoves field = concatMap (\cell -> map (cell,) alphabet) $ getAvailableCells field

getWords :: HashSet String -> Field -> [(Path, String, Move)]
getWords prefixSet field = getWords' prefixSet field (getAvailableMoves field)

getWords' :: HashSet String -> Field -> [Move] -> [(Path, String, Move)]
getWords' prefixSet field moves = mkUniq $ concatMap (getWords'' prefixSet field) moves

getWords'' :: HashSet String -> Field -> Move -> [(Path, String, Move)]
getWords'' prefixSet field (cell, letter) = map (\(word, path) -> (path, word, (cell, letter))) (getWords''' prefixSet fieldAfterMove cell)
  where
    fieldAfterMove = replaceChar field cell letter

getWords''' :: HashSet String -> Field -> Cell -> [WordPath]
getWords''' prefixSet field updatedCell = filter (\(_, path) -> updatedCell `elem` path) $ concatMap (paths prefixSet field) $ cellsWithLetters field

pathToWord :: Field -> Path -> String
pathToWord field = map $ \(x, y) -> (field !! x) !! y

mkUniq :: (Eq a, Hashable a) => [a] -> [a]
mkUniq = toList . fromList

makeMove :: HashSet String -> HashSet String -> [String] -> Field -> IO (Bool, Field, Path, String, Move)
makeMove prefixSet dictionarySet usedWords field = do
  let foundWords = filter (\(_, w, _) -> w `member` dictionarySet && (w `notElem` usedWords)) $ getWords prefixSet field
  if null foundWords
    then do
      return (False, field, [], "", ((0, 0), ' '))
    else do
      let longestWordsFirst = sortBy (\(_, a, _) (_, b, _) -> compare (length b) (length a)) foundWords
      wordIndex <- randomRIO (0, min (9 - difficulty) $ length longestWordsFirst - 1) :: IO Int
      let oneOfLongestWord = longestWordsFirst !! wordIndex
      let (path, word, (cell, letter)) = oneOfLongestWord
      let updatedField = replaceChar field cell letter
      return (True, updatedField, path, word, (cell, letter))

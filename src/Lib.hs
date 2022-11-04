{-# LANGUAGE TupleSections #-}

module Lib (Field, createEmptyField, readDictionary, wordsOfLength, createNewField, makeMove) where

import Data.List (sortBy)
import Data.List.Split
import Data.Set (Set, fromList, insert, member, singleton, toList)
import Paths_blockhead_game (getDataFileName)
import System.Random

type Field = [[Char]]

type Cell = (Int, Int)

type Path = [Cell]

type Move = (Cell, Char)

-- | Should be in range from 0 to 9 (inclusively). The bigger the value the more difficult to play.
difficulty :: Int
difficulty = 4

longestWordComputerCanFind :: Int
longestWordComputerCanFind = 8

createNewField :: [String] -> IO Field
createNewField dictionary = do
  let initWords = wordsOfLength 5 dictionary
  initWordIndex <- randomRIO (0, length initWords - 1) :: IO Int
  let initWord = initWords !! initWordIndex
  return (createField initWord)

createField :: String -> Field
createField = replaceRow createEmptyField 2

createEmptyField :: Field
createEmptyField = replicate 5 (replicate 5 '.')

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

wordsOfLength :: Int -> [String] -> [String]
wordsOfLength n = filter $ \w -> length w == n

getAvailableCells :: Field -> [Cell]
getAvailableCells field = filterCells field (allCells field)

allCells :: Field -> [Cell]
allCells field = concatMap (\i -> map (i,) [0 .. length (field !! i) - 1]) [0 .. length field - 1]

cellsWithLetters :: Field -> [Cell]
cellsWithLetters field = filter (hasLetter field) (allCells field)

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

reachable :: Field -> Cell -> Set Cell -> [Cell]
reachable field (x, y) visited =
  filter (\(a, b) -> not ((a, b) `member` visited) && hasLetter field (a, b)) (getNeighbours field (x, y))

paths :: Field -> Cell -> [Path]
paths field start = paths' field start (singleton start) [start]

paths' :: Field -> Cell -> Set Cell -> Path -> [Path]
paths' field start visited pathSoFar =
  pathSoFar : if length pathSoFar < longestWordComputerCanFind then concatMap (\n -> paths' field n (insert n visited) (pathSoFar ++ [n])) $ reachable field start visited else []

alphabet :: [Char]
alphabet = ['А' .. 'Е'] ++ ['Ё'] ++ ['Ж' .. 'Я']

getAvailableMoves :: Field -> [Move]
getAvailableMoves field = concatMap (\cell -> map (cell,) alphabet) (getAvailableCells field)

getWords :: Field -> [(Path, String, Move)]
getWords field = getWords' field (getAvailableMoves field)

getWords' :: Field -> [Move] -> [(Path, String, Move)]
getWords' field moves = mkUniq $ concatMap (getWords'' field) moves

getWords'' :: Field -> Move -> [(Path, String, Move)]
getWords'' field (cell, letter) = map (\path -> (path, pathToWord fieldAfterMove path, (cell, letter))) (getWords''' fieldAfterMove cell)
  where
    fieldAfterMove = replaceChar field cell letter

getWords''' :: Field -> Cell -> [Path]
getWords''' field updatedCell = filter (elem updatedCell) $ concatMap (paths field) (cellsWithLetters field)

pathToWord :: Field -> Path -> String
pathToWord field = map $ \(x, y) -> (field !! x) !! y

mkUniq :: Ord a => [a] -> [a]
mkUniq = toList . fromList

makeMove :: Set String -> [String] -> Field -> IO (Field, Path, String, Move)
makeMove dictionary usedWords field = do
  let allWords = getWords field
  let realWords = filter (\(_, w, _) -> w `member` dictionary && (w `notElem` usedWords)) allWords
  let longestWordsFirst = sortBy (\(_, a, _) (_, b, _) -> compare (length b) (length a)) realWords
  wordIndex <- randomRIO (0, min (9 - difficulty) (length longestWordsFirst - 1)) :: IO Int
  let oneOfLongestWord = longestWordsFirst !! wordIndex
  let (path, word, (cell, letter)) = oneOfLongestWord
  let updatedField = replaceChar field cell letter
  return (updatedField, path, word, (cell, letter))

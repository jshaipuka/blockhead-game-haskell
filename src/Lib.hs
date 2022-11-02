{-# LANGUAGE TupleSections #-}

module Lib (Field, createEmptyField, readDictionary, startGame, wordsOfLength, createField) where

import Data.List (intercalate, sortBy)
import Data.List.Split
import Data.Set (Set, fromList, insert, member, singleton, toList)
import Paths_blockhead_game (getDataFileName)
import System.Random

type Field = [[Char]]

type Cell = (Int, Int)

type Move = (Cell, Char)

type Game = (Set String, Field, Bool, Set String, [String], [String])

longestWordComputerCanFind :: Int
longestWordComputerCanFind = 8

createField :: String -> Field
createField = replaceRow createEmptyField 2

createEmptyField :: Field
createEmptyField = replicate 5 (replicate 5 '.')

replaceChar :: Field -> Cell -> Char -> Field
replaceChar field (x, y) letter = replaceRow field x (replaceChar' (field !! x) y letter)

replaceRow :: Field -> Int -> [Char] -> Field
replaceRow field x newRow = take x field ++ [newRow] ++ drop (x + 1) field

replaceChar' :: [Char] -> Int -> Char -> [Char]
replaceChar' field x letter = take x field ++ [letter] ++ drop (x + 1) field

getUserMove :: IO (String, Move)
getUserMove = do
  putStrLn "Input coordinate x:"
  x <- getLine
  putStrLn "Input coordinate y:"
  y <- getLine
  putStrLn "Input letter:"
  letter <- getLine
  putStrLn "Type your word:"
  word <- getLine
  return (word, ((read x, read y), head letter))

isValidMove :: Field -> Set String -> (String, Move) -> Bool
isValidMove field foundWords (word, move) =
  all (`elem` alphabet) word && not (word `member` foundWords) && (move `elem` getAvailableMoves field)

readDictionary :: IO [String]
readDictionary = do
  dictionaryFileName <- getDataFileName "dictionary.txt"
  contents <- readFile dictionaryFileName
  return (splitOn "\n" contents)

wordsOfLength :: Int -> [String] -> [String]
wordsOfLength n = filter (\w -> length w == n)

getAvailableCells :: Field -> [Cell]
getAvailableCells field = filterCells field (allCells field)

allCells :: Field -> [Cell]
allCells field = concatMap (\i -> map (i,) [0 .. length (field !! i) - 1]) [0 .. length field - 1]

cellsWithLetters :: Field -> [Cell]
cellsWithLetters field = filter (hasLetter field) (allCells field)

filterCells :: Field -> [Cell] -> [Cell]
filterCells field = filter (\candidate -> isEmpty field candidate && hasNeighboursWithLetter field candidate)

hasNeighboursWithLetter :: Field -> Cell -> Bool
hasNeighboursWithLetter field cell = any (hasLetter field) (getNeighbours field cell)

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

paths :: Field -> Cell -> [[Cell]]
paths field start = paths' field start (singleton start) [start]

paths' :: Field -> Cell -> Set Cell -> [Cell] -> [[Cell]]
paths' field start visited pathSoFar =
  pathSoFar : if length pathSoFar < longestWordComputerCanFind then concatMap (\n -> paths' field n (insert n visited) (pathSoFar ++ [n])) (reachable field start visited) else []

alphabet :: [Char]
alphabet = ['А' .. 'Е'] ++ ['Ё'] ++ ['Ж' .. 'Я']

getAvailableMoves :: Field -> [Move]
getAvailableMoves field = concatMap (\cell -> map (cell,) alphabet) (getAvailableCells field)

getWords :: Field -> [Move] -> [(String, Move)]
getWords field moves = mkUniq (concatMap (getWords' field) moves)

getWords' :: Field -> Move -> [(String, Move)]
getWords' field (cell, letter) = map (\path -> (pathToWord fieldAfterMove path, (cell, letter))) (getWords'' fieldAfterMove cell)
  where
    fieldAfterMove = replaceChar field cell letter

getWords'' :: Field -> Cell -> [[Cell]]
getWords'' field updatedCell = filter (elem updatedCell) (concatMap (paths field) (cellsWithLetters field))

pathToWord :: Field -> [Cell] -> String
pathToWord field = map (\(x, y) -> (field !! x) !! y)

mkUniq :: Ord a => [a] -> [a]
mkUniq = toList . fromList

totalLettersIn :: [String] -> Int
totalLettersIn ws = sum (map length ws)

totalMoves :: Field -> Int
totalMoves field = (length field - 1) * length (head field)

gameLoop :: Game -> IO ()
gameLoop (_, field, _, foundWords, userWords, computerWords) | length foundWords - 1 == totalMoves field = do
  putStrLn (intercalate "\n" field)
  putStrLn ("Your score: " ++ show (totalLettersIn userWords))
  putStrLn ("My score: " ++ show (totalLettersIn computerWords))
gameLoop (dictionary, field, True, foundWords, userWords, computerWords) = do
  putStrLn "Your turn!"
  putStrLn (intercalate "\n" field)
  (word, (cell, letter)) <- getUserMove
  if isValidMove field foundWords (word, (cell, letter))
    then gameLoop (dictionary, replaceChar field cell letter, False, insert word foundWords, word : userWords, computerWords)
    else do
      putStrLn "Invalid input, try again"
      gameLoop (dictionary, field, True, foundWords, userWords, computerWords)
gameLoop (dictionary, field, False, foundWords, userWords, computerWords) = do
  putStrLn "I am thinking..."
  let allWords = getWords field (getAvailableMoves field)
  let realWords = filter (\(w, _) -> w `member` dictionary && not (w `member` foundWords)) allWords
  let sortedWords = sortBy (\(a, _) (b, _) -> compare (length b) (length a)) realWords
  wordIndex <- randomRIO (0, min 5 (length sortedWords)) :: IO Int
  let oneOfLongestWord = sortedWords !! wordIndex
  let (word, (cell, letter)) = oneOfLongestWord
  let updatedField = replaceChar field cell letter
  putStrLn ("I picked word " ++ word)
  gameLoop (dictionary, updatedField, True, insert word foundWords, userWords, word : computerWords)

startGame :: IO ()
startGame = do
  dictionary <- readDictionary
  let initWords = wordsOfLength 5 dictionary
  initWordIndex <- randomRIO (0, length initWords) :: IO Int
  let initWord = initWords !! initWordIndex
  let field = createField initWord
  gameLoop (fromList dictionary, field, True, singleton initWord, [], [])

module Lib
  ( startGame,
  )
where

import Data.List (intercalate)
import Data.List.Split
import Data.Set (fromList, toList)
import System.Random

type Field = [[Char]]

type Cell = (Int, Int)

type Move = (Cell, Char)

longestWordComputerCanFind :: Int
longestWordComputerCanFind = 6

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

makeUserMove :: IO (Int, Int, Char)
makeUserMove = do
  putStrLn "Input coordinate x:"
  x <- getLine
  putStrLn "Input coordinate y:"
  y <- getLine
  putStrLn "Input letter:"
  letter <- getLine
  return (read x, read y, head letter)

dictionary :: IO [String]
dictionary = do
  --  contents <- readFile "C:\\PROJECTS\\haskell\\blockhead-game\\src\\slova.txt"
  contents <- readFile "/Users/yaskovdev/dev/git_home/blockhead-game-haskell/src/slova.txt"
  return (splitOn "\n" contents)

wordsOfLength :: Int -> [String] -> [String]
wordsOfLength _ [] = []
wordsOfLength n (x : xs) = if length x == n then x : wordsOfLength n xs else wordsOfLength n xs

getAvailableCells :: Field -> [Cell]
getAvailableCells field = filterCells' field (allCells field)

allCells :: Field -> [Cell]
allCells field = concatMap (\i -> map (\j -> (i, j)) [0 .. length (field !! i) - 1]) [0 .. length field - 1]

cellsWithLetters :: Field -> [Cell]
cellsWithLetters field = filter (hasLetter field) (allCells field)

filterCells' :: Field -> [Cell] -> [Cell]
filterCells' _ [] = []
filterCells' field (candidate : candidates) =
  if isEmpty field candidate && hasNeighboursWithLetter field candidate then candidate : filterCells' field candidates else filterCells' field candidates

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

reachable :: Field -> Cell -> [Cell] -> [Cell]
reachable field (x, y) visited =
  filter (\(a, b) -> notElem (a, b) visited && hasLetter field (a, b)) (getNeighbours field (x, y))

paths :: Field -> Cell -> [[Cell]]
paths field start = paths' field start [start] [start]

paths' :: Field -> Cell -> [Cell] -> [Cell] -> [[Cell]]
paths' field start visited pathSoFar =
  pathSoFar : if length pathSoFar < longestWordComputerCanFind then concatMap (\n -> paths' field n (visited ++ [n]) (pathSoFar ++ [n])) (reachable field start visited) else []

alphabet :: [Char]
alphabet = ['А' .. 'Е'] ++ ['Ё'] ++ ['Ж' .. 'Я']

getAvailableMoves :: Field -> [Move]
getAvailableMoves field = concatMap (\cell -> map (\letter -> (cell, letter)) alphabet) (getAvailableCells field)

getWords :: Field -> [Move] -> [String]
getWords field moves = concatMap (getWords' field) moves

getWords' :: Field -> Move -> [String]
getWords' field (cell, letter) = map (pathToWord fieldAfterMove) (getWords'' fieldAfterMove cell)
  where
    fieldAfterMove = replaceChar field cell letter

getWords'' :: Field -> Cell -> [[Cell]]
getWords'' field updatedCell = filter (elem updatedCell) (concatMap (paths field) (cellsWithLetters field))

pathToWord :: Field -> [Cell] -> String
pathToWord field path = map (\(x, y) -> (field !! x) !! y) path

-- TODO: filter out duplicates as early as possible
mkUniq :: Ord a => [a] -> [a]
mkUniq = toList . fromList

startGame :: IO ()
startGame = do
  d <- dictionary
  let initWords = wordsOfLength 5 d
  initWordIndex <- randomRIO (0, length initWords) :: IO Int
  initWordIndex2 <- randomRIO (0, length initWords) :: IO Int
  initWordIndex3 <- randomRIO (0, length initWords) :: IO Int
  initWordIndex4 <- randomRIO (0, length initWords) :: IO Int
  initWordIndex5 <- randomRIO (0, length initWords) :: IO Int
  let field = replaceRow createEmptyField 2 (initWords !! initWordIndex)
  let field2 = replaceRow field 1 (initWords !! initWordIndex2)
  let field3 = replaceRow field2 3 (initWords !! initWordIndex3)
  let field4 = replaceRow field3 4 (initWords !! initWordIndex4)
  let field5 = replaceChar (replaceRow field4 0 (initWords !! initWordIndex5)) (0, 0) '.'
  putStrLn "-----"
  putStrLn (intercalate "\n" field5)
  putStrLn "-----"
  --  let l = maximum (map length d)
  --  print l
  let allWords = mkUniq (getWords field5 (getAvailableMoves field5))
  -- TODO: turn the dictionary into a set to speedup searching. Or is Haskell doing that under the hood already?
  putStrLn (intercalate "\n" (filter (`elem` d) allWords))

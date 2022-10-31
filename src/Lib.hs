module Lib
  ( startGame,
  )
where

import Data.List (intercalate)
import Data.List.Split
import System.Random

type Field = [[Char]]

type Cell = (Int, Int)

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

availableCells :: Field -> [Cell]
availableCells field = filterCells' field (cellsOf field)

cellsOf :: Field -> [Cell]
cellsOf field = concatMap (\i -> map (\j -> (i, j)) [0 .. length (field !! i) - 1]) [0 .. length field - 1]

filterCells' :: Field -> [Cell] -> [Cell]
filterCells' _ [] = []
filterCells' field (candidate : candidates) = 
  if isEmpty field candidate && hasNeighboursWithLetter field candidate then candidate : filterCells' field candidates else filterCells' field candidates

hasNeighboursWithLetter :: Field -> Cell -> Bool
hasNeighboursWithLetter field cell = any (hasLetter field) (getNeighbours field cell)

getNeighbours :: Field -> Cell -> [Cell]
getNeighbours field (x, y) = filter (\(a, b) -> 0 <= a && a < length field && 0 <= b && b < length (field !! a)) neighbours
  where neighbours = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

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
  pathSoFar : concatMap (\n -> paths' field n (visited ++ [n]) (pathSoFar ++ [n])) (reachable field start visited)

startGame :: IO ()
startGame = do
  d <- dictionary
  let initWords = wordsOfLength 5 d
  initWordIndex <- randomRIO (0, length initWords) :: IO Int
  let field = createField (initWords !! initWordIndex)
  putStrLn (intercalate "\n" field)
  print (availableCells field)
  print (paths field (2, 1))

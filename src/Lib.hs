module Lib
  ( startGame,
  )
where

import Data.List (intercalate)
import Data.List.Split
import System.Random

createField :: String -> [[Char]]
createField = replaceRow createEmptyField 2

createEmptyField :: [[Char]]
createEmptyField = replicate 5 (replicate 5 '.')

replaceChar :: [[Char]] -> (Int, Int) -> Char -> [[Char]]
replaceChar field (x, y) letter = replaceRow field x (replaceChar' (field !! x) y letter)

replaceRow :: [[Char]] -> Int -> [Char] -> [[Char]]
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

-- TODO: Syabr implements
availableCells :: [[Char]] -> [(Int, Int)]
availableCells _ = []

getNeighbours :: (Int, Int) -> [(Int, Int)]
getNeighbours (x, y) = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

hasLetter :: [[Char]] -> (Int, Int) -> Bool
hasLetter field (a, b) = (field !! a) !! b /= '.'

reachable :: [[Char]] -> (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
reachable field (x, y) visited =
  filter (\(a, b) -> 0 <= a && a < length field && 0 <= b && b < length field && notElem (a, b) visited && hasLetter field (a, b)) (getNeighbours (x, y))

paths :: [[Char]] -> (Int, Int) -> [[(Int, Int)]]
paths field start = paths' field start [start] [start]

paths' :: [[Char]] -> (Int, Int) -> [(Int, Int)] -> [(Int, Int)] -> [[(Int, Int)]]
paths' field start visited pathSoFar =
  if null neighbours then [pathSoFar] else concatMap (\n -> paths' field n (visited ++ [n]) (pathSoFar ++ [n])) neighbours
  where
    neighbours = reachable field start visited

startGame :: IO ()
startGame = do
  d <- dictionary
  let initWords = wordsOfLength 5 d
  initWordIndex <- randomRIO (0, length initWords) :: IO Int
  let field = createField (initWords !! initWordIndex)
  putStrLn (intercalate "\n" field)
  print (paths field (2, 1))

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

neighbours :: (Int, Int) -> [(Int, Int)]
neighbours (x, y) = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

hasLetter :: [[Char]] -> (Int, Int) -> Bool
hasLetter field (a, b) = (field !! a) !! b /= '.'

reachable :: [[Char]] -> (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
reachable field (x, y) visited =
  filter (\(a, b) -> 0 <= a && a < length field && 0 <= b && b < length field && notElem (a, b) visited && hasLetter field (a, b)) (neighbours (x, y))
--
--bfs :: [[Char]] -> [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)] -> [[(Int, Int)]] -> [[(Int, Int)]]
--bfs _ [] _ pathSoFar answer = pathSoFar : answer
--bfs field (x : xs) visited pathSoFar answer = bfs field (xs ++ reachable field x visited) (visited ++ reachable field x visited) pathSoFar (answer ++ reachable field x visited)

startGame :: IO ()
startGame = do
  d <- dictionary
  let initWords = wordsOfLength 5 d
  initWordIndex <- randomRIO (0, length initWords) :: IO Int
  let field = createField (initWords !! initWordIndex)
  putStrLn (intercalate "\n" field)
--  print (bfs field [(2, 0)] [(2, 0)] [(2, 0)])

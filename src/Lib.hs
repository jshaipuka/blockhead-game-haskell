module Lib
  ( startGame,
  )
where

import Data.List (intercalate)
import Data.List.Split
import System.Random

--  (x, y, letter) <- makeUserMove
--  print (makeMoveTwoDim createField (x, y) letter)
--  hSetEncoding stdout utf8

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
  contents <- readFile "C:\\PROJECTS\\haskell\\blockhead-game\\src\\slova.txt"
  return (splitOn "\n" contents)

wordsOfLength :: Int -> [String] -> [String]
wordsOfLength _ [] = []
wordsOfLength n (x : xs) = if length x == n then x : wordsOfLength n xs else wordsOfLength n xs

startGame :: IO ()
startGame = do
  d <- dictionary
  let initWords = wordsOfLength 5 d
  target <- randomRIO (0, length initWords) :: IO Int
  let field = createField (initWords !! target)
  putStrLn (intercalate "\n" field)

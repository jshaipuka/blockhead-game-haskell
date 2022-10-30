module Lib
  ( someFunc,
  )
where

import System.IO
import Text.Printf

someFunc :: IO ()
someFunc = do
  hSetEncoding stdout utf8
  printf "%s" ("And немного русского" :: String)
--  readDictionary
--  (x, y, letter) <- makeUserMove
--  print (makeMoveTwoDim createField x y letter)

createField :: [[Char]]
createField =
  [ [' ', ' ', ' ', ' ', ' '],
    [' ', ' ', ' ', ' ', ' '],
    [' ', ' ', ' ', ' ', ' '],
    [' ', ' ', ' ', ' ', ' '],
    [' ', ' ', ' ', ' ', ' ']
  ]

makeMoveOneDim :: [Char] -> Int -> Char -> [Char]
makeMoveOneDim field x letter = take x field ++ [letter] ++ drop (x + 1) field

makeMoveTwoDim :: [[Char]] -> Int -> Int -> Char -> [[Char]]
makeMoveTwoDim field x y letter = take x field ++ [makeMoveOneDim (field !! x) y letter] ++ drop (x + 1) field

makeUserMove :: IO (Int, Int, Char)
makeUserMove = do
  putStrLn "Input coordinate x:"
  x <- getLine
  putStrLn "Input coordinate y:"
  y <- getLine
  putStrLn "Input letter:"
  letter <- getLine
  return (read x, read y, head letter)

readDictionary = do
  contents <- readFile "C:\\PROJECTS\\haskell\\blockhead-game\\src\\slova.txt"
  putStrLn contents
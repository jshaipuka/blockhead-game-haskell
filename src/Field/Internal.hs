{-# LANGUAGE TupleSections #-}

module Field.Internal where

type Cell = (Int, Int)

type Field = [String]

createField :: Int -> String -> Field
createField size = replaceRow (createEmptyField size) $ size `div` 2

createEmptyField :: Int -> Field
createEmptyField size = replicate size $ replicate size '.'

letterAt :: Field -> Cell -> Char
letterAt field (x, y) = (field !! x) !! y

replaceRow :: Field -> Int -> String -> Field
replaceRow field index newRow = take index field ++ [newRow] ++ drop (index + 1) field

neighboursOf :: Field -> Cell -> [Cell]
neighboursOf field (x, y) = filter (\(a, b) -> 0 <= a && a < length field && 0 <= b && b < length (field !! a)) neighbours
  where
    neighbours = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

replaceLetter :: Field -> Cell -> Char -> Field
replaceLetter field (x, y) letter = replaceRow field x $ replaceLetterInRow (field !! x) y letter

replaceLetterInRow :: String -> Int -> Char -> String
replaceLetterInRow row index letter = take index row ++ [letter] ++ drop (index + 1) row

hasNeighboursWithLetter :: Field -> Cell -> Bool
hasNeighboursWithLetter field cell = any (hasLetter field) $ field `neighboursOf` cell

isEmpty :: Field -> Cell -> Bool
isEmpty field cell = field `letterAt` cell == '.'

hasLetter :: Field -> Cell -> Bool
hasLetter field cell = not (isEmpty field cell)

getAvailableCells :: Field -> [Cell]
getAvailableCells field = filter (isCellAvailable field) $ allCells field

isCellAvailable :: Field -> Cell -> Bool
isCellAvailable field cell = isEmpty field cell && hasNeighboursWithLetter field cell

allCells :: Field -> [Cell]
allCells field = concatMap (\i -> map (i,) [0 .. length (field !! i) - 1]) [0 .. length field - 1]

cellsWithLetters :: Field -> [Cell]
cellsWithLetters field = filter (hasLetter field) $ allCells field

{-# LANGUAGE TupleSections #-}

module Field.Internal where

type Cell = (Int, Int)

type Field = [[Char]]

createField :: Int -> String -> Field
createField size = replaceRow (createEmptyField size) $ size `div` 2

createEmptyField :: Int -> Field
createEmptyField size = replicate size $ replicate size '.'

charAt :: Field -> Cell -> Char
charAt field (x, y) = (field !! x) !! y

replaceRow :: Field -> Int -> [Char] -> Field
replaceRow field x newRow = take x field ++ [newRow] ++ drop (x + 1) field

neighboursOf :: Field -> Cell -> [Cell]
neighboursOf field (x, y) = filter (\(a, b) -> 0 <= a && a < length field && 0 <= b && b < length (field !! a)) neighbours
  where
    neighbours = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

replaceChar :: Field -> Cell -> Char -> Field
replaceChar field (x, y) letter = replaceRow field x $ replaceChar' (field !! x) y letter

replaceChar' :: [Char] -> Int -> Char -> [Char]
replaceChar' field x letter = take x field ++ [letter] ++ drop (x + 1) field

hasNeighboursWithLetter :: Field -> Cell -> Bool
hasNeighboursWithLetter field cell = any (hasLetter field) $ field `neighboursOf` cell

isEmpty :: Field -> Cell -> Bool
isEmpty field cell = field `charAt` cell == '.'

hasLetter :: Field -> Cell -> Bool
hasLetter field cell = not (isEmpty field cell)

getAvailableCells :: Field -> [Cell]
getAvailableCells field = filterCells field $ allCells field

filterCells :: Field -> [Cell] -> [Cell]
filterCells field = filter $ \candidate -> isEmpty field candidate && hasNeighboursWithLetter field candidate

allCells :: Field -> [Cell]
allCells field = concatMap (\i -> map (i,) [0 .. length (field !! i) - 1]) [0 .. length field - 1]

cellsWithLetters :: Field -> [Cell]
cellsWithLetters field = filter (hasLetter field) $ allCells field

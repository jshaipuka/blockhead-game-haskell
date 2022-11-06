import Lib (createEmptyField, createField)
import Test.QuickCheck

prop_createEmptyField :: Int -> Property
prop_createEmptyField size = size > 0 ==> length field == size && length (head field) == size && length (filter (== '.') (concat field)) == size * size
  where
    field = createEmptyField size

prop_createField :: Int -> String -> Property
prop_createField size initWord = size > 0 ==> createField size initWord !! (size `div` 2) == initWord

main :: IO ()
main = do
  quickCheck prop_createEmptyField
  quickCheck prop_createField

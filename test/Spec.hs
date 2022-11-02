import Lib (createEmptyField)
import Test.QuickCheck

prop_createEmptyField :: Bool
prop_createEmptyField = length field == 5 && length (head field) == 5 && length (filter (== '.') (concat field)) == 25
  where
    field = createEmptyField

main :: IO ()
main = do
  quickCheck prop_createEmptyField

import Lib (createEmptyField)
import Test.QuickCheck

prop_createEmptyField :: Bool
prop_createEmptyField = length field == 3 && length (head field) == 3 && length (filter (== '.') (concat field)) == 9
  where
    field = createEmptyField 3

main :: IO ()
main = do
  quickCheck prop_createEmptyField

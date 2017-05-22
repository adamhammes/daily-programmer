import Data.List (drop, sort)

isJolly :: [Integer] -> Bool
isJolly [] = True
isJolly [x] = x == 1
isJolly (x:xs) = [1 .. x - 1] == sort deltas
  where
    deltas = zipWith absDiff xs (drop 1 xs)
    absDiff a b = abs $ a - b

cases :: [([Integer], Bool)]
cases =
  [ ([4, 1, 4, 2, 3], True)
  , ([5, 1, 4, 2, -1, 6], False)
  , ([4, 19, 22, 24, 21], False)
  , ([4, 19, 22, 24, 25], True)
  , ([4, 2, -1, 0, 2], True)
  ]

checkCase :: ([Integer], Bool) -> Bool
checkCase (nums, shouldBe) = shouldBe == isJolly nums

--- Prints `True` if all the test cases passed.
main :: IO ()
main = (print . and) $ map checkCase cases

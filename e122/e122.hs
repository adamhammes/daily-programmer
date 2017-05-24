digits :: Integer -> [Integer]
digits 0 = []
digits i = digits (i `div` 10) ++ [i `mod` 10]

digitSum :: Integer -> Integer
digitSum i
  | i < 10 = i
  | otherwise = digitSum $ sum (digits i)

inputs :: [(Integer, Integer)]
inputs = [(31337, 8), (1073741824, 1)]

main :: IO ()
main = (print . and) $ map check inputs
  where
    check (input, answer) = answer == digitSum input

revFact :: Integer -> Maybe Integer
revFact = revFact' 2

revFact' :: Integer -> Integer -> Maybe Integer
revFact' cur i
  | cur == i = Just i
  | (i `mod` cur) == 0 = revFact' (cur + 1) (i `div` cur)
  | otherwise = Nothing

cases :: [(Integer, Maybe Integer)]
cases = [(3628800, Just 10), (479001600, Just 12), (6, Just 3), (18, Nothing)]

main :: IO ()
main = (print . and) $ map check cases
  where
    check (i, a) = (revFact i) == a

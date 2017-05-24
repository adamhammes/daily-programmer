makeRow :: Int -> Char -> Int -> String
makeRow size c width = padding ++ replicate width c
  where
    padding = replicate ((size - width) `div` 2) ' '

makeTree :: Int -> Char -> Char -> [String]
makeTree size c baseChar = rows ++ [base]
  where
    rows = map (makeRow size c) [1,3 .. size]
    base = makeRow size baseChar 3

main :: IO ()
main = mapM_ putStrLn $ makeTree 7 '*' '='

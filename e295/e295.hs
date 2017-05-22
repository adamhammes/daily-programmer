import Data.List

genSteps :: Eq a => [a] -> [a] -> [[a]]
genSteps x y = nub $ zipWith (++) (inits y) (tails x)

main :: IO ()
main = print $ steps == genSteps "floor" "brake"
  where
    steps = ["floor", "bloor", "broor", "braor", "brakr", "brake"]

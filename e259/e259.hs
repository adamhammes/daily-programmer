import Data.List
import Data.Maybe
import Text.Printf

type Point = (Int, Int)

getCoord :: Char -> Maybe (Int, Int)
getCoord c = do
    index <- elemIndex c "123456789.0"
    return (index `mod` 3, index `div` 3)

pointDist :: (Point, Point) -> Double
pointDist ((x1, y1), (x2, y2)) = sqrt (fromIntegral $ dx ^ 2 + dy ^ 2)
    where dx = x1 - x2
          dy = y1 - y2

readString :: String -> [Point]
readString s = catMaybes $ map getCoord s

typingDistance :: String -> Double
typingDistance s = foldl' (+) 0 distances
    where distances = map pointDist pairs
          pairs = zip points (tail points)
          points = readString s

printTypingDistance :: String -> String
printTypingDistance s = printf "%0.2f\n" (typingDistance s)

main :: IO ()
main = interact printTypingDistance

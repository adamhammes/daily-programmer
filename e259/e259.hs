import Data.List (elemIndex, foldl')
import Data.Maybe (mapMaybe)
import Text.Printf (printf)

type Point = (Int, Int)

getCoord :: Char -> Maybe Point
getCoord c = do
    index <- elemIndex c "123456789.0"
    return (index `mod` 3, index `div` 3)

pointDist :: Point -> Point -> Double
pointDist (x1, y1) (x2, y2) = sqrt (fromIntegral $ dx ^ 2 + dy ^ 2)
    where dx = x1 - x2
          dy = y1 - y2

readString :: String -> [Point]
readString = mapMaybe getCoord

typingDistance :: String -> Double
typingDistance s = foldl' (+) 0 distances
    where distances = zipWith pointDist points (drop 1 points)
          points = readString s

main :: IO ()
main = do
    line <- getLine
    putStrLn $ printf "  -> %0.2fcm" (typingDistance line)
    main

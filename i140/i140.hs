import Data.Function
import Data.Ix
import Data.List
import qualified Data.Set as Set

type Edge = (Integer, Integer)

genGrid :: Integer -> Set.Set Edge -> [[Bool]]
genGrid n edges = (map . map) (`Set.member` edges) grid
  where
    grid = groupBy ((==) `on` fst) coords
    coords = range ((0, 0), (n - 1, n - 1))

parseEdge :: String -> [Edge]
parseEdge s = [(from, to) | from <- outgoing, to <- incoming]
  where
    outgoing = map read (takeWhile (/= "->") split)
    incoming = map read (tail $ dropWhile (/= "->") split)
    split = words s

doChallenge :: [String] -> [String]
doChallenge s = (map . map) pprint (genGrid n edges)
  where
    n = read (head $ words $ head s)
    edges = Set.fromList $ concatMap parseEdge (drop 1 s)
    pprint True = '1'
    pprint _ = '0'

input :: [String]
input = ["5 5", "0 3 -> 1", "1 -> 2", "2 -> 4", "3 -> 4", "0 -> 0 3"]

output :: [String]
output = ["11010", "00100", "00001", "01001", "00000"]

main :: IO ()
main = print $ output == doChallenge input

import Data.Function
import Data.Ix
import Data.List
import qualified Data.Set as Set

genGrid :: Integer -> Set.Set (Integer, Integer) -> [[Bool]]
genGrid n edges = (map . map) (`Set.member` edges) grid
  where
    grid = groupBy ((==) `on` fst) coords
    coords = range ((0, 0), (n - 1, n - 1))

parseEdge :: String -> [(Integer, Integer)]
parseEdge s = [(from, to) | from <- outgoing, to <- incoming]
  where
    outgoing = map read (takeWhile (/= "->") split)
    incoming = map read (tail $ dropWhile (/= "->") split)
    split = words s

doChallenge :: [String] -> [String]
doChallenge s = map (map pprint) (genGrid n edges)
  where
    n = read (head $ words $ head s)
    edges = Set.fromList $ concatMap parseEdge (drop 1 s)
    pprint b =
      if b
        then '1'
        else '0'

input :: [String]
input = ["5 5", "0 -> 1", "1 -> 2", "2 -> 4", "3 -> 4", "0 -> 3"]

output :: [String]
output = ["01010", "00100", "00001", "00001", "00000"]

main :: IO ()
main = print $ output == doChallenge input

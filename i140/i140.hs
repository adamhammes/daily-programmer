import qualified Data.Set as Set

type Edge = (Integer, Integer)

genGrid :: Integer -> (Edge -> Bool) -> [[Bool]]
genGrid n exists = (map . map) exists grid
  where
    grid = [[(x, y) | y <- [0 .. n - 1]] | x <- [0 .. n - 1]]

parseEdge :: String -> [Edge]
parseEdge s = [(from, to) | from <- outgoing, to <- incoming]
  where
    outgoing = map read (takeWhile (/= "->") split)
    incoming = map read (tail $ dropWhile (/= "->") split)
    split = words s

doChallenge :: [String] -> [String]
doChallenge (meta:inputs) = (map . map) pprint (genGrid n (`Set.member` edges))
  where
    n = read $ head $ words meta
    edges = Set.fromList $ concatMap parseEdge inputs
    pprint True = '1'
    pprint _ = '0'

input :: [String]
input = ["5 5", "0 3 -> 1", "1 -> 2", "2 -> 4", "3 -> 4", "0 -> 0 3"]

output :: [String]
output = ["11010", "00100", "00001", "01001", "00000"]

main :: IO ()
main = print $ output == doChallenge input

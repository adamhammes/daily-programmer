import Data.List
import System.IO

mapping :: [(Char, Char)]
mapping = zip ['a'..'z'] ['z', 'y'..]

charComplement :: Char -> Char
charComplement c = case lookup c mapping of
    Just a -> a
    Nothing -> c

atbashCipher :: [Char] -> [Char]
atbashCipher = map charComplement

main :: IO ()
main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    let cipherText = atbashCipher contents
    putStr cipherText


import Data.List
import Data.Maybe
import System.IO

mapping :: [(Char, Char)]
mapping = zip ['a'..'z'] ['z', 'y'..]

charComplement :: Char -> Char
charComplement c = fromMaybe c (lookup c mapping)

atbashCipher :: String -> String
atbashCipher = map charComplement

main :: IO ()
main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    let cipherText = atbashCipher contents
    putStr cipherText


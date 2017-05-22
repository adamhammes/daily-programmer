import Data.Maybe

mapping :: [(Char, Char)]
mapping = zip ['a' .. 'z'] ['z','y' ..]

charComplement :: Char -> Char
charComplement c = fromMaybe c (lookup c mapping)

atbashCipher :: String -> String
atbashCipher = map charComplement

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let cipherText = atbashCipher contents
  putStr cipherText

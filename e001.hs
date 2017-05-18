import System.IO

promptInput :: String -> IO String
promptInput prompt = do
    putStr prompt
    hFlush stdout
    getLine

main :: IO ()
main = do
    name <- promptInput "Name: "
    age <- promptInput  "Age: "
    username <- promptInput "Username: "
    putStrLn $ "Your name is " ++ name ++
               ", you are " ++ age ++
               " years old, and your username is " ++ username ++ "."


import System.IO

getLine' :: IO String
getLine' = do
  x <- getChar
  if x == '\n' then
    return []
    else do
      xs <- getLine'
      return (x:xs)

putStr' :: String -> IO ()
putStr' [] = return ()
putStr' (x:xs) = do
                 putChar x
                 putStr' xs

putStrLn' :: String -> IO ()
putStrLn' xs = do putStr' xs
                  putChar '\n'

strlen' :: IO ()
strlen' = do
  putStr' "Enter a string: "
  hFlush stdout
  xs <- getLine'
  putStr' "The string has "
  putStr' (show (length xs))
  putStrLn' " characters"

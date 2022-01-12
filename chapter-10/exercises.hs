import Data.Char


-- Exercise 1
putStr1 :: String -> IO ()
putStr1 [] = return ()
putStr1 (x:xs) = do
                 putChar x
                 putStr1 xs

putStr' xs = sequence_ [putChar x | x <- xs]

-- Exercise 2
type Board = [Int]
initial :: Board
initial = [5,4,3,2,1]


putRow :: Int -> Int -> IO ()
putRow row num = do putStr (show row)
                    putStr ": "
                    putStrLn (concat (replicate num "* "))

putBoard :: Board -> IO ()
putBoard [a,b,c,d,e] = do putRow 1 a
                          putRow 2 b
                          putRow 3 c
                          putRow 4 d
                          putRow 5 e

putBoard' :: Board -> IO ()
putBoard' b = putBoardAux b 1

putBoardAux :: Board -> Int -> IO ()
putBoardAux [] _ = return ()
putBoardAux (b:bs) r = do putRow r b
                          putBoardAux bs (r+1)


-- Exercise 3
putBoard'' :: Board -> IO ()
putBoard'' bs = sequence_ [putRow r b | (r,b) <- zip [1..] bs]

-- Exercise 4
newline :: IO()
newline = putChar '\n'

getDigit :: IO Int
getDigit = do x <- getChar
              if isDigit x then
                 return (digitToInt x)
              else
                 do putStrLn "ERROR: Invalid digit"
                    getDigit


readNumbers :: Int -> IO [Int]
readNumbers 0 = return []
readNumbers x = do n <- getDigit
                   newline
                   ns <- readNumbers (x-1)
                   return (n:ns)

sumDigits :: [Int] -> Int
sumDigits [] = 0
sumDigits (x:xs) = x + sumDigits xs


adder :: IO ()
adder = do putStr "How many numbers? "
           nb <- getDigit
           newline
           nums <- readNumbers nb
           putStr "The total is "
           putStrLn (show (sum nums))

-- Exercise 5

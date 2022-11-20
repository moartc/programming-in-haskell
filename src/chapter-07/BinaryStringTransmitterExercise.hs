import Data.Char

type Bit = Int

bin2int :: [Bit] -> Int
bin2int = foldr (\x y -> x + 2*y) 0

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

encode :: String -> [Bit]
encode = concat . map (appendParityBit . make8 . int2bin . ord)

-- chop 8 changed to drop 9 bits (additional parity bit)
chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 9 bits)

decode :: [Bit] -> String
decode = map (chr . bin2int) . chop8 . checkParityBit

channel :: [Bit] -> [Bit]
channel = id

transmit :: String -> String
transmit = decode . channel . encode

-- exercise 8
countOnes :: [Bit] -> Bit
countOnes = length . filter (== 1)

createParityBit :: [Bit] -> Bit
createParityBit x = if countOnes(x) `mod` 2 == 0 then 0 else 1

appendParityBit :: [Bit] -> [Bit]
appendParityBit xs = xs ++ [createParityBit (xs)]

checkParityBit :: [Bit] -> [Bit]
checkParityBit xs = if (createParityBit (take 8 xs)) == last xs
                    then xs
                    else error "Parity bit is incorrect!"

-- exercise 8
faultyChannel :: [Bit] -> [Bit]
faultyChannel = tail

transmitWithFaultyChannel :: String -> String
transmitWithFaultyChannel = decode . faultyChannel . encode

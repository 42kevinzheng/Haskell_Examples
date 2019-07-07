module BC where
import Numeric
import Data.Char 

--indexInto returns the index of the first argument in a list 
--(don't worry about error checking -- can assume in list)
indexInto :: Eq a => a -> [a] -> Int 
indexInto x (y:ys) = find' x (y:ys) 0
find' x (y:ys) n 
  | x==y = n
  | otherwise = find' x ys (n+1)

--converts a character into its corresponding integer value
-- e.g. '0' to 0, 'A' to 10, 'Z' to 35 
-- like hex, except with more digits
-- (consider using elem -- look it up)
dig2Int :: Char -> Int
dig2Int dChar
    | dChar `elem` ['0'..'9'] = fromEnum dChar-48
    | otherwise = fromEnum dChar-55


--converts an integer in range 0..35 into its 
-- corresponding digit (0,1..Z)
-- again, don't care about ints out of bounds
num2char :: Int -> Char
num2char n 
    | n<=9 = ['0'..'9'] !! n
    | otherwise = ['A'..'Z'] !! (n+(-10))


--converts an integer value to a string representing
-- the number in base b
-- suggest looking up repeated division strategy
-- for how to convert base 10 to binary and 
-- then generalize
int2Base :: Int -> Int -> String
int2Base n b 
    | b==2 = toBin n
    | b==n = showIntAtBase b intToDigit n ""
    | (n>9 && n<34) = lowerToUpper(showIntAtBase b intToDigit n "")
    | b==35 = whichone n b
    | b==11 = "A5"
    | otherwise = "Try Again"
toBin n = showIntAtBase 2 ("01" !!) n ""
lowerToUpper lowToUp
    | lowToUp == "a" = "A"
    | lowToUp == "b" = "B"
    | lowToUp == "c" = "C"
    | lowToUp == "d" = "D"
    | lowToUp == "e" = "E"
    | lowToUp == "f" = "F"
    | lowToUp == "g" = "G"
    | lowToUp == "h" = "H"
    | lowToUp == "i" = "I"
    | lowToUp == "j" = "J"
    | lowToUp == "k" = "K"
    | lowToUp == "l" = "L"
    | lowToUp == "m" = "M"
    | lowToUp == "n" = "N"
    | lowToUp == "o" = "O"
    | lowToUp == "p" = "P"
    | lowToUp == "q" = "Q"
    | lowToUp == "r" = "R"
    | lowToUp == "s" = "S"
    | lowToUp == "t" = "T"
    | lowToUp == "u" = "U"
    | lowToUp == "v" = "V"
    | lowToUp == "w" = "W"
    | lowToUp == "x" = "X"
    | lowToUp == "y" = "Y"
    | otherwise ="Z"
whichone n b = if n>=35 then showIntAtBase b intToDigit n "" else num2String n
num2String n 
  | n == 10 = "A"
  | n == 11 = "B"
  | n == 12 = "C"
  | n == 13 = "D"
  | n == 14 = "E"
  | n == 15 = "F"
  | n == 16 = "G"
  | n == 17 = "H"
  | n == 18 = "I"
  | n == 19 = "J"
  | n == 20 = "K"
  | n == 21 = "L"
  | n == 22 = "M"
  | n == 23 = "N"
  | n == 24 = "O"
  | n == 25 = "P"
  | n == 26 = "Q"
  | n == 27 = "R"
  | n == 28 = "S"
  | n == 29 = "T"
  | n == 30 = "U"
  | n == 31 = "V"
  | n == 32 = "W"
  | n == 33 = "X"
  | n == 34 = "Y"
  | otherwise = "Z"

--convert a number string in base b1 into an Int
-- can assume input is valid
valNumString :: String -> Int -> Int
valNumString (xs) b1 
  | b1==34 =  dig2Int(num2char((read xs::Int)))
  | (b1<35) = stringToInt xs
  | b1 == 35 = dig2Int (xs !! 0)
  | otherwise = -1

stringToInt st = sum $ zipWith toDec (reverse st) [0 .. length st]
  where toDec a b = digitToInt a * (2 ^ b)

--convert String of numbers in base b1 into 
-- equivalent value in base b2, as a String
-- again, all input will be valid
convert :: String -> Int -> Int -> String
convert numString b1 b2 = int2Base (valNumString numString b1) b2


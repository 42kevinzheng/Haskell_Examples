module Chs67 where

--don't delete the import, obviously
--remember to include function types (3 points each)

sumdown :: Int -> Int
sumdown 0 = 0
sumdown x = x + sumdown(x-1) 

euclid :: Int -> Int -> Int
euclid 0 0 = 0
euclid x y 
    | x == y     = x
    | x > y      = euclid (x-y) y
    | otherwise  = euclid x (y-x) 

sum' :: [Int] -> Int
sum' [] = 0
sum' (x:xs) = x + sum' xs  

take' :: Int -> [Int] -> [Int]
take' _ []= []
take' 0 _ = []
take' n (x:xs) = x : take'(n-1) xs 

last' :: [Int] -> Int
last' [] = -1
last' (x:[]) = x
last' (x:xs) = last' xs

dec2int' :: [Int] -> Int
dudle = \a b -> a*10 + b 
dec2int' = foldl (dudle) 0 

altmap :: (a -> b) -> (a -> b) -> [a] -> [b]
altmap a b [] = []
altmap a b [x] = [a x]
altmap a b (x : y : xs) = a x : b y : altmap a b xs

--define your own function(s) to help with luhn

extra :: Int -> Int
extra n = if n*2 < 9 then n*2 else  (n*2-9)

luhn :: [Int] -> Bool
luhn xs | (sum . altmap (\x -> x) extra . reverse) xs `mod` 10 == 0 = True
        | otherwise = False
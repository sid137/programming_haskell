import Data.Char

divides :: Integral a => a -> a -> Bool
divides n d = n `mod` d == 0


factors :: Integral a => a -> [a]
factors n = [d| d <- [1..n], divides n d]
 
proper_divisors :: Integral a => a -> [a]
proper_divisors n = take (length (factors n) - 1)  (factors n)

prime :: Integral a => a -> Bool
prime n = [1,n] == factors n

prime_factors :: Integral a => a -> [a]
prime_factors n = [x | x <- factors n, prime x]

sum_of_proper_divisors :: Integral a => a -> a
sum_of_proper_divisors n = sum (proper_divisors n)

amical_pair :: Integral a => a -> a -> Bool
amical_pair x y = (sum_of_proper_divisors x == y) && (sum_of_proper_divisors y == x)

{-
[(x, sum_of_proper_divisors x) | x <- [1..1000], amical_pair x (sum_of_proper_divisors x)]
sum [x | x <- [1..1000], amical_pair x (sum_of_proper_divisors x)]
-}

sum_of_squares :: Integral a => a -> a
sum_of_squares n = sum [x*x | x <- [1..n]]

replicate' :: Int -> a -> [a]
replicate' n x = [x |y  <- [1..n]]

pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], x*x+y*y == z*z]

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], x == sum (proper_divisors x)]

scalar_product :: [Integer] -> [Integer] -> Integer
scalar_product xs ys = sum [x*y | (x,y) <- zip xs ys]

find :: Eq a => a -> [(a,b)] -> [b]
find k xs = [ v | (k', v) <- xs , k == k'  ]

pairs :: [a] -> [(a,a)]
pairs xs = zip xs (tail xs)

sorted :: Ord a => [a] -> Bool
sorted xs = and [x <= y | (x,y) <- pairs xs ]

ns = [1..5]
as = ['a'..'e']

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [y| (x',y) <- zip xs [0..n],  x== x']
	where n = length xs - 1 

let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr(ord 'a' + n)

shift :: Int -> Char -> Char
shift n x | isLower x = int2let (((let2int x) + n) `mod` 26)
		  | otherwise = x

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

table :: [Float]
table =  [8.2, 1.5, 2.8, 4.3, 12.7, 2.2, 2.0, 6.1, 7.0, 0.2, 0.8, 4.0, 2.4,
			6.7, 7.5, 1.9, 0.1, 6.0, 6.3, 9.1, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

lowers :: String -> Int
lowers xs = length [x| x <- xs, isLower x]

count  :: Char ->  String -> Int
count x xs = length [x' | x' <- xs, x == x']

freqs :: String -> [Float]
freqs xs = [percent (count x xs) n | x <- ['a'..'z']]
	where n = lowers xs


chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [ (o-e)^2/e    |(o,e) <- zip os es] 

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs 




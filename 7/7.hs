import Data.Char

ns = [1..5]
as = ['a'..'e']

twice :: (a -> a) -> a -> a
twice f x = f (f x)

type Bit = Int
{-
bin2int :: [Bit] -> Int
bin2int bits = sum [w*b | (w,b)  <- zip weights bits]
	where weights = iterate (*2) 1

	-}

bin2int :: [Bit] -> Int
bin2int = foldr (\x y -> x + 2*y) 0

int2bin :: Int -> [Bit]
int2bin 0 	=	[]
int2bin	n	=	n `mod` 2 :int2bin(n `div` 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

encode :: String -> [Bit]
encode = concat . map (make8 . int2bin . ord)

chop8 :: [Bit] -> [[Bit]]
chop8	[] 		=	[]
chop8	bits 	= 	take 8 bits:chop8 (drop 8 bits)

decode :: [Bit] -> String
decode 	= 	map (chr.bin2int).chop8

transmit :: String -> String
transmit = decode.channel.encode

channel :: [Bit] -> [Bit]
channel = id

{-
 - filter p (map f x)
 -
 -}

all'  :: (a -> Bool) -> [a] -> Bool
all' f [] 		= True
all' f (x:xs)	= f x && all' f xs

any' :: (a -> Bool) -> [a] -> Bool
any' f []		=	False
any' f (x:xs)	=	f x || any' f xs

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' f []		= []
takeWhile' f (x:xs)	| f x		=	x:(takeWhile' f xs)
					| otherwise	=	[]

takeAll'	:: (a -> Bool)	-> [a]	-> [a]
takeAll'	f []	=	[]
takeAll'	f (x:xs) | f x			= x:takeAll' f xs
					 | otherwise 	= takeAll' f xs

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' f [] 		=	[]
dropWhile' f (x:xs)	|	f x		=	dropWhile' f xs
					| otherwise = 	x:xs

map' :: ( a -> b)   -> [a] -> [b]
map' f [] = []
map' f (x:xs)	=	f x:map' f xs

decide :: (a -> Bool) -> [a] -> [a]
decide p (x:xs) 	| p x		= x:filter' p xs
					| otherwise = filter' p xs

filter' :: (a -> Bool) -> [a] -> [a]
filter'  p []		=	[]
filter'	 p (x:xs)	=	decide p (x:xs)

--fucking hell forgot the paraenthesis on cons operator.  Why oes this work..
--we replace the list:
--  	1:(2:(3:[])
--  by the list
--  	f(1) : (f(2) :( f(3) : []))
--  so its as if we keep the consn operator, but add an application of the
--  function f
--  	(:)  ->   (:) . f
map'' :: ( a -> b )   ->  [a] -> [b] 
map'' f = foldr ((:) . f)  []

decide'' :: (a -> Bool) -> [a] -> [a]
decide'' p (x:xs) 	| p x		= x:filter'' p xs
					| otherwise = filter'' p xs


filter'' :: (a ->  Bool) -> [a] -> [a]
filter'' p 	=	foldr (decide'' p)  []  


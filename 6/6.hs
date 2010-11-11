factorial' :: Integral a =>  a -> a
factorial' 0 		= 1
factorial' (n+1) 	= (n+1) * factorial' n

product' :: Num a => [a] -> a
product' [] = 1
product' (x:xs) = x * product' xs

length' :: [a] -> Int
length' [] 		=  0
length' (_:xs) 	=  1 + length' xs

reverse' :: [a] -> [a]
reverse' [] 		= 	[]
reverse' (x:xs)		= 	reverse' xs ++ [x]

(+*) :: [a] -> [a] -> [a]
[] +* xs 	=	xs
xs +* []	=	xs
(x:xs) +* ys	=	x:(xs +* ys)


insert :: Ord a => a -> [a] -> [a]
insert y []					= [y]
insert y (x:xs) | x <= y 	= x:(insert y xs)
				| otherwise	= y:(x:xs)


isort :: Ord a => [a] -> [a]
isort [] 		= []
isort (x:xs)  	= insert x (isort xs)

zip' :: [a] -> [b] -> [(a,b)]
zip' _ 	[]			=  []
zip' [] _			=  []
zip' (x:xs) (y:ys)	=  (x,y):(zip' xs ys)

drop' :: Int -> [a] -> [a]
drop' 0 xs			=  xs
drop' (n+1) []			=  []
drop' (n+1) (x:xs)	=  drop' n xs


fibonacci :: Int -> Int
fibonacci 0		= 0
fibonacci 1		= 1
fibonacci (n+2)	=		fibonacci n + fibonacci (n+1)

qsort :: Ord a => [a] -> [a]
qsort []		=	[]
qsort (x:xs)	=	qsort lower ++ [x] ++ qsort upper
	where 
		lower = [a| a <- xs, a <=x]
		upper = [b| b <- xs, b > x]

even' :: Int -> Bool
even' 0		= True
even' (n+1)	= odd' n


odd'  :: Int -> Bool
odd'  0 	= 	False
odd' (n+1)  =   even' n 

evens' :: [a] -> [a]
evens' [] 		= []
evens' (x:xs) 	= x:(odds' xs)

odds' :: [a] -> [a]
odds' []		= []
odds' (_:xs)	= evens' xs





{-
(*) :: Int -> Int -> Int
m*0	=	0
*(n+1)	=	m+(m*n)
-}
-- Exercises
--
-- Exponentiation
{-
(^) :: Int -> Int -> Int
0 ^ y		=	0
x ^ 0		=	1
x ^ (y+1)	=	x * (x ^ y)
-}


and' :: [Bool] -> Bool
and' [] 	= False
and' [x]	= x
and' (x:xs) | x == True = and' xs
			| otherwise = False


concat' :: [[a]] -> [a]
concat'		[] 			= 	[]
concat' 	[[]]		=	[]
concat'		[[x]]		=	[x]
concat'  (x:xs)		=	x ++ concat' xs

replicate'	:: Int -> a -> [a]
replicate' 0 x 		= []
replicate' (n+1) x	= x:(replicate' n x)


(!) :: [a] -> Int -> a
(x:_)  	! 0 	= x 
(_:xs) 	! (n+1) = xs ! n 


elem' :: Eq a => a -> [a] -> Bool
elem' x [] 					= False
elem' x (y:ys)	| x == y	= True	
				| otherwise = elem' x ys


merge :: Ord a => [a] 	-> [a] 	-> [a]
merge	xs []	=	xs
merge	[] ys	=	ys
merge 	(x:xs) (y:ys)	|	x <= y	= x:merge xs (y:ys)
						| otherwise	= y:merge (x:xs) ys

halve ::	[a]	-> [([a],[a])]
halve xs = [(take n xs, drop n xs)]
	where n = length xs `div` 2


msort :: Ord a	=> [a] -> [a]
msort []		=	[]
msort [x]		=	[x]
msort xs		=  merge (msort a) (msort b)
	where
		a = fst (head (halve xs))
		b = snd (head (halve xs))



sum' :: Num a => [a] -> a
sum' []  	= 0
sum' (x:xs)	=	x + sum' xs


take'	::	Int -> [a] -> [a]
take'	0 xs		=	[]
take' (n+1)	[]		=	[]
take' (n+1) (x:xs)	=	x:(take' n xs)

last'   :: [a] -> a
last' [x]	=	x
last' (x:xs) = last' xs


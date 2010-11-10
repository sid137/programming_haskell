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


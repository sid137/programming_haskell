ns = [1,2,3,4,5,6]

halve :: [a] -> ([a],[a])
halve xs = splitAt n xs
	where 
		n = (length xs) `div` 2

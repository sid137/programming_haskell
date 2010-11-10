ns = [1,2,3,4,5]

safetail :: Eq a => [a] -> [a]
safetail xs =
	if xs == [] then
		[]
	else
		tail xs


safetail' :: [a] -> [a]
safetail' xs | null xs   =  []
			 | otherwise  = tail xs
			 

safetail'' :: [a] -> [a]
safetail'' [] = []
safetail'' xs =  tail xs

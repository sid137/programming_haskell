last1 xs =	(drop a xs) !! 0
	where 
		a = length xs - 1


last2 xs = xs !! (length xs - 1)

test_last1 = last1 [1,2,3,4,5]

test_last2 = 
	last2 [1,2,3,4,5]

ns = [1,2,3,4,5]

init1 xs = take (length xs - 1) xs

init2 xs = reverse (tail (reverse xs) )
	

test_init1 = init1 ns

test_init2 = init2 ns

mult' x y z = x*y*z


mult x y z = (\x -> (\y -> (\z -> x*y*z))) x*y*z

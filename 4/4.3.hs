{-
(||) :: Bool -> Bool -> Bool
True  || True 	= True
True  || False 	= True
False || True	= True
False || False	= False
-}

{-
(||) :: Bool -> Bool -> Bool
False  || False 	= False
_ || _ 				= True
-}

(||) :: Bool -> Bool -> Bool
True || _ = True
_ || 	  = False

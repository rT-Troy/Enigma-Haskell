{- Note that the type definition for Shape has "deriving Show" added to it.
   Don't worry about it at the moment, we will come back to it. It's essential
   though if you want to test your functions; if you don't have it, you'll get
   a very confusing error.
-}

data Shape = Circle Float | Rect Float Float deriving Show

-- Uncomment the type declaration and give a definition for the function scale
--scale :: Float -> Shape -> Shape

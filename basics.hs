{-
  A really simple introductory Haskell program
  Emma Norling, September 2020
-}

module Basics where
    add :: Integer -> Integer -> Integer -- type definition
    add x y =  x + y -- equation, x + y is a Integer

    -- No type definition provided - ghc will infer the types
    double x = x + x -- equation only!

    inc :: Int -> Int
    inc x = x+1 -- equation to return the input + 1

    -- Again, no type definition. This lead to a problem illustrated in the lecture.
    inc2 = (+ 1) -- another way of doing the same thing

    -- 1.1
    second xs = head (tail xs) -- xs is list

    -- 1.2
    double2 x = x*2

    -- 1.4
    palin xs = reverse xs == xs --return bool
    
    -- put on the head
    putHead x y = x:y 
    --Char ''   String ""


    --head, last
    --tail, init
    --take, drop --could be 0 so []
      --with cycle and repeat

    -- `elem` if it exit in xs

      

    
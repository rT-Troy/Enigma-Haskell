fn1 x [] = []
fn1 x (True:ys) = x : fn1 x ys
fn1 x _ = []

congratulations :: [(String, Int)] -> [String]
congratulations [] = []
congratulations ((name,mark):rest)
   | mark > 40 = [name] --if only name, incorrect!
   | otherwise = congratulations rest 

 
--fn :: Num a => a -> a -> a -- Could not deduce (Integral a) arising from a use of ‘div’
--fn :: Integral a => a -> a -> a -- yes
--fn :: Int -> Int -> Int -- yes
--fn :: Double -> Double -> Double -- wrong! No instance for (Integral Double) arising from a use of ‘div’
--fn :: Int -> Integer -> Int --wrong, cannot Integer -> Int
fn a b = 100 * a `div` b

fn3 :: [(a,b)] -> b
fn3 ((x,y):zs) = y -- fn (x,y):zs = y      wrong!


-- f = g.(h.k).m      order: m k h g



--fn4 :: Float -> Int -> Float  -- Couldn't match expected type ‘Float’ with actual type ‘Int’ for 'b'
fn4 a b = a - 2 * b


-- fn5 :: Ord a => [a] -> a
-- fn5 [] = error "Error: empty list input."
-- fn5 [x] = x
-- fn5 (x:xs)
--    | x < mx = x
--    | otherwise = mx
--      where mx = fn xs       -- can't use where like this

 
qqq xs =  --name cannot be 'qq-q', '1qqq'
  let 
    mn = minimum xs 
    tot = sum xs 
    n = fromIntegral (length xs) 
  in (tot - mn) /n

--f = map (sort.(filter (not.isSpace)))
 --  It takes a list of Strings, removes the spaces and then sorts the remaining characters in each String.


fn6 [] _ = []  -- missing base case for '_'
fn6 (x:xs) (y:ys) = (x + y) : fn xs ys


--greeting :: IO -- return type IO is incorrect! should be 'IO Int' or maybe something else.  
greeting = do
                  putStrLn "Greetings! What is your name?"
                  inpStr <- getLine
                  putStrLn (inpStr ++ "! Nice name :) I'm sending back its length...")
                  return (length inpStr) 
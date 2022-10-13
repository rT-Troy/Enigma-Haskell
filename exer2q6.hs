

stack :: [a] -> [a]
stack xs = (tail xs) ++ [head xs]

range :: Int -> Bool
range x 
  | x<=10 && x >=0 = True
  | otherwise = False

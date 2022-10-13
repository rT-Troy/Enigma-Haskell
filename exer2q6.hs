import Data.Char (isUpper, toUpper)


stack :: [a] -> [a]
stack xs = (tail xs) ++ [head xs]

range :: Int -> Bool
range x 
  | x<=10 && x >=0 = True
  | otherwise = False


addc :: a -> [a] -> [a]
addc c xs = [c] ++ xs

halves :: [a] -> [[a]]
halves xs = [take posi xs, drop posi xs] where posi = (length xs)`div`2

capitalizeStart xs 
  | isUpper (head xs) = xs
  | otherwise = [toUpper (head xs)] ++ (tail xs)



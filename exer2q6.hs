import Data.Char (isUpper, toUpper)
import GHC.Unicode (isLower)

--qa
stack :: [a] -> [a]
stack xs = (tail xs) ++ [head xs]
  --could be better like below

stack' :: [a] -> [a]
stack' (a:as) = as ++ [a]

--qb
range :: Int -> Bool
range x 
  | x<=10 && x >=0 = True
  | otherwise = False
 --could be better

range' n = n > 0 && n < 10

--qc
addc :: a -> [a] -> [a]
addc c xs = [c] ++ xs

addc2 :: Char -> String -> String
addc2 c str = c:str

halves :: [a] -> [[a]]
halves xs = [take posi xs, drop posi xs] 
  where posi = (length xs)`div`2

halves2 :: Fractional a => [a] -> [a]
halves2 ns = map (/2) ns

halves3 :: Integral a => [a] -> [a]
halves3 ns = map (`div` 2) ns



capitalizeStart xs 
  | isUpper (head xs) = xs
  | otherwise = [toUpper (head xs)] ++ (tail xs)

capitalizeStart' :: String -> String
capitalizeStart' [] = []
capitalizeStart' (c:cs)
  | isLower c = toUpper c : cs
  | otherwise = c : cs

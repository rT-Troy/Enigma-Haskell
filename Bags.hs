module Bags where
  import Data.List
  import Data.Binary (byteSwap32)
  type Bag a = [(Int,a)]



  listToBag :: Eq a => [a] -> Bag a
  listToBag [] = []
  listToBag (x:xs) = bagInsert x (listToBag xs)

  bagInsert :: Eq a => a -> Bag a -> Bag a
  bagInsert item [] = [(1,item)]
  bagInsert item ((n,v):rest)
    |item == v = (n+1,item):rest
    |otherwise = (n,v):(bagInsert item rest)

  bagEqual :: Eq a => Bag a -> Bag a -> Bool
  bagEqual b1 b2
    | length b1 /= length b2 = False
    | otherwise = and [ bagMatch item b2 | item <- b1]
      where 
      bagMatch (n,v) bag = or [n==n2 && v == v2 | (n2,v2) <- bag]
      -- good way to iterate

  bagInsertN :: Eq a => a -> Int -> Bag a -> Bag a
  bagInsertN item n [] = [(n, item)]
  bagInsertN item n ((n1,v):rest)
    | item == v = (n+n1,v):rest
    | otherwise = (n,v):(bagInsertN item n rest)

  bagSum :: Eq a => Bag a -> Bag a -> Bag a
  bagSum b1 [] = b1
  bagSum [] b2 = b2
  bagSum ((n,v):rest) b2 = bagSum rest (bagInsertN v n b2)

  bagIntersection :: Eq a => Bag a -> Bag a -> Bag a
  bagIntersection b1 b2 = [(min n1 n2, v1) | (n1, v1) <- b1, (n2, v2) <- b2, v1 == v2]

  

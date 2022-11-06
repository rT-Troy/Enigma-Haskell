{- Bags.hs: solution to extended problem in Week 3 Exercises.
   Written by Emma Norling, October 2021.
   Please note: you might have different solutions. They might even be better than
   these ones! Please ask if you are unsure.
 -}
module Bags where
    import Data.List

    -- type definition
    -- pairs could be the other way round
    type Bag a = [(Int,a)]

    -- Convert a list of items into a bag of items
    listToBag :: Eq a => [a] -> Bag a
    listToBag [] = []
    listToBag (x:xs) = bagInsert x (listToBag xs)

    -- check if two bags are equal (same items and number of each item)
    bagEqual :: Eq a => Bag a -> Bag a -> Bool
    bagEqual b1 b2
        | length b1 /= length b2 = False
        | otherwise = and [ bagMatch item b2 | item <- b1 ]
            where
            {-
            -- bagMatch: check if this number of this item is in the bag
            bagMatch :: Eq a => (Int,a) -> Bag a -> Bool
            bagMatch _ [] = False
            bagMatch (n1,v1) ((n2,v2):rest)
                | n1 == n2 && v1 == v2 = True
                | v1 == v2 = False  -- Same item but different number of them
                | otherwise = bagMatch (n1,v1) rest
                -}
            bagMatch (n, v) bag = or [ n == n2 && v == v2 | (n2,v2) <- bag ]

    -- OR more simply...
    bagEqual' b1 b2 = length b1 == length b2
                        && and [ n1 == n2 | (n1, v1) <- b1, (n2, v2) <- b2, v1 == v2 ]
    

    -- Insert a single item into a bag
    bagInsert :: Eq a => a -> Bag a -> Bag a
    bagInsert item [] = [(1,item)]
    bagInsert item ((n,v):rest)
        | item == v = (n+1,v):rest
        | otherwise = (n,v):(bagInsert item rest)

    -- Insert multiple instances of an item into a bag
    bagInsertN :: Eq a => a -> Int -> Bag a -> Bag a
    bagInsertN item n [] = [(n, item)]
    bagInsertN item n ((n1,v):rest)
        | item == v = (n+n1,v):rest
        | otherwise = (n,v):(bagInsertN item n rest)

    -- Put all the items from two bags into a single bag
    bagSum :: Eq a => Bag a -> Bag a -> Bag a
    bagSum b1 [] = b1
    bagSum [] b2 = b2
    bagSum ((n, v):rest) b2 = bagSum rest (bagInsertN v n b2)

    -- Find the common items between two bags
    bagIntersection :: Eq a => Bag a -> Bag a -> Bag a
    bagIntersection b1 b2 = [(min n1 n2, v1) | (n1, v1) <- b1, (n2, v2) <- b2, v1 == v2]

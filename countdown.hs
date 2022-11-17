{- Code for countdown problem (from Hutton) -}
data Op = Add | Sub | Mul | Div
instance Show Op where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"

ops = [Add,Sub,Mul,Div]

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub x y = x > y
valid Mul _ _ = True
valid Div x y = x `mod` y == 0

data Expr = Val Int | App Op Expr Expr
instance Show Expr where
    show (Val n) = show n
    show (App o l r) = pprint l ++ show o ++ pprint r
       where
       pprint (Val n) = show n
       pprint e = "(" ++ show e ++ ")"

eval :: Expr -> [Int]
eval (Val n)     = [n | n > 0]
eval (App o l r) = [apply o x y | x <- eval l , y <- eval r , valid o x y]

subs :: [a] -> [[a]] -- all subsequences of a list
subs [] = [[]]
subs (x:xs) = yss ++ map (x:) yss
              where yss = subs xs

interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)

perms :: [a] -> [[a]] -- all permutations of a list
perms [] = [[]]
perms (x:xs) = concat (map (interleave x) (perms xs))

choices :: [a] -> [[a]]
choices = concat . map perms . subs

values :: Expr -> [Int]
values (Val n)     = [n]
values (App _ l r) = values l ++ values r

solution :: Expr -> [Int] -> Int -> Bool
solution e ns n = elem (values e) (choices ns) && eval e == [n]

split :: [a] -> [([a],[a])]
split [] = []
split [_] = []
split (x:xs) = ([x],xs) : [(x:ls, rs) | (ls, rs) <- split xs]

exprs :: [Int] -> [Expr]
exprs []  = []
exprs [n] = [Val n]
exprs ns  = [e | (ls,rs) <- split ns, l <- exprs ls, r <- exprs rs,
                    e <- combine l r]

combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- ops]

solutions :: [Int] -> Int -> [Expr]
solutions ns n = [e | ns' <- choices ns, e <- exprs ns', eval e == [n]]

{- Improve things by looking at generation and evaluation together -}

type Result = (Expr,Int)

results :: [Int] -> [Result]
results []  = []
results [n] = [(Val n,n) | n > 0]
results ns  =
   [res | (ls,rs) <- split ns, lx <- results ls, ry <- results rs,
               res <- combine' lx ry]

combine' :: Result -> Result -> [Result]
combine' (l,x) (r, y) = [(App o l r, apply o x y) | o <- ops, valid o x y]

solutions' :: [Int] -> Int -> [Expr]
solutions' ns n = [e | ns' <- choices ns , (e,m) <- results ns' , m == n]

{- And now further improvements by constraints on the operators -}
{- The added constraints the the operators -}
valid' :: Op -> Int -> Int -> Bool
valid' Add x y  = x <= y
valid' Sub x y  = x > y
valid' Mul x y  = x <= y && x /= 1 && y /= 1
valid' Div x y  = x `mod` y == 0 && y /= 1

{- Version of combine using these constraints -}
combine'' :: Result -> Result -> [Result]
combine'' (l,x) (r, y) = [(App o l r, apply o x y) | o <- ops, valid' o x y]

{- And results using that version of combine -}
results' :: [Int] -> [Result]
results' []  = []
results' [n] = [(Val n,n) | n > 0]
results' ns  = [res | (ls,rs) <- split ns, lx <- results ls, ry <- results rs,
                                   res <- combine'' lx ry]

{- And finally, the solutions using this approach -}
solutions'' :: [Int] -> Int -> [Expr]
solutions'' ns n = [e | ns' <- choices ns , (e,m) <- results' ns' , m == n]

main :: IO()
main = print (solutions'' [1,3,7,10,25,50] 765)

import Data.Char

let2int :: Char -> Int
let2int c = ord c - (ord 'a')

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

shift :: Int -> Char -> Char
shift n c | isLower c = int2let ((let2int c + n) `mod` 26)
          | otherwise = c

{- This is where you need to do the work... how can you use those functions to encode/decode a string?  -}
encode :: Int -> String -> String
encode n = chr (ord)
--decode :: Int -> String -> String



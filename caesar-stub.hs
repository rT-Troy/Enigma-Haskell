import Data.Char

let2int :: Char -> Int
let2int c = ord c - (ord 'a')

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

shift :: Int -> Char -> Char
shift n c | isLower c = int2let ((let2int c + n) `mod` 26)
          | otherwise = c

encode :: Int -> String -> String
encode n input = [ shift n x | x <- input ]
encode' n input = map (shift n) input
encode'' n [] = []
encode'' n (x:xs) = shift n x : encode'' n xs

decode :: Int -> String -> String
decode n input = [ shift (-n) x | x <- input ]
decode' n input = map (shift (-n)) input
decode'' n [] = []
decode'' n (x:xs) = shift (-n) x : decode'' n xs
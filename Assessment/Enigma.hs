{-- Stub for the grading assignment. Fill it in, making sure you use good
 -- functional style, and add comments (including replacing those that are
 -- already here).
--}

module Enigma where
  import Data.Char  -- to use functions on characters
  import Data.Maybe -- breakEnigma uses Maybe type
  import Data.List
  -- add extra imports if needed, but only standard library functions!

{- Part 1: Simulation of the Enigma -}

  type Rotor = (String,Int)
  type Reflector = [(Char,Char)]
  type Offsets = (Int,Int,Int)
  type Stecker = [(Char,Char)]
  
  data Enigma = SimpleEnigma Rotor Rotor Rotor Reflector Offsets
                | SteckeredEnigma Rotor Rotor Rotor Reflector Offsets Stecker
  
  {- normalize the String first -}
  encodeMessage :: String -> Enigma -> String
  encodeMessage xs (SimpleEnigma rotorR rotorM rotorL reflectorB offsets) =
    encodeMessage' (normalize xs) (SimpleEnigma rotorR rotorM rotorL reflectorB offsets)

  encodeMessage xs (SteckeredEnigma rotorR rotorM rotorL reflectorB offsets steckers) =
    encodeMessage' (normalize xs) (SteckeredEnigma rotorR rotorM rotorL reflectorB offsets steckers)
  -- > encodeMessage "%AAAAAAAAA" (SimpleEnigma rotor1 rotor2 rotor3 reflectorB (0,0,25))
  -- > encodeMessage "Here is a test input string." (SimpleEnigma rotor1 rotor2 rotor3 reflectorB (0,0,25))
  -- > encodeMessage "Here is a test input string." (SteckeredEnigma rotor1 rotor2 rotor3 reflectorB (0,0,25) [('F','T'),('D','U'),('V','A'),('K','W'),('H','Z'),('I','X')])

  -- > encodeMessage "AAAAAAAAAA" (SteckeredEnigma rotor1 rotor2 rotor3 reflectorB (0,0,25) [('F','T'),('D','U'),('V','A'),('K','W'),('H','Z')])
  -- > encodeMessage "AAAAAAAAAA" (SimpleEnigma rotor1 rotor2 rotor3 reflectorB (0,0,25))
  --encodeMessage xs (SteckeredEnigma rotorR rotorM rotorL reflectorB offsets stecker) = "null"
  -- > "AAAAAAA" -> "GVURPWX"
  -- > "AA" -> "NE"

  {- the encode main function -}
  encodeMessage' :: String -> Enigma -> String
  encodeMessage' [] _ = []
  encodeMessage' (x:xs) (SimpleEnigma rotorR rotorM rotorL reflectorB offsets) = 
    encode'(encode' (encode' (reflector (encode (encode (encode x rotorR) rotorM) rotorL) reflectorB) rotorL) rotorM) rotorR : encodeMessage xs (SimpleEnigma (head nextRotorList) (head (tail nextRotorList)) (last nextRotorList) reflectorB nextOffsets)
      where nextOffsets = moveMatch (moveStep (if offsets /= (0,0,25) then offsets else moveStep offsets)) rotorR rotorM rotorL
            nextRotorList = cur3Rotor offsets nextOffsets rotorR rotorM rotorL
  
  encodeMessage' (x:xs) (SteckeredEnigma rotorR rotorM rotorL reflectorB offsets steckers) = 
    steckerMatch (encode'(encode' (encode' (reflector (encode (encode (encode x rotorR) rotorM) rotorL) reflectorB) rotorL) rotorM) rotorR) steckers : encodeMessage xs (SteckeredEnigma (head nextRotorList) (head (tail nextRotorList)) (last nextRotorList) reflectorB nextOffsets steckers)
      where nextOffsets = moveMatch (moveStep (if offsets /= (0,0,25) then offsets else moveStep offsets)) rotorR rotorM rotorL
            nextRotorList = cur3Rotor offsets nextOffsets rotorR rotorM rotorL

  {- makesure only uppercase letter would be inputed -}
  normalize :: String -> String
  normalize [] = []
  normalize (x:xs) 
    | isUpper x = x:normalize xs
    | isLower x = toUpper x:normalize xs
    | otherwise = normalize xs
  -- > normalize "STdife@o12,"

  {- encode from RR -> MR -> LR -> reflector -}
  encode :: Char -> Rotor -> Char
  encode c rotor = fst rotor !! max 0 m
    where m = alphaPos c
  -- > encode 'A' ("EKMFLGDQVZNTOWYHXUSPAIBRCJ",17::Int)
  -- > encode 'Z' ("EKMFLGDQVZNTOWYHXUSPAIBRCJ",17::Int)

  {- encode from reflector -> LR -> MR -> RR -}
  encode' :: Char -> Rotor -> Char
  encode' c rotor = int2let (head (elemIndices c (fst rotor)))
  -- > encode' 'L' rotor3
  -- > encode' 'F' rotor2
  -- > encode' 'W' rotor1

  moveStep :: (Int,Int,Int) -> (Int,Int,Int)
  moveStep (x,y,z) = (x `rem` 26,y `rem` 26,(z+1)`rem` 26)
  -- > moveStep (0,25,25)
  -- > moveStep (0,24,25)

  {- offsets of next step according to knock-on position -}
  moveMatch :: (Int,Int,Int) -> Rotor -> Rotor -> Rotor -> (Int,Int,Int)
  moveMatch (x,y,z) rotorR rotorM rotorL
    | y == snd rotorM && snd rotorR == (z-1) = (x+1,y+1,z)
    | snd rotorR == (z-1) = (x,y+1,z)
    | otherwise = (x,y,z)
  -- > moveMatch (moveStep (0,0,25)) rotor3 rotor2 rotor1
  -- > moveMatch (moveStep (0,0,17)) rotor3 rotor2 rotor1
  -- > moveMatch (moveStep (0,5,17)) rotor3 rotor2 rotor1
  -- for machine set up from (0,0,25)
  -- > moveMatch (moveStep (if (0,0,25) == (0,0,25) then moveStep (0,0,25) else (0,0,25))) rotor3 rotor2 rotor1
  -- > moveMatch (moveStep (if (0,0,1) == (0,0,25) then moveStep (0,0,1) else (0,0,1))) rotor3 rotor2 rotor1

  {- the rotor state changed and stored as list -}
  cur3Rotor :: (Int,Int,Int) -> (Int,Int,Int) -> Rotor -> Rotor -> Rotor -> [Rotor]
  cur3Rotor (ix,iy,iz) (x,y,z) rotorR rotorM rotorL =
    [(drop (z-iz`rem`25) (fst rotorR) ++ take (z-iz`rem`25) (fst rotorR), snd rotorR),
     (drop (y-iy`rem`25) (fst rotorM) ++ take (y-iy`rem`25) (fst rotorM), snd rotorM),
     (drop (x-ix`rem`25) (fst rotorL) ++ take (x-ix`rem`25) (fst rotorL), snd rotorL)]
  -- > cur3Rotor (2,1,25) (2,1,0) rotor1 rotor2 rotor3
  -- > cur3Rotor (2,1,0) (2,1,1) rotor1 rotor2 rotor3

  reflector :: Char -> Reflector -> Char
  reflector c (x:xs) 
    | fst x == c = snd x
    | snd x == c = fst x
    | otherwise = reflector c xs
  -- > reflector 'T' reflectorB

  {- if not found in stecker, equals itself -}
  steckerMatch :: Char -> Stecker -> Char
  steckerMatch c (x:xs)
    | fst x == c = snd x
    | snd x == c = fst x
    | otherwise = steckerMatch c xs
  steckerMatch c [] = c
  -- > steckerMatch 'A' [('F','T'),('D','U'),('V','A'),('K','W'),('H','Z'),('I','X')]
  -- > steckerMatch 'C' [('F','T'),('D','U'),('V','A'),('K','W'),('H','Z'),('I','X')]  

{- Part 2: Finding the Longest Menu -}

  crib1 = "WETTERVORHERSAGEBISKAYA"
  message1 = "RWIVTYRESXBFOGKUHQBAISE"

  type Menu = [Int] -- the supplied type is not correct; fix it!
  type Crib = [(Char,Char)] -- the supplied type is not correct; fix it!

  --longestMenu :: Crib -> Menu
  --longestMenu all@(x:xs) = search (snd x) all

  combine :: [Int] -> [Int] -> [[Int]]
  combine ori [] = []
  combine ori (x:xs) = [ori++[x]] ++ combine ori xs
  -- > combine [0,2] [5,8,11]
  -- [[0,2,5],[0,2,8],[0,2,11]]


  searchPos' :: Int -> Crib -> [Int]
  searchPos' i crib = elemIndices num (map fst crib)
    where num = head(drop i (map snd crib))
  -- > searchPos' 0 (zip crib1 message1)
  -- [5,8,11]

  lengthList :: [[Int]] -> [Int]
  lengthList [] = []
  lengthList (x:xs)
    | length (elemIndices (head x) x) /= 1 = [head(tail (elemIndices (head x) x))] ++ lengthList xs
    | otherwise = lengthList xs
  --lengthList xs = lengthList --elemIndices (maximum lengthList) lengthList
  --  where lengthList = [head(tail (elemIndices (head x) x)) - head(elemIndices (head x) x)| x<-xs]
  -- > lengthList (searchEach [[0]] (zip crib1 message1))
  -- > lengthList [[0,2,1,0],[1,2,3,4,1]]

  searchEach :: [[Int]] -> Crib -> [[Int]]
  searchEach all@(x:xs) crib
    | length (last [x]) == 20 = []
    | otherwise = (eachSearch x crib) ++ searchEach (xs++eachSearch x crib) crib
  -- > searchEach [[0]] (zip crib1 message1)

  eachSearch :: [Int] -> Crib -> [[Int]]
  eachSearch [] crib = []
  eachSearch x crib = combine x (searchPos' (last x) crib)
  -- > eachSearch [[0]] (zip crib1 message1)
  -- > eachSearch [[0,5],[0,8],[0,11]] (zip crib1 message1)
  -- > eachSearch [[0,8],[0,11],[0,5,21]] (zip crib1 message1)
  -- > eachSearch [[0,11],[0,5,21],[0,8,12],[0,8,18]] (zip crib1 message1)
  -- > eachSearch [[0,5,21],[0,8,12],[0,8,18],[]] (zip crib1 message1)
  -- > eachSearch [[0,5,21],[0,8,12],[0,8,18]] (zip crib1 message1)
  -- > eachSearch [[0,8,12],[0,8,18],[0,5,21,12],[0,5,21,18]] (zip crib1 message1)
  -- > eachSearch [[0,8,18],[0,5,21,12],[0,5,21,18],[0,8,12,7]] (zip crib1 message1)
  -- > eachSearch [[0,5,21,12],[0,5,21,18],[0,8,12,7],[0,8,18,16]] (zip crib1 message1)
  -- > eachSearch [[0,5,21,18],[0,8,12,7],[0,8,18,16],[0,5,21,12,7]] (zip crib1 message1)
  -- > eachSearch [[0,8,12,7],[0,8,18,16],[0,5,21,12,7],[0,5,21,18,16]] (zip crib1 message1)
  -- > eachSearch [[0,8,18,16],[0,5,21,12,7],[0,5,21,18,16],[0,8,12,7,1],[0,8,12,7,4],[0,8,12,7,10],[0,8,12,7,15]] (zip crib1 message1)
  

  --verify :: [[Int]] -> [[Int]]
  --verify (x:xs)
  --  | [last x] == 

  appeared :: Int -> [[Int]] -> [[Int]]
  appeared len [] = []
  appeared len (x:xs)
    | len > (length x + 1) = appeared len xs
    | otherwise = [x] ++ appeared len xs
  -- > appeared 5 [[0,5],[0,8],[0,11],[0,5,21],[0,8,12],[0,8,18],[0,5,21,12],[0,5,21,18],[0,8,12,7],[0,8,18,16],[0,5,21,12,7],[0,5,21,18,16],[0,8,12,7,1],[0,8,12,7,4],[0,8,12,7,10],[0,8,12,7,15]]
  -- > appeared 5 [[0,5,4],[0,8],[0,11],[0]]

  
  -- > last (head [[0,5],[0,6]])

  -- > searchPos' 0 (zip crib1 message1)
  -- > snd(searchPos' 1 (zip crib1 message1))

  searchPos :: Char -> Crib -> (Char,[Int])
  searchPos c crib = (c,elemIndices c (map fst crib))
  -- > searchPos 'R' (zip crib1 message1)
  -- ('R',[5,8,11])

  --searchRoute :: Crib -> Crib -> [String] -> [String]
  --searchRoute (x:xs) crib [] = searchNext [x] crib []

  --searchNext :: [(Char,Char)] -> Crib -> [String] -> [String]
  --searchNext [] _ _ = []
  --searchNext (plain,cipher) crib [] = 

  --searchNext x crib str = searchNext ((cribPair (last [snd x]) cipherSin) : xs) crib zipStr  -- ++ searchRoute xs crib
  --  where cipherSin = cipherChar (last[snd x]) crib
  --        zipStr = zip' [fst x] cipherSin
  -- > searchNext [('W','R')] (zip crib1 message1)
  -- > searchNext [('Y','S'),('S','O'),('S','B')] (zip crib1 message1)

  {- input a cipher Char and return the match plain position -}


  cribPair :: Char -> [Char] -> [(Char,Char)]
  cribPair c [] = []
  cribPair c (x:xs) = [(c,x)] ++ cribPair c xs
  -- > cribPair 'R' "YSF"
  -- [('R','Y'),('R','S'),('R','F')]


  {- input a cipher Char and return the match plain position -}
  cipherChar :: Char -> Crib -> [Char]
  cipherChar c [] = []
  cipherChar c (x:xs)
    | c == fst x = snd x:cipherChar c xs
    | otherwise = cipherChar c xs
  -- > cipherChar 'R' (zip crib1 message1)
  -- "YSF"

  {- combine the route -}
  zip' :: [Char] -> [Char] ->[String]
  zip' _ [] = []
  zip' ori (x:xs) = (ori ++ [x]) : zip' ori xs
  -- > zip' "ABC" "QWERT"
  -- > zip' "W" (cipherChar 'R' (zip crib1 message1))
  -- ["WY","WS","WF"]

  getLast :: [String] -> [Char]
  getLast [] = []
  getLast (x:xs) = [last x] ++ getLast xs



  --searchCipher :: Char -> [(Char,Char)] -> Char
  --searchCipher c (x:xs) = 

  minLength :: String -> String -> Int
  minLength plain cipher = min (length plain) (length cipher)




{- Part 3: Simulating the Bombe -}
  
  breakEnigma :: Crib -> Maybe (Offsets, Stecker)
  breakEnigma _ = Nothing

{- Useful definitions and functions -}

   -- substitution cyphers for the Enigma rotors
   -- as pairs of (wirings, knock-on position)
   -- knock-on position is where it will cause the next left wheel to
   -- advance when it moves past this position
 
        --"ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  rotor1=("EKMFLGDQVZNTOWYHXUSPAIBRCJ",17::Int)
  rotor2=("AJDKSIRUXBLHWTMCQGZNPYFVOE",5::Int)
  rotor3=("BDFHJLCPRTXVZNYEIWGAKMUSQO",22::Int)
  rotor4=("ESOVPZJAYQUIRHXLNFTGKDCMWB",10::Int)
  rotor5=("VZBRGITYUPSDNHLXAWMJQOFECK",0::Int)

  -- > "AAAAAAA" -> "GVURPWX"

  {- the standard Enigma reflector (Reflector B)
    swapped A<->Y, B<->R, C<->U,D<->H, E<->Q, F<->S, G<->L, 
            I<->P, J<->X, K<->N, M<->O, T<->Z,V<->W
  -}
  reflectorB= [('A','Y'),
              ('B','R'),
              ('C','U'),
              ('D','H'),
              ('E','Q'),
              ('F','S'),
              ('G','L'),
              ('I','P'),
              ('J','X'),
              ('K','N'),
              ('M','O'),
              ('T','Z'),
              ('V','W')]

  {- alphaPos: given an uppercase letter, returns its index in the alphabet
     ('A' = position 0; 'Z' = position 25)
   -}
  alphaPos :: Char -> Int
  alphaPos c = (ord c) - ord 'A'

  int2let :: Int -> Char
  int2let n = chr (ord 'A' + n)

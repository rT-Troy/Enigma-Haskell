{-- COM2108: Functional Programming – 2022
 -- Functional Programming Design Case Study
 -- Author: Jun Zhang
 -- Date: 12/4/2022
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

  encodeMessage :: String -> Enigma -> String
  encodeMessage xs (SimpleEnigma rotorR rotorM rotorL reflectorB offsets) =
    encodeMessage' (normalize xs) (SimpleEnigma (head startRotor) (startRotor!!1) (last startRotor) reflectorB (moveMatch offsets rotorR rotorM rotorL))
      where startRotor = cur3Rotor (0,0,0) (moveMatch offsets rotorR rotorM rotorL) rotorR rotorM rotorL

  encodeMessage xs (SteckeredEnigma rotorR rotorM rotorL reflectorB offsets steckers) =
    encodeMessage' (normalize xs) (SteckeredEnigma (head startRotor) (startRotor!!1) (last startRotor) reflectorB (moveMatch offsets rotorR rotorM rotorL) steckers)
      where startRotor = cur3Rotor (0,0,0) (moveMatch offsets rotorR rotorM rotorL) rotorR rotorM rotorL
  -- > encodeMessage "Here is a test input string." (SimpleEnigma rotor1 rotor2 rotor3 reflectorB (0,0,25))
  -- > encodeMessage "Here is a test input string." (SteckeredEnigma rotor1 rotor2 rotor3 reflectorB (0,0,25) [('F','T'),('D','U'),('V','A'),('K','W'),('H','Z'),('I','X')])
  -- > encodeMessage "ALICE" (SimpleEnigma rotor1 rotor2 rotor3 reflectorB (0,0,25))
  -- > encodeMessage "ALICE" (SimpleEnigma rotor1 rotor2 rotor3 reflectorB (0,1,1))
  -- > encodeMessage "NIQVD" (SimpleEnigma rotor1 rotor2 rotor3 reflectorB (0,0,25))
  -- > "NIQVD" -> "ALICE"

  encodeMessage' :: String -> Enigma -> String
  encodeMessage' [] _ = []
  encodeMessage' (x:xs) (SimpleEnigma rotorR rotorM rotorL reflectorB (l,m,r)) = 
    encode'(encode' (encode' (reflector (encode (encode (encode x rotorR r) rotorM m) rotorL l) reflectorB) rotorL l) rotorM m) rotorR r : encodeMessage' xs (SimpleEnigma (head nextRotorList) (nextRotorList!!1) (last nextRotorList) reflectorB (moveMatch (l,m,r) rotorR rotorM rotorL))
      where nextRotorList = cur3Rotor (l,m,r) (moveMatch (l,m,r) rotorR rotorM rotorL) rotorR rotorM rotorL
  encodeMessage' (x:xs) (SteckeredEnigma rotorR rotorM rotorL reflectorB (l,m,r) steckers) = 
   steckerMatch (encode'(encode' (encode' (reflector (encode (encode (encode (steckerMatch x steckers) rotorR r) rotorM m) rotorL l) reflectorB) rotorL l) rotorM m) rotorR r) steckers : encodeMessage' xs (SteckeredEnigma (head nextRotorList) (nextRotorList!!1) (last nextRotorList) reflectorB (moveMatch (l,m,r) rotorR rotorM rotorL) steckers)
     where nextRotorList = cur3Rotor (l,m,r) (moveMatch (l,m,r) rotorR rotorM rotorL) rotorR rotorM rotorL
  -- > encodeMessage' "ALICE" (SimpleEnigma rotor1 rotor2 rotor3 reflectorB (0,0,0))
  -- > encodeMessage' "NIQVD" (SimpleEnigma rotor1 rotor2 rotor3 reflectorB (0,0,0))

  {- makesure only uppercase letter would be inputed -}
  normalize :: String -> String
  normalize [] = []
  normalize (x:xs) 
    | isUpper x = x:normalize xs
    | isLower x = toUpper x:normalize xs
    | otherwise = normalize xs
  -- > normalize "STdife@o12,"

  {- encode from RR -> MR -> LR -> reflector -}
  encode :: Char -> Rotor -> Int -> Char
  encode c rotor pos =  int2Char (head (elemIndices (fst rotor !! max 0 (alphaPos c)) (drop pos ['A'..'Z'] ++ take pos ['A'..'Z'])))
  -- > encode 'A' ("EKMFLGDQVZNTOWYHXUSPAIBRCJ",5::Int) 0
  -- > encode 'A' ("KMFLGDQVZNTOWYHXUSPAIBRCE",5::Int) 1

  {- encode from reflector -> LR -> MR -> RR -}
  encode' :: Char -> Rotor -> Int -> Char
  encode' c rotor pos = int2Char (head(elemIndices input (fst rotor))) 
    where input = (drop pos ['A' .. 'Z'] ++ take pos ['A' .. 'Z']) !! max 0 (alphaPos c)
  -- > encode' 'I' ("BDFHJLCPRTXVZNYEIWGAKMUSQO",22::Int) 0
  -- > encode' 'I' ("DFHJLCPRTXVZNYEIWGAKMUSQOB",22::Int) 1

  int2Char :: Int -> Char
  int2Char n = chr (ord 'A' + n)
  -- > int2Char 0

  {- offsets of next step according to knock-on position -}
  moveMatch :: (Int,Int,Int) -> Rotor -> Rotor -> Rotor -> (Int,Int,Int)
  moveMatch (x,y,z) rotorR rotorM rotorL
    | snd rotorM == y && snd rotorR == z = ((x+1)`rem`26,(y+1)`rem`26,(z+1)`rem`26)
    | snd rotorM /= y && snd rotorR == z = (x`rem`26,(y+1)`rem`26,(z+1)`rem`26)
    | otherwise = (x`rem`26,y`rem`26,(z+1)`rem`26)
  -- > moveMatch (0,25,25) rotor1 rotor2 rotor3
  -- > moveMatch (0,0,17) rotor1 rotor2 rotor3
  -- > moveMatch (0,5,17) rotor1 rotor2 rotor3

  {- the rotor state changed and stored as list by input current offsets and successor offsets -}
  cur3Rotor :: (Int,Int,Int) -> (Int,Int,Int) -> Rotor -> Rotor -> Rotor -> [Rotor]
  cur3Rotor (ox,oy,oz) (x,y,z) rotorR rotorM rotorL = 
    [(drop (if z-oz == (-25) then 1 else z-oz) (fst rotorR) ++ take (if z-oz == (-25) then 1 else z-oz) (fst rotorR), snd rotorR),
     (drop (if y-oy == (-25) then 1 else y-oy) (fst rotorM) ++ take (if y-oy == (-25) then 1 else y-oy) (fst rotorM), snd rotorM),
     (drop (if x-ox == (-25) then 1 else x-ox) (fst rotorL) ++ take (if x-ox == (-25) then 1 else x-ox) (fst rotorL), snd rotorL)]
  -- > cur3Rotor (0,0,0) (0,0,1) rotor1 rotor2 rotor3
  -- > cur3Rotor (0,5,17) (1,6,18) rotor1 rotor2 rotor3

  reflector :: Char -> Reflector -> Char
  reflector c (x:xs) 
    | fst x == c = snd x
    | snd x == c = fst x
    | otherwise = reflector c xs
  -- > reflector 'A' reflectorB
  -- > reflector 'P' reflectorB

  steckerMatch :: Char -> Stecker -> Char
  steckerMatch c (x:xs)
    | fst x == c = snd x
    | snd x == c = fst x
    | otherwise = steckerMatch c xs
  steckerMatch c [] = c
  -- > steckerMatch 'A' [('F','T'),('D','U'),('V','A'),('K','W'),('H','Z'),('I','X')]
  -- > steckerMatch 'C' [('F','T'),('D','U'),('V','A'),('K','W'),('H','Z'),('I','X')]  

{- Part 2: Finding the Longest Menu -}

  {- test data -}
  cribData :: [(Char,Char)]
  cribData = [('W','R'),('E','W'),('T','I'),('T','V'),('E','T'),('R','Y'),('V','R'),('O','E'),('R','S'),('H','X'),('E','B'),('R','F'),('S','O'),('A','G'),('G','K'),('E','U'),('B','H'),('I','Q'),('S','B'),('K','A'),('A','I'),('Y','S'),('A','E')]

  type Menu = [Int] -- the supplied type is not correct; fix it!
  type Crib = [(Char,Char)]

  {- task 2 main -}
  longestMenu :: Crib -> Menu
  longestMenu [] = []
  longestMenu crib = allInOneMenu (multiMenu [0..len] crib)
    where len = length crib - 1
  -- > longestMenu cribData

  {- get one longest menu of multiple starting position -}
  allInOneMenu :: [(Menu,Int)] -> Menu
  allInOneMenu [] = []
  allInOneMenu all@(x:xs)
    | snd x == longest = fst x
    | otherwise = allInOneMenu xs
      where longest = maximum (map snd all)
  -- > allInOneMenu (multiMenu [0..5] cribData)
  -- > allInOneMenu (multiMenu [9] cribData)

  {-  get a pair of menu and its length by different input positions -}
  multiMenu :: [Int] -> Crib -> [(Menu,Int)]
  multiMenu [] _ = []
  multiMenu (x:xs) crib = (longMenu, length longMenu) : multiMenu xs crib
    where longMenu = chooseOne (longSucc (length (last (possMenu [[x]] crib))) (possMenu [[x]] crib))
  -- > multiMenu [0..5] cribData
  -- > multiMenu [9] cribData
  
  {- get a longest menu by position input -}
  --longMenu :: Int -> Crib -> Menu
  --longMenu x crib = chooseOne (longSucc (length (last (possMenu [[x]] crib))) (possMenu [[x]] crib))
  -- > longMenu 0 cribData

  {- choose one longest menu or null if not exit -}
  chooseOne :: [Menu] -> Menu
  chooseOne [] = []
  chooseOne (x:xs)
    | length (elemIndices (last x) x) == 1 = x
    | otherwise = chooseOne xs
  -- > chooseOne (longSucc (length (last (possMenu [[0]] cribData))) (possMenu [[0]] cribData))

  {- queue for all possible menu by input start position -}
  possMenu :: [[Int]] -> Crib -> [Menu]
  possMenu [] _ = []
  possMenu all@(x:xs) crib = nextMenu x crib ++ possMenu (xs++nextMenu x crib) crib
  -- > possMenu [[0]] cribData
  -- > possMenu [[9]] cribData

  {- get nested list of all possible next successors and make it menu -}
  nextMenu :: [Int] -> Crib -> [Menu]
  nextMenu [] crib = []
  nextMenu x crib = combine x (succFinder (last x) crib)
  -- > nextMenu [0] cribData
  -- > nextMenu [9] cribData

  {- get successors' position by input current cipher's position -}
  succFinder :: Int -> Crib -> [Int]
  succFinder i crib = elemIndices num (map fst crib)
    where num = map snd crib !! max 0 i
  -- > succFinder 0 cribData
  -- > succFinder 9 cribData

  {- combine original menu with its possible successors and return nested list -}
  combine :: Menu -> [Int] -> [Menu]
  combine ori [] = []
  combine ori (x:xs)
    | x `elem` ori = combine ori xs
    | otherwise = (ori ++ [x]) : combine ori xs
  -- > combine [0,2] [5,8,11]

  {- get longest successors of nested list of menu starting from a specific position -}
  longSucc :: Int -> [Menu] -> [Menu]
  longSucc len [] = []
  longSucc len (x:xs)
    | len > length x = longSucc len xs
    | otherwise = x : longSucc len xs
  -- > longSucc 3 [[0,5,4],[0,8],[0,11],[0]]
  -- > longSucc 2 [[0,5,4],[0,8],[0,11],[0]]

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


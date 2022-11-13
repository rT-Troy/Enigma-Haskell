{-- Stub for the grading assignment. Fill it in, making sure you use good
 -- functional style, and add comments (including replacing those that are
 -- already here).
--}

module Enigma where
  import Data.Char  -- to use functions on characters
  import Data.Maybe -- breakEnigma uses Maybe type
  -- add extra imports if needed, but only standard library functions!

{- Part 1: Simulation of the Enigma -}

  type Rotor = (String,Int)
  type Reflector = [(Char,Char)] -- the supplied type is not correct; fix it!
  type Offsets = (Int,Int,Int) -- the supplied type is not correct; fix it!
  type Stecker = Bool -- the supplied type is not correct; fix it!
  
  data Enigma = SimpleEnigma Rotor Rotor Rotor Reflector Offsets
                | SteckeredEnigma Rotor Rotor Rotor Reflector Offsets Stecker

  encodeMessage :: String -> Enigma -> String
  encodeMessage [] _ = []
  encodeMessage (x:xs) (SimpleEnigma rotorR rotorM rotorL reflectorB offsets) = 
    [encode (encode (encode x rotorR) rotorM) rotorL] ++ encodeMessage xs (SimpleEnigma (head nextRotorList) (head (tail nextRotorList)) (last nextRotorList) reflectorB nextOffsets)
      where nextOffsets = moveMatch (moveStep (if offsets /= (0,0,25) then offsets else moveStep offsets)) rotorR rotorM rotorL
            nextRotorList = cur3Rotor offsets nextOffsets rotorR rotorM rotorL
  -- > encodeMessage "ABC" (SimpleEnigma rotor1 rotor2 rotor3 reflectorB (0,0,25))
  -- > encodeMessage "AAAAAAAAAA" (SimpleEnigma rotor1 rotor2 rotor3 reflectorB (0,0,25))
  -- > "AAAAAAA" -> "GVURPWX"


  {- You will need to add many more functions. Remember, design it carefully
   - and keep it simple! If things are feeling complicated, step back from your
   - code and think about the design again.
   -}

  {- Test -}



  int2let :: Int -> Char
  int2let n = chr (ord 'A' + n)

  {- makesure only uppercase letter input -}
  normalize :: String -> String
  normalize [] = []
  normalize (x:xs) 
    | isUpper x = (toUpper x):normalize xs
    | otherwise = normalize xs
  -- > normalize "STdife@o12,"

  {- offset 1 step for one input -}
  offset :: Rotor -> Rotor
  offset rotor = (tail(rotorStr rotor) ++ [head(rotorStr rotor)], snd(rotor))
  -- > offset ("EKMFLGDQVZNTOWYHXUSPAIBRCJ",17::Int)
  -- > offset (offset ("EKMFLGDQVZNTOWYHXUSPAIBRCJ",17::Int))

  moveStep :: (Int,Int,Int) -> (Int,Int,Int)
  moveStep (x,y,z) = (x `rem` 26,y `rem` 26,(z+1)`rem` 26)
  -- > moveStep (0,25,25)
  -- > moveStep (0,24,25)

  {- offsets of next step according to knock-on position -}
  moveMatch :: (Int,Int,Int) -> Rotor -> Rotor -> Rotor -> (Int,Int,Int)
  moveMatch (x,y,z) rotorR rotorM rotorL
    | y == rotorInt rotorM && rotorInt rotorR == (z-1) = (x+1,y+1,z)
    | rotorInt rotorR == (z-1) = (x,y+1,z)
    | otherwise = (x,y,z)
  -- > moveMatch (moveStep (0,0,25)) rotor3 rotor2 rotor1
  -- > moveMatch (moveStep (0,0,17)) rotor3 rotor2 rotor1
  -- > moveMatch (moveStep (0,5,17)) rotor3 rotor2 rotor1
  -- > moveMatch (moveStep (if (0,0,25) == (0,0,25) then moveStep (0,0,25) else (0,0,25))) rotor3 rotor2 rotor1
  -- > moveMatch (moveStep (if (0,0,1) == (0,0,25) then moveStep (0,0,1) else (0,0,1))) rotor3 rotor2 rotor1

  --moveRotor :: [Int] -> Rotor -> Rotor -> Rotor -> Rotor -> Rotor -> Rotor
  --moveRotor [x,y,z] rotorL rotorM rotorR = rotorL


  {- set current state of rotor by every Char insert -}
  curRotor :: Int -> Rotor -> Rotor
  curRotor int rotor = (drop int (rotorStr rotor) ++ (take int (rotorStr rotor)), rotorInt rotor)
  -- > curRotor 3 rotor1

  cur3Rotor :: (Int,Int,Int) -> (Int,Int,Int) -> Rotor -> Rotor -> Rotor -> [Rotor]
  cur3Rotor (ix,iy,iz) (x,y,z) rotorR rotorM rotorL =[(drop (z-iz`rem`25) (rotorStr rotorR) ++ (take (z-iz`rem`25) (rotorStr rotorR)), rotorInt rotorR),
                                                      (drop (y-iy`rem`25) (rotorStr rotorM) ++ (take (y-iy`rem`25) (rotorStr rotorM)), rotorInt rotorM),
                                                      (drop (x-ix`rem`25) (rotorStr rotorL) ++ (take (x-ix`rem`25) (rotorStr rotorL)), rotorInt rotorL)]
  -- > cur3Rotor (2,1,25) (2,1,0) rotor1 rotor2 rotor3
  -- > cur3Rotor (2,1,0) (2,1,1) rotor1 rotor2 rotor3


  reflector :: Char -> [(Char,Char)] -> Char
  reflector c [] = c
  reflector c (x:xs) 
    | fst x == c = snd x
    | snd x == c = fst x
    | otherwise = reflector c xs
  -- > reflector 'T' reflectorB


  {- encode char to char  by a rotor -}
  encode :: Char -> Rotor -> Char
  encode c rotor = head(drop m (rotorStr rotor))
    where m = alphaPos c
  -- > encode 'A' ("EKMFLGDQVZNTOWYHXUSPAIBRCJ",17::Int)
  -- > encode 'Z' ("EKMFLGDQVZNTOWYHXUSPAIBRCJ",17::Int)

  charToString :: Char -> String
  charToString c = [c]

  {- encode String to String by a rotor -}
  encodeStr :: String -> Rotor -> String
  encodeStr [] rotor= []
  encodeStr (x:xs) rotor = (encode x rotor):(encodeStr xs (offset rotor))
  -- > encodeStr "ABCDE" ("EKMFLGDQVZNTOWYHXUSPAIBRCJ",17::Int)
  -- > encodeStr "AAZAZ" ("EKMFLGDQVZNTOWYHXUSPAIBRCJ",17::Int)

  {- get rotor wirings -}
  rotorStr :: Rotor -> String
  rotorStr rotor = fst(rotor)

  {- get rotor knock-on position -}
  rotorInt :: Rotor -> Int
  rotorInt rotor = snd(rotor)
  
{- Part 2: Finding the Longest Menu -}

  type Menu = Bool -- the supplied type is not correct; fix it!
  type Crib = Bool -- the supplied type is not correct; fix it!

  longestMenu :: Crib -> Menu
  longestMenu _ = False

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


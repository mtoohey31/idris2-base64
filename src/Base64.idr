module Base64

import Data.Bits
import Data.String

-- TODO: Do conversions with a static array lookups. This hasn't been done yet
-- because Data.IOArray requires IO, and contrib's Data.Linear.Array requires
-- runtime initialization. For the time being, massive case statements are used
-- because an optimized backend should reduce these to static array lookups
-- (Rust does with `-C opt-level=3`), though explicitly writing it as array
-- lookups would be preferable since it would be more readable and we wouldn't
-- have to hope the backend will optimize things correctly.

-- TODO: Require proof of bitwidth 6 or less to avoid partiality.

||| Convert a value of bitwidth 6 or less to the corresponding encoded
||| character.
partial
bits6ToChar : Bits8 -> Char
bits6ToChar i = case i of
  0 => 'A'
  1 => 'B'
  2 => 'C'
  3 => 'D'
  4 => 'E'
  5 => 'F'
  6 => 'G'
  7 => 'H'
  8 => 'I'
  9 => 'J'
  10 => 'K'
  11 => 'L'
  12 => 'M'
  13 => 'N'
  14 => 'O'
  15 => 'P'
  16 => 'Q'
  17 => 'R'
  18 => 'S'
  19 => 'T'
  20 => 'U'
  21 => 'V'
  22 => 'W'
  23 => 'X'
  24 => 'Y'
  25 => 'Z'
  26 => 'a'
  27 => 'b'
  28 => 'c'
  29 => 'd'
  30 => 'e'
  31 => 'f'
  32 => 'g'
  33 => 'h'
  34 => 'i'
  35 => 'j'
  36 => 'k'
  37 => 'l'
  38 => 'm'
  39 => 'n'
  40 => 'o'
  41 => 'p'
  42 => 'q'
  43 => 'r'
  44 => 's'
  45 => 't'
  46 => 'u'
  47 => 'v'
  48 => 'w'
  49 => 'x'
  50 => 'y'
  51 => 'z'
  52 => '0'
  53 => '1'
  54 => '2'
  55 => '3'
  56 => '4'
  57 => '5'
  58 => '6'
  59 => '7'
  60 => '8'
  61 => '9'
  62 => '+'
  63 => '/'

export
btoa' : List Bits8 -> List Char
btoa' (a :: b :: c :: xs) = let x1 = a `prim__shr_Bits8` 2
                                x2 = ((a .&. 3) `prim__shl_Bits8` 4) .|. (b `prim__shr_Bits8` 4)
                                x3 = ((b .&. 15) `prim__shl_Bits8` 2) .|. (c `prim__shr_Bits8` 6)
                                x4 = c .&. 63
                                enc = assert_total $ map bits6ToChar [x1, x2, x3, x4]
                            in enc ++ btoa' xs
btoa' (a :: b :: []) = let x1 = a `prim__shr_Bits8` 2
                           x2 = ((a .&. 3) `prim__shl_Bits8` 4) .|. (b `prim__shr_Bits8` 4)
                           x3 = (b .&. 15) `prim__shl_Bits8` 2
                       in (assert_total $ map bits6ToChar [x1, x2, x3]) ++ ['=']
btoa' (a :: []) = let x1 = a `prim__shr_Bits8` 2
                      x2 = (a .&. 3) `prim__shl_Bits8` 4
                  in (assert_total $ map bits6ToChar [x1, x2]) ++ ['=', '=']
btoa' [] = []

export
btoa : List Bits8 -> String
btoa = fastPack . btoa'

-- TODO: Require proof that the character is a valid character to avoid
-- partiality.

-- ||| Convert an encoded character to a value of bitwidth 6 or less.
-- partial
-- charToBits6 : Char -> Bits8
-- charToBits6 c = case c of
--   'A' => 0
--   'B' => 1
--   'C' => 2
--   'D' => 3
--   'E' => 4
--   'F' => 5
--   'G' => 6
--   'H' => 7
--   'I' => 8
--   'J' => 9
--   'K' => 10
--   'L' => 11
--   'M' => 12
--   'N' => 13
--   'O' => 14
--   'P' => 15
--   'Q' => 16
--   'R' => 17
--   'S' => 18
--   'T' => 19
--   'U' => 20
--   'V' => 21
--   'W' => 22
--   'X' => 23
--   'Y' => 24
--   'Z' => 25
--   'a' => 26
--   'b' => 27
--   'c' => 28
--   'd' => 29
--   'e' => 30
--   'f' => 31
--   'g' => 32
--   'h' => 33
--   'i' => 34
--   'j' => 35
--   'k' => 36
--   'l' => 37
--   'm' => 38
--   'n' => 39
--   'o' => 40
--   'p' => 41
--   'q' => 42
--   'r' => 43
--   's' => 44
--   't' => 45
--   'u' => 46
--   'v' => 47
--   'w' => 48
--   'x' => 49
--   'y' => 50
--   'z' => 51
--   '0' => 52
--   '1' => 53
--   '2' => 54
--   '3' => 55
--   '4' => 56
--   '5' => 57
--   '6' => 58
--   '7' => 59
--   '8' => 60
--   '9' => 61
--   '+' => 62
--   '/' => 63

-- TODO: Require proof that the string is well-formed base64 data.

-- atob' : List Char -> List Bits8
-- atob' (a :: b :: c :: xs) = []
-- atob' (a :: b :: []) = []
-- atob' (a :: []) = []
-- atob' [] = []

-- atob : String -> List Bits8
-- atob = atob' . fastUnpack

||| Errors that can be encountered 
export
data Base64Error : Type
data Base64Error = InvalidChar Char

export
Eq Base64Error where
  InvalidChar c1 == InvalidChar c2 = c1 == c2

export
Show Base64Error where
  show (InvalidChar c) = "Invalid base64 character: " ++ singleton c

||| Convert an encoded character to a value of bitwidth 6 or less.
partial
tryCharToBits6 : Char -> Either (Maybe Bits8) Base64Error
tryCharToBits6 c = case c of
  'A' => Left $ Just 0
  'B' => Left $ Just 1
  'C' => Left $ Just 2
  'D' => Left $ Just 3
  'E' => Left $ Just 4
  'F' => Left $ Just 5
  'G' => Left $ Just 6
  'H' => Left $ Just 7
  'I' => Left $ Just 8
  'J' => Left $ Just 9
  'K' => Left $ Just 10
  'L' => Left $ Just 11
  'M' => Left $ Just 12
  'N' => Left $ Just 13
  'O' => Left $ Just 14
  'P' => Left $ Just 15
  'Q' => Left $ Just 16
  'R' => Left $ Just 17
  'S' => Left $ Just 18
  'T' => Left $ Just 19
  'U' => Left $ Just 20
  'V' => Left $ Just 21
  'W' => Left $ Just 22
  'X' => Left $ Just 23
  'Y' => Left $ Just 24
  'Z' => Left $ Just 25
  'a' => Left $ Just 26
  'b' => Left $ Just 27
  'c' => Left $ Just 28
  'd' => Left $ Just 29
  'e' => Left $ Just 30
  'f' => Left $ Just 31
  'g' => Left $ Just 32
  'h' => Left $ Just 33
  'i' => Left $ Just 34
  'j' => Left $ Just 35
  'k' => Left $ Just 36
  'l' => Left $ Just 37
  'm' => Left $ Just 38
  'n' => Left $ Just 39
  'o' => Left $ Just 40
  'p' => Left $ Just 41
  'q' => Left $ Just 42
  'r' => Left $ Just 43
  's' => Left $ Just 44
  't' => Left $ Just 45
  'u' => Left $ Just 46
  'v' => Left $ Just 47
  'w' => Left $ Just 48
  'x' => Left $ Just 49
  'y' => Left $ Just 50
  'z' => Left $ Just 51
  '0' => Left $ Just 52
  '1' => Left $ Just 53
  '2' => Left $ Just 54
  '3' => Left $ Just 55
  '4' => Left $ Just 56
  '5' => Left $ Just 57
  '6' => Left $ Just 58
  '7' => Left $ Just 59
  '8' => Left $ Just 60
  '9' => Left $ Just 61
  '+' => Left $ Just 62
  '/' => Left $ Just 63
  '=' => Left $ Nothing
  i => Right $ InvalidChar i

-- TODO: is this case valid at all?

makeOne : Bits8 -> List Bits8
makeOne x1 = [x1 `prim__shr_Bits8` 2]

makeTwo : Bits8 -> Bits8 -> List Bits8
makeTwo x1 x2 = [(x1 `prim__shl_Bits8` 2) .|.
                   (x2 `prim__shr_Bits8` 4)]

makeThree : Bits8 -> Bits8 -> Bits8 -> List Bits8
makeThree x1 x2 x3 = [(x1 `prim__shl_Bits8` 2) .|.
                        (x2 `prim__shr_Bits8` 4),
                      ((x2 .&. 15) `prim__shl_Bits8` 4) .|.
                         (x3 `prim__shr_Bits8` 2)]

makeFour : Bits8 -> Bits8 -> Bits8 -> Bits8 -> List Bits8
makeFour x1 x2 x3 x4 = [(x1 `prim__shl_Bits8` 2) .|.
                          (x2 `prim__shr_Bits8` 4),
                        ((x2 .&. 15) `prim__shl_Bits8` 4) .|.
                          (x3 `prim__shr_Bits8` 2),
                        ((x3 .&. 3) `prim__shl_Bits8` 6) .|. x4]

export
tryAtob' : List Char -> Either (List Bits8) Base64Error
tryAtob' (a :: b :: c :: d :: xs) = case tryCharToBits6 a of
  Left (Just x1) => case tryCharToBits6 b of
    Left (Just x2) => case tryCharToBits6 c of
      Left (Just x3) => case tryCharToBits6 d of
        Left (Just x4) => case tryAtob' xs of
          Left ys => Left $ makeFour x1 x2 x3 x4 ++ ys
          Right e => Right e
        Left Nothing => Left $ makeThree x1 x2 x3
        Right e => Right e
      Left Nothing => Left $ makeTwo x1 x2
      Right e => Right e
    Left Nothing => Left $ makeOne x1
    Right e => Right e
  Left Nothing => Left []
  Right e => Right e
tryAtob' (a :: b :: c :: []) = case tryCharToBits6 a of
  Left (Just x1) => case tryCharToBits6 b of
    Left (Just x2) => case tryCharToBits6 c of
      Left (Just x3) => Left $ makeThree x1 x2 x3
      Left Nothing => Left $ makeTwo x1 x2
      Right e => Right e
    Left Nothing => Left $ makeOne x1
    Right e => Right e
  Left Nothing => Left []
  Right e => Right e
tryAtob' (a :: b :: []) = case tryCharToBits6 a of
  Left (Just x1) => case tryCharToBits6 b of
    Left (Just x2) => Left $ makeTwo x1 x2
    Left Nothing => Left $ makeOne x1
    Right e => Right e
  Left Nothing => Left []
  Right e => Right e
tryAtob' (a :: []) = case tryCharToBits6 a of
  Left (Just x1) => Left $ makeOne x1
  Left Nothing => Left []
  Right e => Right e
tryAtob' [] = Left []

export
tryAtob : String -> Either (List Bits8) Base64Error
tryAtob = tryAtob' . fastUnpack

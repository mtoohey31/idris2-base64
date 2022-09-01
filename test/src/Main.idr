module Main

import Base64
import Hedgehog

bytesGen : Gen (List Bits8)
bytesGen = list (linear 0 100) $ bits8 (linear 0 255)

propBytes : Property
propBytes = property $ do b <- forAll bytesGen
                          Left b === tryAtob (btoa b)

propInvalidCharError : Property
propInvalidCharError = withTests 1 . property $ Right (InvalidChar '%') === tryAtob "AA%A"

propInvalidLengthError : Property
propInvalidLengthError = withTests 1 . property $ Right InvalidLength === tryAtob "AAAAA"

main : IO ()
main = test . pure $ MkGroup "Base64" [
  ("propBytes", propBytes),
  ("propInvalidCharError", propInvalidCharError),
  ("propInvalidLengthError", propInvalidLengthError)
]

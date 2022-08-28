module Main

import Base64
import Hedgehog

bytesGen : Gen (List Bits8)
bytesGen = list (linear 0 100) $ bits8 (linear 0 255)

propBytes : Property
propBytes = property $ do b <- forAll bytesGen
                          Left b === tryAtob (btoa b)

main : IO ()
main = test . pure $ MkGroup "Base64" [
  ("propBytes", propBytes)
]

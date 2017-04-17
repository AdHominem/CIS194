{-# OPTIONS_GHC -Wall #-}

import HW01Tests()

lastDigit :: Integer -> Integer
dropLastDigit :: Integer -> Integer

lastDigit n = mod n 10

dropLastDigit n = div n 10

module Luhn
  (validate)
where

import           Luhn.Internal

validate :: Integer -> Bool
validate n = (sumDigits . doubleEveryOther . toDigits) n `rem` 10 == 0

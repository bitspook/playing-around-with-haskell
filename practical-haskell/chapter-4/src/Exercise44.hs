module Exercise44 where

class Priceable i where
  price :: i -> Integer

totalPrice :: Priceable p => [p] -> Integer
totalPrice = sum . map price

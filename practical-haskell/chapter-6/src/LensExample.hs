{-# LANGUAGE LambdaCase #-}
module LensExample where

import           Lens.Micro.Platform

data Client i = GovOrg i String
              | Company i String Person String
              | Individual i Person
data Person = Person String String

firstName :: Lens' Person String
firstName = lens
            (\(Person f _) -> f)
            (\(Person _ l) newF -> Person newF l)

lastName :: Lens' Person String
lastName = lens
           (\(Person _ l) -> l)
           (\(Person f l) newL -> Person f newL)

identifier :: Lens (Client i) (Client j) i j
identifier = lens
             (\case (GovOrg i _) -> i
                    (Company i _ _ _) -> i
                    (Individual i _) -> i)
             (\client newId -> case client of
                 GovOrg _ n      -> GovOrg newId n
                 Company _ n p r -> Company newId n p r
                 Individual _ p  -> Individual newId p)

fullName :: Lens' Person String
fullName = lens
           (\(Person f l) -> f ++ " " ++ l)
           (\_ newF -> case words newF of
                        f:l:_ -> Person f l
                        _     -> error "Incorrect name")

{-# LANGUAGE TemplateHaskell #-}

module AutoLensExample where

import           Lens.Micro.Platform

data Client i = GovOrg { _identifier :: i, _name :: String }
              | Company { _identifier :: i, _name :: String, _person :: Person, _duty :: String }
              | Individual { _identifier :: i, _person :: Person }
              deriving Show

data Person = Person { _firstName :: String, _lastName :: String }
            deriving Show

makeLenses ''Client
makeLenses ''Person

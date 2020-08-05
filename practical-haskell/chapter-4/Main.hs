module Main where

import qualified Data.Map as M

insert :: Ord k => k -> a -> M.Map k a -> M.Map k a
insert k v m = M.alter (\_ -> Just v) k m

delete :: Ord k => M.Map k a -> k -> M.Map k a
delete m k = M.alter (\_ -> Nothing) k m

adjust :: Ord k => (a -> a) -> k -> M.Map k a -> M.Map k a
adjust f k m = M.alter (fmap f) k m

main :: IO ()
main = putStrLn "lol"

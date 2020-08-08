module Chapter4 where

import qualified Data.Map as M

insert :: Ord k => k -> a -> M.Map k a -> M.Map k a
insert k v m = M.alter (\_ -> Just v) k m

delete :: Ord k => k -> M.Map k a -> M.Map k a
delete k m = M.alter (\_ -> Nothing) k m

adjust :: Ord k => (a -> a) -> k -> M.Map k a -> M.Map k a
adjust f k m = M.alter (fmap f) k m

data ClientKind = GovOrgKind | CompanyKind | IndividualKind

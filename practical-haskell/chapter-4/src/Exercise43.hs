{-# LANGUAGE RecordWildCards #-}
module Exercise43 where

import qualified Data.Map as M
import qualified Data.Set as S

data Client i = GovOrg { clientId :: i, clientName :: String }
              | Company { clientId :: i, clientName :: String
                        , person :: Person, duty :: String }
              | Individual { clientId :: i, person :: Person }
              deriving (Show, Eq, Ord)

data Person = Person { firstName :: String, lastName :: String }
              deriving (Show, Eq, Ord)

data ClientKind = GovOrgKind | CompanyKind | IndividualKind
  deriving (Show, Eq, Ord)

classifyClients :: [Client Integer] -> M.Map ClientKind (S.Set (Client Integer))
classifyClients = foldr insert initial
  where
    initial = M.fromList [(GovOrgKind, S.empty), (CompanyKind, S.empty), (IndividualKind, S.empty)]
    insert c ckm = case c of
      GovOrg {..}     -> M.adjust (\s -> S.insert c s) GovOrgKind ckm
      Company {..}    -> M.adjust (\s -> S.insert c s) CompanyKind ckm
      Individual {..} -> M.adjust (\s -> S.insert c s) IndividualKind ckm

classifyClients' :: [Client Integer] -> M.Map ClientKind (S.Set (Client Integer))
classifyClients' xs = M.fromList [(GovOrgKind, govs), (CompanyKind, companies), (IndividualKind, individuals)]
  where
    isGov (GovOrg _ _) = True
    isGov _            = False
    isCompany (Company {..}) = True
    isCompany _              = False
    isIndividual (Individual _ _) = True
    isIndividual _                = False
    govs = S.fromList $ filter isGov xs
    companies = S.fromList $ filter isCompany xs
    individuals = S.fromList $ filter isIndividual xs

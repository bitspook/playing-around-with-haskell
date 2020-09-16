{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module KMeansState where

import           Data.Foldable
import qualified Data.Map      as M

class Ord v => Vector v where
  distance :: v -> v -> Double
  centroid :: [v] -> v

instance Vector (Double, Double) where
  distance (a, b) (c, d) = sqrt $ (c-a)*(c-a) + (d-b)*(d-b)
  centroid vs = (x / n, y / n)
    where
      (x, y) = foldr (\(a, b) (c, d) -> (a + c, b + d)) (0, 0) vs
      n = fromIntegral $ length vs

class Vector v => Vectorizable e v where
  toVector :: e -> v

instance Vectorizable (Double, Double) (Double, Double) where
  toVector = id


type State s a = s -> (a, s)

thenDo :: State s a -> (a -> State s b) -> State s b
thenDo f g s = g resultOfF stateAfterF
  where (resultOfF, stateAfterF) = f s

data KMeansState v = KMeansState { centroids :: [v]
                                 , threshold :: Double
                                 , steps     :: Int }

newCentroids :: (Vector v, Vectorizable e v) => M.Map v [e] -> [v]
newCentroids = M.elems . fmap (centroid . map toVector)

clusterAssignments :: (Vector v, Vectorizable e v)
                   => [v] -> [e] -> M.Map v [e]
clusterAssignments centrs points =
  let initialMap = M.fromList $ zip centrs (repeat [])
  in foldr (\p m -> let chosenC = minimumBy (compareDistance p) centrs
                   in M.adjust (p:) chosenC m)
     initialMap points
  where compareDistance p x y = compare (distance x $ toVector p)
                                        (distance y $ toVector p)

kMeans' :: (Vector v, Vectorizable e v) => [e] -> State (KMeansState v) [v]
kMeans' = undefined

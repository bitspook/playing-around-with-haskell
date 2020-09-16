{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module KMeansST where

import           Control.Monad.ST
import           Data.Foldable
import qualified Data.Map         as M
import           Data.STRef

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


newCentroidsPhase :: (Vector v, Vectorizable e v) => M.Map v [e] -> [v]
newCentroidsPhase = M.elems . fmap (centroid . map toVector)

clusterAssignments :: (Vector v, Vectorizable e v)
                   => [v] -> [e] -> M.Map v [e]
clusterAssignments centrs points =
  let initialMap = M.fromList $ zip centrs (repeat [])
  in foldr (\p m -> let chosenC = minimumBy (compareDistance p) centrs
                   in M.adjust (p:) chosenC m)
     initialMap points
  where compareDistance p x y = compare (distance x $ toVector p)
                                        (distance y $ toVector p)

shouldStop :: (Vector v) => [(v, v)] -> Double -> Bool
shouldStop centroids threshold =
  foldr (\(x, y) s -> s + distance x y) 0.0 centroids < threshold

kMeans ::
     (Ord v, Vector v, Vectorizable e v)
  => (Int -> [e] -> [v]) -- initialization function
  -> Int -- number of centroids
  -> [e] -- the information
  -> Double -- threshold
  -> ([v], Int) -- final centroids with no of iterations
kMeans i k points threshold =
  runST $ do
    c <- newSTRef (i k points)
    d <- newSTRef 1
    kMeans' c points threshold d

kMeans' ::
     (Num b, Vectorizable e v)
  => STRef s [v]
  -> [e]
  -> Double
  -> STRef s b
  -> ST s ([v], b)
kMeans' centroids points threshold depth = do
  c <- readSTRef centroids
  d <- readSTRef depth

  let assignments = clusterAssignments c points
      newCentroids = newCentroidsPhase assignments
      newOldCentroids = zip c newCentroids

  writeSTRef centroids newCentroids
  modifySTRef' depth (+ 1)

  if shouldStop newOldCentroids threshold
    then return (c, d)
    else kMeans' centroids points threshold depth

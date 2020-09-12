{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Vector where

import           Data.List
import qualified Data.Map  as M

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

clusterAssignmentPhase :: (Ord v, Vector v, Vectorizable e v) => [v] -> [e] -> M.Map v [e]
clusterAssignmentPhase centroids points =
  foldr (\p m -> let chosenC = minimumBy (compareDistance p) centroids
                in M.adjust (p:) chosenC m) initialMap points
  where
    initialMap = M.fromList $ zip centroids (repeat [])
    compareDistance p x y = compare (distance x $ toVector p) (distance y $ toVector p)

newCentroidPhase :: (Vector v, Vectorizable e v) => M.Map v [e] -> [(v, v)]
newCentroidPhase = M.toList . fmap (centroid . map toVector)

shouldStop :: (Vector v) => [(v, v)] -> Double -> Bool
shouldStop centroids threshold =
  foldr (\(x, y) s -> s + distance x y) 0.0 centroids < threshold

kMeans :: (Vector v, Vectorizable e v)
  => (Int -> [e] -> [v]) --initialization function
  -> Int               --number of centroids
  -> [e]               --the information
  -> Double            --threshold
  -> [v]               --final centroids
kMeans i k points t = kMeans' (i k points) points t

kkMeans
  :: Vectorizable e v =>
     (t -> [e] -> [v]) -> t -> [e] -> Double -> ([v], Integer)
kkMeans i k points t = kkMeans' (i k points) points t 0

kMeans' :: (Vector v, Vectorizable e v) => [v] -> [e] -> Double -> [v]
kMeans' centroids points threshold =
  let assignments = clusterAssignmentPhase centroids points
      oldNewCentroids = newCentroidPhase assignments
      newCentroids = map snd oldNewCentroids
  in if shouldStop oldNewCentroids threshold
     then newCentroids
     else kMeans' newCentroids points threshold

kkMeans' :: (Vector v, Vectorizable e v)
  => [v] -> [e] -> Double -> Integer -> ([v], Integer)
kkMeans' centroids points threshold callCount =
  let assignments = clusterAssignmentPhase centroids points
      oldNewCentroids = newCentroidPhase assignments
      newCentroids = map snd oldNewCentroids
  in if shouldStop oldNewCentroids threshold
     then (newCentroids, callCount + 1)
     else kkMeans' newCentroids points threshold (callCount + 1)

initializeSimple :: Int -> [e] -> [(Double, Double)]
initializeSimple 0 _ = []
initializeSimple n v = (fromIntegral n, fromIntegral n) : initializeSimple (n - 1) v

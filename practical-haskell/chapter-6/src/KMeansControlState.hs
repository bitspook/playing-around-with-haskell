{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module KMeansControlState where

import           Control.Monad.State
import           Data.Foldable
import qualified Data.Map            as M
import           Lens.Micro.Platform

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

data KMeansState v = KMeansState { _centroids :: [v]
                                 , _threshold :: Double
                                 , _steps     :: Int }

makeLenses ''KMeansState

initializeState :: (Int -> [e] -> [v])
                -> Int -> [e] -> Double -> KMeansState e v
initializeState i n pts t = KMeansState (i n pts) pts (1.0/0.0) t 0


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
kMeans' points = do
  prevCentrs <- use centroids
  let assignments = clusterAssignments prevCentrs points
      newCentrs = newCentroids assignments
  centroids .= newCentrs
  steps += 1
  t <- use threshold
  let err = sum $ zipWith distance prevCentrs newCentrs
  if err < t then return newCentrs else kMeans' points

kMeans :: (Vector v, Vectorizable e v) => Int -> [e] -> Double -> [v]
kMeans n pts t = evalState (kMeans' pts) (initializeState n t)

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module KMeansLens where

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


data KMeansState e v = KMeansState { _centroids :: [v], _points :: [e]
                                   , _err :: Double, _threshold :: Double
                                   , _steps :: Int }

makeLenses ''KMeansState

initializeState :: (Int -> [e] -> [v])
                -> Int -> [e] -> Double -> KMeansState e v
initializeState i n pts t = KMeansState (i n pts) pts (1.0/0.0) t 0

clusterAssignmentPhase :: (Vector v, Vectorizable e v)
                       => KMeansState e v -> M.Map v [e]
clusterAssignmentPhase state =
  foldr
    ( \p m ->
        let chosenC = minimumBy (compareDistance p) cts
         in M.adjust (p :) chosenC m
    )
    initialMap
    pts
  where
    pts = state ^. points
    cts = state ^. centroids
    initialMap = M.fromList $ zip cts (repeat [])
    compareDistance p x y = compare (distance x $ toVector p) (distance y $ toVector p)

kMeans :: (Vector v, Vectorizable e v)
       => (Int -> [e] -> [v]) -> Int -> [e] -> Double -> [v]
kMeans i n pts t = view centroids $ kMeans'  (initializeState i n pts t)

kMeans' :: (Vector v, Vectorizable e v)
        => KMeansState e v -> KMeansState e v
kMeans' state =
  let assignments = clusterAssignmentPhase state
      state1 = state & centroids.traversed
                     %~ (\c -> centroid
                          $ fmap toVector
                          $ M.findWithDefault [] c assignments)
      state2 = state1 & err .~ sum (zipWith distance
                                     (state^.centroids)
                                     (state1^.centroids))
      state3 = state2 & steps +~ 1
  in if state3^.err < state3^.threshold then state3 else kMeans' state3

initializeSimple :: Int -> [e] -> [(Double, Double)]
initializeSimple 0 _ = []
initializeSimple n v = (fromIntegral n, fromIntegral n) : initializeSimple (n - 1) v

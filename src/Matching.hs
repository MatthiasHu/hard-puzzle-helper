module Matching
  ( MatchingData
  , edgeMatchingsStatistic
  , bestEdgeMatchings
  , greedyGrowth
  , fillPositions
  , bestQuad
  ) where

import Cluster
import MatchingData
import Statistic

import qualified Data.Vector as V
import Data.List
import Data.Ord


edgeMatchingsStatistic :: MatchingData -> Int -> [Position]
edgeMatchingsStatistic md step =
    statistic
  . map ((*step) . (`div` step) . edgeMatchingCost md)
  $ allEdgeMatchings md

bestEdgeMatchings :: MatchingData -> [(EdgeId, EdgeId)]
bestEdgeMatchings md =
  sortOn (edgeMatchingCost md) (allEdgeMatchings md)

-- | Grow cluster by one piece, greedily.
greedyGrowth :: MatchingData -> Cluster -> Cluster
greedyGrowth md c = addPiece pos rp c
  where
    (pos, rp) = minimumBy (comparing $ uncurry (avgAdditionCost md c))
      (allPossibleAdditions md c)

-- | Find additions with minimal total cost filling the given positions.
-- Abort search when reaching given bound on total cost.
bestMultiAddition ::
  MatchingData -> Cluster -> Int -> [Position] ->
  Maybe ([(Position, RotatedPiece)], Int)
bestMultiAddition md c bound [] = Just ([], 0)
bestMultiAddition md c bound (pos:rest) =
  fstMaybe $ foldl updateOptimum (Nothing, bound) candidates
  where
    candidates = sortOn snd . filter ((<bound) . snd) $
      [ (rp, totalAdditionCost md c pos rp)
      | rp <- unusedRotatedPieces c ]
    fstMaybe (Nothing, _) = Nothing
    fstMaybe (Just a, b) = Just (a, b)
    updateOptimum (x, costOfX) (rp, cost) =
      let c' = addPiece pos rp c
          bound' = costOfX - cost
      in case bestMultiAddition md c' bound' rest of
        Nothing           -> (x, costOfX)
        Just (y, costOfY) -> (Just ((pos, rp) : y), cost + costOfY)

fillPositions :: MatchingData -> Cluster -> [Position] -> Cluster
fillPositions md c positions =
  foldl (flip (uncurry addPiece)) c additions
  where
    Just (additions, _) = bestMultiAddition md c highBound positions
    highBound = 10^5

bestQuad :: MatchingData -> Cluster
bestQuad md = fillPositions md (emptyCluster md)
  [(0, 0), (0, 1), (1, 0), (1, 1)]

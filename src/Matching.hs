module Matching
  ( MatchingData
  , edgeMatchingsStatistic
  , bestEdgeMatchings
  , greedyGrowth
  , bestMultiAdditionCandidates
  , bestMultiAddition
  , bestMultiAdditionDijkstra
  , fillPositions
  , bestQuad
  ) where

import Cluster
import MatchingData
import CostTree
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
greedyGrowth md c = addPiece addition c
  where
    addition = minimumBy (comparing $ avgAdditionCost md c)
      (allPossibleAdditions md c)

multiAdditionsTree ::
  MatchingData -> Cluster -> [Position] -> Tree (Int, Addition)
multiAdditionsTree md c [] = Leaf
multiAdditionsTree md c (pos:rest) =
  Fork [ ( (totalAdditionCost md c a, a)
         , multiAdditionsTree md (addPiece a c) rest )
       | a <- [ (pos, rp) | rp <- unusedRotatedPieces c ] ]

bestMultiAdditionCandidates ::
  MatchingData -> Cluster -> Int -> [Position] -> [(Int, [Addition])]
bestMultiAdditionCandidates md c bound poss =
  cheapestPathCandidates (multiAdditionsTree md c poss) bound

-- | Find additions with minimal total cost filling the given positions.
-- Abort search when reaching given bound on total cost.
bestMultiAddition ::
  MatchingData -> Cluster -> Int -> [Position] ->
  Maybe (Int, [Addition])
bestMultiAddition md c bound poss =
  mLast $ bestMultiAdditionCandidates md c bound poss
  where
    mLast [] = Nothing
    mLast l = Just (last l)

bestMultiAdditionDijkstra ::
  MatchingData -> Cluster -> Int -> [Position] ->
  (Int, [Addition])
bestMultiAdditionDijkstra md c bound poss =
  cheapestPathDijkstra' (multiAdditionsTree md c poss) bound

fillPositions :: MatchingData -> Cluster -> [Position] -> Cluster
fillPositions md c positions =
  foldl (flip addPiece) c additions
  where
    Just (_ , additions) = bestMultiAddition md c highBound positions
    highBound = 50 -- 10^5

bestQuad :: MatchingData -> Cluster
bestQuad md = fillPositions md (emptyCluster md)
  [(0, 0), (0, 1), (1, 0), (1, 1)]

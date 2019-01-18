module Matching
  ( MatchingData
  , edgeMatchingsStatistic
  , bestEdgeMatchings
  ) where

import MatchingData
import Statistic

import qualified Data.Vector as V
import Data.List


edgeMatchingsStatistic :: MatchingData -> Int -> [(Int, Int)]
edgeMatchingsStatistic md step =
    statistic
  . map ((*step) . (`div` step) . edgeMatchingCost md)
  $ allEdgeMatchings md

bestEdgeMatchings :: MatchingData -> [(EdgeId, EdgeId)]
bestEdgeMatchings md =
  sortOn (edgeMatchingCost md) (allEdgeMatchings md)

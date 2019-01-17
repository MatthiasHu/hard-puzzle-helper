module Matching
  ( MatchingData
  , edgeMatchingsStatistic
  , bestEdgeMatchings
  ) where

import Edge
import Piece
import Directions
import Statistic

import qualified Data.Vector as V
import Data.List
import Data.Ord


type EdgeId = (Int, Direction)

type MatchingData = V.Vector Piece

edgeMatchingsStatistic :: MatchingData -> Int -> [(Int, Int)]
edgeMatchingsStatistic md step =
    statistic
  . map ((*step) . (`div` step) . edgeMatchingCost md)
  $ allEdgeMatchings md

bestEdgeMatchings :: MatchingData -> [(EdgeId, EdgeId)]
bestEdgeMatchings md =
  sortOn (edgeMatchingCost md) (allEdgeMatchings md)

edgeMatchingCost :: MatchingData -> (EdgeId, EdgeId) -> Int
edgeMatchingCost md (a, b) = matchingCost (getEdge md a) (getEdge md b)

getEdge :: MatchingData -> EdgeId -> Edge64
getEdge md (pieceId, dir) = direction dir (md V.! pieceId)

allEdgeMatchings :: MatchingData -> [(EdgeId, EdgeId)]
allEdgeMatchings md =
  [ ((p0, d0), (p1, d1))
  | p0 <- [0..length md-1], p1 <- [p0+1..length md-1]
  , d0 <- allDirections, d1 <- allDirections ]

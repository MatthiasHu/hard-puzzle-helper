module Matching
  ( bestEdgeMatchings
  ) where

import Edge
import Piece
import Directions

import qualified Data.Vector as V
import Data.List
import Data.Ord


type EdgeId = (Int, Direction)

type MatchingData = V.Vector Piece

bestEdgeMatchings :: MatchingData -> [(EdgeId, EdgeId)]
bestEdgeMatchings md = sortBy (comparing match) allMatchings
  where
    allMatchings = [(a, b) | a <- allEdgeIds, b <- allEdgeIds]
    match (a, b) = matchingCost (getEdge md a) (getEdge md b)
    allEdgeIds =
      [ (pieceId, dir)
      -- TODO: all matchings, not the first few
      | pieceId <- [0..100], dir <- allDirections ]

getEdge :: MatchingData -> EdgeId -> Edge64
getEdge md (pieceId, dir) = direction dir (md V.! pieceId)

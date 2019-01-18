module MatchingData
  ( PieceId
  , EdgeId
  , MatchingData
  , getEdge
  , edgeMatchingCost
  , allEdgeMatchings
  ) where

import Piece
import Directions
import Edge

import qualified Data.Vector as V


type PieceId = Int

type EdgeId = (Int, Direction)

type MatchingData = V.Vector Piece

getEdge :: MatchingData -> EdgeId -> Edge64
getEdge md (pieceId, dir) = direction dir (md V.! pieceId)

edgeMatchingCost :: MatchingData -> (EdgeId, EdgeId) -> Int
edgeMatchingCost md (a, b) = matchingCost (getEdge md a) (getEdge md b)

allEdgeMatchings :: MatchingData -> [(EdgeId, EdgeId)]
allEdgeMatchings md =
  [ ((p0, d0), (p1, d1))
  | p0 <- [0..length md-1], p1 <- [p0+1..length md-1]
  , d0 <- allDirections, d1 <- allDirections ]

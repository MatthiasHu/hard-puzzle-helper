module MatchingData
  ( PieceId
  , EdgeId
  , MatchingData
  , getPiece
  , getEdge
  , edgeMatchingCost
  , allEdgeMatchings
  ) where

import Piece
import Directions
import Edge

import qualified Data.Vector as V


type PieceId = Int

type EdgeId = (PieceId, Direction)

type MatchingData = V.Vector Piece

getPiece :: MatchingData -> PieceId -> Piece
getPiece md pieceId = md V.! pieceId

getEdge :: MatchingData -> EdgeId -> Edge64
getEdge md (pieceId, dir) = direction dir (getPiece md pieceId)

edgeMatchingCost :: MatchingData -> (EdgeId, EdgeId) -> Int
edgeMatchingCost md (a, b) = matchingCost (getEdge md a) (getEdge md b)

allEdgeMatchings :: MatchingData -> [(EdgeId, EdgeId)]
allEdgeMatchings md =
  [ ((p0, d0), (p1, d1))
  | p0 <- [0..length md-1], p1 <- [p0+1..length md-1]
  , d0 <- allDirections, d1 <- allDirections ]

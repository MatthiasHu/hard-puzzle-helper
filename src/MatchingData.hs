module MatchingData
  ( Cost
  , PieceId
  , EdgeId
  , MatchingData()
  , matchingDataFromPieces
  , numberOfPieces
  , getPiece
  , getEdge
  , edgeMatchingCost
  , rawEdgeMatchingCost
  , allEdgeMatchings
  ) where

import Piece
import Directions
import Edge

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Data.Word


type PieceId = Int

type EdgeId = (PieceId, Direction)

data MatchingData = MatchingData
  { pieces        :: V.Vector Piece
  , matchingTable :: VU.Vector Word16
  }

matchingDataFromPieces :: V.Vector Piece -> MatchingData
matchingDataFromPieces ps = MatchingData ps table
  where
    table = VU.replicate tableSize 0 VU.//
      [ ( edgeMatchingIndex numberOfPieces e0 e1
        , rawMatchingCost (edge e0) (edge e1) )
      | e0 <- allEdges, e1 <- allEdges ]
    edge (pId, d) = direction d (ps V.! pId)
    allEdges =
      [ (pId, d)
      | pId <- [0..numberOfPieces-1], d <- allDirections ]
    numberOfPieces = length ps
    tableSize = (4*numberOfPieces)^2

edgeMatchingIndex :: Int -> EdgeId -> EdgeId -> Int
edgeMatchingIndex numberOfPieces (pId0, d0) (pId1, d1) =
  n*i + j
  where
    n = 4*numberOfPieces
    i = 4*pId0 + directionIndex d0
    j = 4*pId1 + directionIndex d1

numberOfPieces :: MatchingData -> Int
numberOfPieces md = length (pieces md)

getPiece :: MatchingData -> PieceId -> Piece
getPiece md pieceId = pieces md V.! pieceId

getEdge :: MatchingData -> EdgeId -> Edge64
getEdge md (pieceId, dir) = direction dir (getPiece md pieceId)

edgeMatchingCost :: MatchingData -> (EdgeId, EdgeId) -> Cost
edgeMatchingCost md (a, b) =
  rawCostToCost (rawEdgeMatchingCost md (a, b))

rawEdgeMatchingCost :: MatchingData -> (EdgeId, EdgeId) -> Word16
rawEdgeMatchingCost md (a, b) =
  matchingTable md VU.! edgeMatchingIndex (numberOfPieces md) a b
--  rawMatchingCost (getEdge md a) (getEdge md b)

allEdgeMatchings :: MatchingData -> [(EdgeId, EdgeId)]
allEdgeMatchings md =
  [ ((p0, d0), (p1, d1))
  | p0 <- [0..n-1], p1 <- [p0+1..n-1]
  , d0 <- allDirections, d1 <- allDirections ]
  where
    n = numberOfPieces md

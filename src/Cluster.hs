module Cluster
  ( Cluster
  ) where

import MatchingData
import Rotation
import SquareGrid

import qualified Data.Set as S
import qualified Data.Map.Strict as M


data Cluster = Cluster
  { pieces       :: M.Map (Int, Int) (PieceId, Rotation)
  , unusedPieces :: S.Set PieceId
  }

emptyCluster :: MatchingData -> Cluster
emptyCluster md = Cluster M.empty (S.fromList [0..length md -1])

addPiece :: (Int, Int) -> (PieceId, Rotation) -> Cluster -> Cluster
addPiece pos (p, r) c
  | pos `M.member` (pieces c)  =
    error $ unwords ["addPiece: position", show pos, "is occupied"]
  | otherwise  = Cluster
    { pieces = M.insert pos (p, r) (pieces c)
    , unusedPieces = S.delete p (unusedPieces c)
    }

boundaryPositions :: Cluster -> S.Set (Int, Int)
boundaryPositions c = boundaryNonMembers $ M.keysSet (pieces c)

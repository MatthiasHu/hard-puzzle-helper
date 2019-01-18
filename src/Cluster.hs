module Cluster
  ( Position
  , RotatedPiece
  , Cluster
  , emptyCluster
  , addPiece
  , onePieceCluster
  , greedyGrowth
  , showCluster
  ) where

import MatchingData
import Piece
import Edge
import Rotation
import Directions
import SquareGrid

import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Ord
import Data.List


type Position = (Int, Int)
type RotatedPiece = (PieceId, Rotation)

data Cluster = Cluster
  { pieces       :: M.Map Position RotatedPiece
  , unusedPieces :: S.Set PieceId
  }

emptyCluster :: MatchingData -> Cluster
emptyCluster md = Cluster M.empty (S.fromList [0..length md -1])

addPiece :: Position -> RotatedPiece -> Cluster -> Cluster
addPiece pos rp@(p, _) c
  | pos `M.member` (pieces c)  =
    error $ unwords ["addPiece: position", show pos, "is occupied"]
  | otherwise  = Cluster
    { pieces = M.insert pos rp (pieces c)
    , unusedPieces = S.delete p (unusedPieces c)
    }

onePieceCluster :: MatchingData -> PieceId -> Cluster
onePieceCluster md p =
  addPiece (0, 0) (p, mkRotation 0) (emptyCluster md)

getClusterPiece ::
  MatchingData -> Cluster -> Position -> Maybe Piece
getClusterPiece md c pos =
  case pieces c M.!? pos of
    Nothing -> Nothing
    Just (p, r) -> Just $ rotate r (getPiece md p)

neighbouringPositions :: Cluster -> S.Set Position
neighbouringPositions c = boundaryNonMembers $ M.keysSet (pieces c)

allPossibleAdditions ::
  MatchingData -> Cluster -> [(Position, RotatedPiece)]
allPossibleAdditions md c =
  [ (pos, (p, r))
  | pos <- S.toList (neighbouringPositions c)
  , p <- S.toList (unusedPieces c)
  , r <- allRotations ]

clusterEdgeCost ::
  MatchingData -> Cluster -> Position -> Direction -> Maybe Int
clusterEdgeCost md c pos dir = matchingCost
  <$> (direction dir  <$> getClusterPiece md c pos )
  <*> (direction dir' <$> getClusterPiece md c pos')
  where
    pos' = move dir pos
    dir' = oppositeDirection dir

avgAdditionCost ::
  MatchingData -> Cluster -> Position -> RotatedPiece -> Float
avgAdditionCost md c pos rp = avg . catMaybes $
  [ clusterEdgeCost md c' pos dir | dir <- allDirections ]
  where
    c' = addPiece pos rp c
    avg l = fromIntegral (sum l) / fromIntegral (length l)

-- | Grow cluster by one piece, greedily.
greedyGrowth :: MatchingData -> Cluster -> Cluster
greedyGrowth md c = addPiece pos rp c
  where
    (pos, rp) = minimumBy (comparing $ uncurry (avgAdditionCost md c))
      (allPossibleAdditions md c)

showCluster :: Cluster -> String
showCluster c
  | M.null ps  = "(empty cluster)"
  | otherwise  = unlines . intersperse "" $
    [ unwords [ maybe "    " (fourDigit . fst) (ps M.!? (x, y))
              | x <- [x0..x1] ]
    | y <- [y0..y1] ]
  where
    x0 = minimum xs
    x1 = maximum xs
    y0 = minimum ys
    y1 = maximum ys
    xs = map fst (M.keys ps)
    ys = map snd (M.keys ps)
    fourDigit = reverse . take 4 . (++repeat '0') . reverse . show
    ps = pieces c

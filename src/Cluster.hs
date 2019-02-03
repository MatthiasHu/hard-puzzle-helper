module Cluster
  ( Cost
  , Position
  , RotatedPiece
  , Addition
  , Cluster
  , emptyCluster
  , addPiece
  , onePieceCluster
  , showCluster
  , showClusterWithAvgCost
  , positionInCluster
  , getClusterPiece
  , neighbouringPositions
  , unusedRotatedPieces
  , allPossibleAdditions
  , totalAdditionCost
  , avgAdditionCost
  , multiAdditionCosts
  , avgMultiAdditionCost
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
import Data.List


type Position = (Int, Int)
type RotatedPiece = (PieceId, Rotation)
type Addition = (Position, RotatedPiece)

data Cluster = Cluster
  { pieces       :: M.Map Position RotatedPiece
  , unusedPieces :: S.Set PieceId
  }

emptyCluster :: MatchingData -> Cluster
emptyCluster md = Cluster M.empty (S.fromList [0..numberOfPieces md -1])

clusterSize :: Cluster -> Int
clusterSize c = M.size (pieces c)

addPiece :: Addition -> Cluster -> Cluster
addPiece (pos, rp@(p, _)) c
  | pos `M.member` (pieces c)  =
    error $ unwords ["addPiece: position", show pos, "is occupied"]
  | otherwise  = Cluster
    { pieces = M.insert pos rp (pieces c)
    , unusedPieces = S.delete p (unusedPieces c)
    }

onePieceCluster :: MatchingData -> PieceId -> Cluster
onePieceCluster md p =
  addPiece ((0, 0), (p, mkRotation 0)) (emptyCluster md)

showCluster :: Cluster -> String
showCluster c
  | M.null ps  = "(empty cluster)"
  | otherwise  = unlines . intersperse "" $
    [ unwords [ maybe " .. " (fourDigit . fst) (ps M.!? (x, y))
              | x <- [x0-1..x1+1] ]
    | y <- [y0-1..y1+1] ]
  where
    x0 = minimum xs
    x1 = maximum xs
    y0 = minimum ys
    y1 = maximum ys
    xs = map fst (M.keys ps)
    ys = map snd (M.keys ps)
    fourDigit = reverse . take 4 . (++repeat '0') . reverse . show
    ps = pieces c

showClusterWithAvgCost :: MatchingData -> Cluster -> String
showClusterWithAvgCost md c = unlines
  [ showCluster c
  , concat ["(", show (clusterSize c), " pieces, avg edge cost "
  , show (clusterAvgCost md c), ")"] ]

positionInCluster ::
  Cluster -> Position -> Bool
positionInCluster c pos =
  case pieces c M.!? pos of
    Nothing -> False
    Just _ -> True

getClusterPiece ::
  MatchingData -> Cluster -> Position -> Maybe Piece
getClusterPiece md c pos =
  case pieces c M.!? pos of
    Nothing -> Nothing
    Just (p, r) -> Just $ rotate r (getPiece md p)

neighbouringPositions :: Cluster -> S.Set Position
neighbouringPositions c = boundaryNonMembers $ M.keysSet (pieces c)

unusedRotatedPieces :: Cluster -> [RotatedPiece]
unusedRotatedPieces c =
  [ (p, r)
  | p <- S.toList (unusedPieces c)
  , r <- allRotations ]

allPossibleAdditions :: MatchingData -> Cluster -> [Addition]
allPossibleAdditions md c =
  [ (pos, rp)
  | pos <- S.toList (neighbouringPositions c)
  , rp <- unusedRotatedPieces c ]

clusterEdgeCost ::
  MatchingData -> Cluster -> Position -> Direction -> Maybe Cost
clusterEdgeCost md c pos dir = curry (edgeMatchingCost md)
  <$> (edgeOfRP dir  <$> (pieces c M.!? pos ))
  <*> (edgeOfRP dir' <$> (pieces c M.!? pos'))
  where
    pos' = move dir pos
    dir' = oppositeDirection dir
    edgeOfRP d (p, r) = (p, rotateDir (invertRotation r) d)

additionCosts :: MatchingData -> Cluster -> Addition -> [Cost]
additionCosts md c a@(pos, _) = catMaybes $
  [ clusterEdgeCost md c' pos dir | dir <- allDirections ]
  where
    c' = addPiece a c

totalAdditionCost :: MatchingData -> Cluster -> Addition -> Cost
totalAdditionCost md c a = sum (additionCosts md c a)

avgAdditionCost :: MatchingData -> Cluster -> Addition -> Cost
avgAdditionCost md c a = avg (additionCosts md c a)

multiAdditionCosts :: MatchingData -> Cluster -> [Addition] -> [Cost]
multiAdditionCosts md c [] = []
multiAdditionCosts md c (a:as) =
  additionCosts md c a ++
  multiAdditionCosts md (addPiece a c) as

avgMultiAdditionCost :: MatchingData -> Cluster -> [Addition] -> Cost
avgMultiAdditionCost md c as = avg (multiAdditionCosts md c as)

avg :: [Cost] -> Cost
avg l = (sum l) / fromIntegral (length l)

clusterCosts :: MatchingData -> Cluster -> [Cost]
clusterCosts md c = catMaybes
  [ clusterEdgeCost md c pos dir
  | pos <- M.keys (pieces c)
  , dir <- [East, North] ]

clusterAvgCost :: MatchingData -> Cluster -> Cost
clusterAvgCost md c = avg (clusterCosts md c)

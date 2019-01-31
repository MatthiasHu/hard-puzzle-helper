module Grow
  ( DecoratedCluster()
  , decorateCluster
  , undecorateCluster
  , grow
  ) where

import Matching
import Cluster
import Rotation
import Directions
import SquareGrid

import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Data.List
import Data.Maybe
import Data.Ord


growLocations :: Cluster -> [S.Set Position]
growLocations c = pairs ++ singles
  where
    ns = neighbouringPositions c
    pairs = filter (all (`S.member` ns)) $
         [ S.fromList [p, move East  p] | p <- S.toList ns ]
      ++ [ S.fromList [p, move North p] | p <- S.toList ns ]
    singles = map S.singleton . filter singleAllowed $ S.toList ns
    singleAllowed =
      (>=2) . length . filter (positionInCluster c) . neighbours

data DecoratedCluster = DecoratedCluster
  { undecorateCluster :: Cluster
  , growthOptions     :: M.Map (S.Set Position) (Maybe [Addition])
  }

decorateCluster :: MatchingData -> Cost -> Cluster -> DecoratedCluster
decorateCluster md avgCostBound c =
  completeDecoration md avgCostBound (DecoratedCluster c M.empty)

completeDecoration ::
  MatchingData -> Cost -> DecoratedCluster -> DecoratedCluster
completeDecoration md avgCostBound (DecoratedCluster c options) =
  DecoratedCluster c
    (M.fromList [ (loc, findOption loc) | loc <- growLocations c ])
  where
    findOption loc
      | loc `M.member` options
        = options M.! loc
      | otherwise
        = mkMultiAddition loc
    mkMultiAddition loc =
      bestMultiAddition md c
        (avgCostBound * fromIntegral (numberOfCosts loc))
        (S.toList loc)
    numberOfCosts loc = length
      (multiAdditionCosts md c (map mkDummyAddition (S.toList loc)))
    mkDummyAddition pos =
      (pos, (error "evaluating dummy addition", mkRotation 0))

invalidatePositions ::
  [Position] -> M.Map (S.Set Position) a -> M.Map (S.Set Position) a
invalidatePositions innerBadPoss options =
  M.filterWithKey (\l ma -> goodLocation l) options
  where
    allBadPoss = S.fromList
      ( innerBadPoss
        ++ concatMap neighbours innerBadPoss )
    goodLocation l = S.null (l `S.intersection` allBadPoss)

grow :: MatchingData -> Cost -> DecoratedCluster -> DecoratedCluster
grow md avgCostBound dc =
  applyMultiAddition md avgCostBound (snd winner) dc
  where
    winner = minimumBy (comparing fst) cmas
    cmas = [ (avgMultiAdditionCost md (undecorateCluster dc) ma, ma)
           | Just ma <- M.elems (growthOptions dc) ]

applyMultiAddition ::
  MatchingData -> Cost ->
  [Addition] -> DecoratedCluster -> DecoratedCluster
applyMultiAddition md avgCostBound ma (DecoratedCluster c options) =
  completeDecoration md avgCostBound
    ( DecoratedCluster
        (foldl' (flip addPiece) c ma)
        (invalidatePositions (map fst ma) options) )

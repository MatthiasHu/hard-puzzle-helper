module Grow
  ( grow
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


-- TODO: grow faster using decorated clusters
grow :: MatchingData -> Cost -> Cluster -> Cluster
grow md avgCostBound c =
  applyMultiAddition winner c
  where
    winner = minimumBy (comparing (avgMultiAdditionCost md c)) mas
    mas = mapMaybe mkMultiAddition (growLocations c)
    mkMultiAddition poss =
      bestMultiAddition md c
        (avgCostBound * fromIntegral (numberOfCosts poss))
        (S.toList poss)
    numberOfCosts poss = length
      (multiAdditionCosts md c (map mkDummyAddition (S.toList poss)))
    mkDummyAddition pos =
      (pos, (error "evaluating dummy addition", mkRotation 0))

applyMultiAddition :: [Addition] -> Cluster -> Cluster
applyMultiAddition ma c = foldl' (flip addPiece) c ma

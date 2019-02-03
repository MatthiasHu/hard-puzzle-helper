module Matching
  ( Cost
  , MatchingData
  , edgeMatchingsStatistic
  , bestEdgeMatchings
  , greedyGrowth
  , bestMultiAdditions
  , bestMultiAddition
  ) where

import Cluster
import MatchingData
import Statistic

import Data.List
import Data.Ord


edgeMatchingsStatistic :: MatchingData -> Int -> [(Int, Int)]
edgeMatchingsStatistic md step =
    statistic
  . map ((*step) . (`div` step) . fromIntegral . rawEdgeMatchingCost md)
  $ allEdgeMatchings md

bestEdgeMatchings :: MatchingData -> [(EdgeId, EdgeId)]
bestEdgeMatchings md =
  sortOn (edgeMatchingCost md) (allEdgeMatchings md)

-- | Grow cluster by one piece, greedily.
greedyGrowth :: MatchingData -> Cluster -> Cluster
greedyGrowth md c = addPiece addition c
  where
    addition = minimumBy (comparing $ avgAdditionCost md c)
      (allPossibleAdditions md c)

-- | List all multiadditions satisfying the given bound on total cost.
bestMultiAdditions ::
  MatchingData -> Cluster -> Cost -> [Position] -> [(Cost, [Addition])]
bestMultiAdditions md c costBound poss =
  searchCandidatesLists md costBound (map mkCandidatesList poss)
  where
    mkCandidatesList pos =
      ( pos
      , filter ((< costBound) . fst)
          [ (totalAdditionCost md c (pos, rp), rp)
          | rp <- unusedRotatedPieces c ]
      )

type CandidatesLists = [(Position, [(Cost, RotatedPiece)])]

searchCandidatesLists ::
  MatchingData -> Cost -> CandidatesLists -> [(Cost, [Addition])]
searchCandidatesLists md costBound [] = [(0, [])]
searchCandidatesLists md costBound ((pos, candidates):cls) =
  concatMap (choosingCandidate md costBound cls)
    [ (pos, c) | c <- candidates ]

choosingCandidate ::
  MatchingData -> Cost -> CandidatesLists ->
  (Position, (Cost, RotatedPiece)) -> [(Cost, [Addition])]
choosingCandidate md oldCostBound cls (thisPos, (thisCost, thisRp)) =
  map augmentResult $
    searchCandidatesLists md newCostBound (map updateCandidatesList cls)
  where
    newCostBound = oldCostBound - thisCost
    augmentResult (totalCost, additions) =
      ( totalCost + thisCost
      , (thisPos, thisRp) : additions )
    -- TODO: prohibit multiadditions using the same piece twice
    updateCandidatesList (pos', l) =
      ( pos'
      ,   filterCandidates
        . map (increaseCost pos')
        . filterCandidates
        $ l )
    filterCandidates = filter ((< newCostBound) . fst)
    increaseCost pos' (priorCost, rp') =
      (priorCost + totalAdditionCost md cluster (pos', rp'), rp')
    cluster = addPiece (thisPos, thisRp) (emptyCluster md)

bestMultiAddition ::
  MatchingData -> Cluster -> Cost -> [Position] -> Maybe [Addition]
bestMultiAddition md c costBound poss =
  case bestMultiAdditions md c costBound poss of
    [] -> Nothing
    l -> Just (snd $ minimumBy (comparing fst) l)

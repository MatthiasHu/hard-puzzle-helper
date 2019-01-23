module CostTree
  ( Tree(..)
  , cheapestPath
  , cheapestPathCandidates
  ) where

import Data.List


-- Edge marked trees with empty forks distinct from leafs.
data Tree a = Fork [(a, Tree a)] | Leaf

cheapestPath :: (Num c, Ord c) =>
  Tree (c, a) -> c -> Maybe (c, [a])
cheapestPath t bound = mLast (cheapestPathCandidates t bound)
  where
    mLast [] = Nothing
    mLast l = Just (last l)

cheapestPathCandidates :: (Num c, Ord c) =>
  Tree (c, a) -> c -> [(c, [a])]
cheapestPathCandidates Leaf _ = [(0, [])]
cheapestPathCandidates (Fork l) bound = go (sortOn (fst . fst) l) bound
  where
    go [] _ = []
    go (((c, a), t):rest) b =
      if c < b
      then let candidates =
                 [ (c'+c, a:l)
                 | (c', l) <- cheapestPathCandidates t (b - c) ]
               b' = last (b : map fst candidates)
           in candidates ++ go rest b'
      else go rest b

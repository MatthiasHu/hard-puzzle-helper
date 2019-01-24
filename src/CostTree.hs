module CostTree
  ( Tree(..)
  , cheapestPath
  , cheapestPathCandidates
  , cheapestPathDijkstra
  , cheapestPathDijkstra'
  ) where

import qualified Data.Map.Strict as M
import qualified Data.Heap as H
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

-- Dijkstra style search for shortest path.
cheapestPathDijkstra :: (Num c, Ord c) =>
  Tree (c, a) -> c -> (c, [a])
cheapestPathDijkstra originalTree bound =
  explore (M.singleton 0 [(originalTree, [])]) bound

explore :: (Num c, Ord c) =>
  M.Map c [(Tree (c, a), [a])] -> c -> (c, [a])
explore m bound = case t of
  Leaf          -> (minCost, trace)
  Fork children -> explore (M.unionWith (++) m' (toHeap children)) bound
  where
    Just (minCost, ((t, trace):_)) = M.lookupGE 0 m
    m' = M.update tail' minCost m
    tail' [_] = Nothing
    tail' (_:rest) = Just rest
    toHeap = M.fromListWith (++)
             . filter ((<bound) . fst)
             . map childToEntry
    childToEntry ((c, a), t') = (minCost + c, [(t', trace ++ [a])])

-- Use actual heaps.
cheapestPathDijkstra' :: (Num c, Ord c) =>
  Tree (c, a) -> c -> (c, [a])
cheapestPathDijkstra' originalTree bound =
  explore' (H.singleton (H.Entry 0 (originalTree, []))) bound

explore' :: (Num c, Ord c) =>
  H.Heap (H.Entry c (Tree (c, a), [a])) -> c -> (c, [a])
explore' h bound = case t of
  Leaf          -> (minCost, trace)
  Fork children -> explore' (H.union h' (toHeap children)) bound
  where
    Just (H.Entry minCost (t, trace), h') = H.viewMin h
    toHeap = H.fromList
             . filter ((<bound) . H.priority)
             . map childToEntry
    childToEntry ((c, a), t') = H.Entry (minCost + c) (t', trace ++ [a])

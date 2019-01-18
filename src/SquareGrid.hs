module SquareGrid
  ( neighbours
  , boundaryMembers
  , boundaryNonMembers
  ) where

import qualified Data.Set as S


neighbours :: (Int, Int) -> [(Int, Int)]
neighbours (x, y) =
  [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]

boundaryMembers :: S.Set (Int, Int) -> S.Set (Int, Int)
boundaryMembers s = S.filter (any (`S.notMember` s) . neighbours) s

boundaryNonMembers :: S.Set (Int, Int) -> S.Set (Int, Int)
boundaryNonMembers s = S.fromList $
  filter (`S.notMember` s) (concatMap neighbours (S.toList s))

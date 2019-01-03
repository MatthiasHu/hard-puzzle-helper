module SquareGrid
  ( neighbours
  , boundary
  ) where

import qualified Data.Set as Set


neighbours :: (Int, Int) -> [(Int, Int)]
neighbours (x, y) =
  [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]

boundary :: Set.Set (Int, Int) -> Set.Set (Int, Int)
boundary s = Set.filter (any (`Set.notMember` s) . neighbours) s

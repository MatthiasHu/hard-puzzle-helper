module Statistic
  ( statistic
  ) where

import qualified Data.Map.Strict as M
import Data.List


statistic :: (Ord a) => [a] -> [(a, Int)]
statistic l = M.assocs
  (foldl' (\m k -> M.insertWith (+) k 1 m) M.empty l)

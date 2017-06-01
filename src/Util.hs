
module Util(fmapForest, fmapTreeDepth) where

import Data.Tree


-- | 'fmap' over a 'Tree', but passing the depth as well
fmapTreeDepth :: (Int -> a -> b) -> Tree a -> Tree b
fmapTreeDepth op = f 0
    where f i (Node x xs) = Node (op i x) $ map (f $! i+1) xs

-- | 'fmap' over a forest, bottom-up
fmapForest :: ([Tree a] -> [Tree a]) -> [Tree a] -> [Tree a]
fmapForest op = f
    where f xs = op [Node y $ f ys | Node y ys <- xs]

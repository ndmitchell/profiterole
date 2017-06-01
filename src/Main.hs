{-# LANGUAGE RecordWildCards, OverloadedStrings, ViewPatterns #-}

module Main(main) where

import GHC.Prof
import Data.List.Extra
import Data.Char
import Data.Monoid
import Data.Scientific
import Data.Tree
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as T
import System.IO.Extra
import System.Environment


main :: IO ()
main = do
    args <- getArgs
    Right (Just tree) <- fmap costCentres . decode <$> T.readFile (head args)
    let vals = removeZero $ fmap toVal tree
    config <- readConfig ".profiterole.yaml"
    let roots = findRoots config vals
    let vals2 =  sortOn (negate . timeInh . rootLabel) $
                 fmapForest (sortOn (negate . timeTot . rootLabel)) $
                 mergeRoots $ liftRoots roots vals
    let indent i x = x{name = replicate (i*2) ' ' ++ name x}
    putStr $ unlines $ intercalate ["",""] $
        (" TOT   INH   IND" : showVals (map rootLabel $ take 25 vals2)) :
        [showVals $ flatten $ fmapTreeDepth indent x | x <- vals2]
    print $ sum $ map timeInd $ concatMap flatten vals2
    print $ sum $ map (timeInh . rootLabel) vals2


---------------------------------------------------------------------
-- UTILITIES

-- | 'fmap' over a 'Tree', but passing the depth as well
fmapTreeDepth :: (Int -> a -> b) -> Tree a -> Tree b
fmapTreeDepth op = f 0
    where f i (Node x xs) = Node (op i x) $ map (f $! i+1) xs

-- | 'fmap' over a forest, bottom-up
fmapForest :: ([Tree a] -> [Tree a]) -> [Tree a] -> [Tree a]
fmapForest op = f
    where f xs = op [Node y $ f ys | Node y ys <- xs]



data Val = Val
    {name :: String -- Name of this node
    ,timeTot :: Scientific -- Time spent under this node
    ,timeInh :: Scientific -- Time spent under this node excluding rerooted
    ,timeInd :: Scientific -- Time spent in this code
    ,entries :: Integer -- Number of times this node was called
    } deriving Show

toVal :: CostCentre -> Val
toVal CostCentre{..} = Val
    (T.unpack costCentreModule ++ " " ++ T.unpack costCentreName)
    costCentreInhTime costCentreInhTime costCentreIndTime
    costCentreEntries

data Config = Root | Bury deriving Eq

readConfig :: FilePath -> IO (String -> Maybe Config)
readConfig file = do
    let f (stripPrefix "root: " -> Just x) = (x, Root)
        f (stripPrefix "bury: " -> Just x) = (x, Bury)
        f x = error $ "Bad config, got " ++ show x
    mp <- Map.fromList . map f .  lines <$> readFile' file
    return $ flip Map.lookup mp

removeZero :: Tree Val -> Tree Val
removeZero (Node x xs) = Node x $ map removeZero $ filter (not . isZero . rootLabel) xs
    where isZero Val{..} = timeTot == 0


-- | A root has at least two distinct parents and isn't a local binding
findRoots :: (String -> Maybe Config) -> Tree Val -> Set.Set String
findRoots config x = Map.keysSet $
    Map.filterWithKey (\k v -> case config k of
        Just v -> v == Root
        Nothing -> not (isLocal k) && Set.size v > 1) $
    Map.fromListWith (<>) $ f x
    where
        f (Node v xs) = [(name $ rootLabel x, Set.singleton $ name v) | x <- xs] ++
                        concatMap f xs
        isLocal (word1 -> (_, x)) =  any isAlpha x && '.' `elem` x

liftRoots :: Set.Set String -> Tree Val -> [Tree Val]
liftRoots set x = fs set x
    where
        fs set x = let (y,_,ys) = f set x in y:ys

        -- return (this tree, discount to apply up, new roots)
        f :: Set.Set String -> Tree Val -> (Tree Val, Scientific, [Tree Val])
        f set (Node x ys)
            | name x `Set.member` set = (Node x{timeInh=0,timeInd=0} [], timeInh x, fs (Set.delete (name x) set) $ Node x ys)
            | otherwise = (Node x{timeInh = timeInh x - disc} child, disc, root)
                where (child, sum -> disc, concat -> root) = unzip3 $ map (f set) ys

mergeRoots :: [Tree Val] -> [Tree Val]
mergeRoots xs = Map.elems $ Map.fromListWith f [(name $ rootLabel x, x) | x <- xs]
    where f (Node x xs) (Node y ys) = Node (mergeVal x y) $ mergeRoots $ xs ++ ys

mergeVal :: Val -> Val -> Val
mergeVal x y
    | name x /= name y = error $ "mergeRoots, invariant violated"
    | otherwise = Val
        {name = name x
        ,timeTot = timeTot x + timeTot y
        ,timeInh = timeInh x + timeInh y
        ,timeInd = timeInd x + timeInd y
        ,entries = entries x + entries y}

showVals :: [Val] -> [String]
showVals xs = [intercalate "  " $ [f timeTot, f timeInh, f timeInd, name ++ " (" ++ show entries ++ ")"] | Val{..} <- xs]
    where
        f x = case show x of
            "0.0" -> "   -"
            "100.0" -> "99.9" -- avoid making the column bigger for a corner case
            ['0','.',x] -> [' ',' ','.',x]
            x -> replicate (4 - length x) ' ' ++ x

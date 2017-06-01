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
    let vals2 =  sortOn (negate . valTimeInh . rootLabel) $
                 map (sortTreeOn (negate . valTimeTot)) $
                 mergeRoots $ liftRoots roots vals
    putStr $ unlines $ intercalate ["",""] $
        (" TOT   INH   IND" : showVals (map rootLabel $ take 25 vals2)) :
        [showVals [y{valId = replicate (i*2) ' ' ++ valId y} | (i,y) <- unwindTree x] | x <- vals2]
    print $ sum $ map (valTimeInd . snd) $ concatMap unwindTree vals2
    print $ sum $ map (valTimeInh . rootLabel) vals2

unwindTree :: Tree a -> [(Int,a)]
unwindTree = f 0
    where f i (Node x xs) = (i,x) : concatMap (f $! i+1) xs

sortTreeOn :: Ord b => (a -> b) -> Tree a -> Tree a
sortTreeOn f (Node x xs) = Node x $ sortOn (f . rootLabel) (map (sortTreeOn f) xs)

rootId = valId . rootLabel

data Val = Val
    {valId :: String -- Name of this node
    ,valTimeTot :: Scientific -- Time spent under this node
    ,valTimeInh :: Scientific -- Time spent under this node excluding rerooted
    ,valTimeInd :: Scientific -- Time spent in this code
    ,valEntries :: Integer -- Number of times this node was called
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
    where isZero Val{..} = valTimeTot == 0


-- | A root has at least two distinct parents and isn't a local binding
findRoots :: (String -> Maybe Config) -> Tree Val -> Set.Set String
findRoots config x = Map.keysSet $
    Map.filterWithKey (\k v -> case config k of
        Just v -> v == Root
        Nothing -> not (isLocal k) && Set.size v > 1) $
    Map.fromListWith (<>) $ f x
    where
        f (Node v xs) = [(rootId x, Set.singleton $ valId v) | x <- xs] ++
                        concatMap f xs
        isLocal (word1 -> (_, x)) =  any isAlpha x && '.' `elem` x

liftRoots :: Set.Set String -> Tree Val -> [Tree Val]
liftRoots set x = fs set x
    where
        fs set x = let (y,_,ys) = f set x in y:ys

        -- return (this tree, discount to apply up, new roots)
        f :: Set.Set String -> Tree Val -> (Tree Val, Scientific, [Tree Val])
        f set (Node x ys)
            | valId x `Set.member` set = (Node x{valTimeInh=0,valTimeInd=0} [], valTimeInh x, fs (Set.delete (valId x) set) $ Node x ys)
            | otherwise = (Node x{valTimeInh = valTimeInh x - disc} child, disc, root)
                where (child, sum -> disc, concat -> root) = unzip3 $ map (f set) ys

mergeRoots :: [Tree Val] -> [Tree Val]
mergeRoots xs = Map.elems $ Map.fromListWith f [(rootId x, x) | x <- xs]
    where f (Node x xs) (Node y ys) = Node (mergeVal x y) $ mergeRoots $ xs ++ ys

mergeVal :: Val -> Val -> Val
mergeVal x y
    | valId x /= valId y = error $ "mergeRoots, invariant violated"
    | otherwise = Val
        {valId = valId x
        ,valTimeTot = valTimeTot x + valTimeTot y
        ,valTimeInh = valTimeInh x + valTimeInh y
        ,valTimeInd = valTimeInd x + valTimeInd y
        ,valEntries = valEntries x + valEntries y}

showVals :: [Val] -> [String]
showVals xs = [intercalate "  " $ [f valTimeTot, f valTimeInh, f valTimeInd, valId ++ " (" ++ show valEntries ++ ")"] | Val{..} <- xs]
    where
        f x = case show x of
            "0.0" -> "   -"
            "100.0" -> "99.9" -- avoid making the column bigger for a corner case
            ['0','.',x] -> [' ',' ','.',x]
            x -> replicate (4 - length x) ' ' ++ x

{-# LANGUAGE RecordWildCards, ViewPatterns #-}

module Main(main) where

import GHC.Prof
import Data.List.Extra
import Data.Char
import Data.Monoid
import Data.Tree
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Text.Lazy.IO as T
import System.Environment
import System.FilePath
import System.Directory
import Type
import Config
import Report


main :: IO ()
main = do
    [arg] <- getArgs
    Right vals <- fmap (removeZero . valFromProfile) . decode <$> T.readFile arg
    b <- doesFileExist ".profiterole.yaml"
    config <- if b then readConfig ".profiterole.yaml" else return emptyConfig
    let roots = findRoots config vals
    let vals2 =  mergeRoots $ liftRoots roots vals
    let arg0 = if takeExtension arg == ".prof" then dropExtension arg else arg
    writeFile (arg0 <.> "profiterole.txt") $ unlines $ reportText vals2
    writeFile (arg0 <.> "profiterole.html") $ unlines $ reportHTML vals2
    print $ sum $ map timeInd $ concatMap flatten vals2
    print $ sum $ map (timeInh . rootLabel) vals2


---------------------------------------------------------------------
-- TRANSFORMATIONS

removeZero :: Tree Val -> Tree Val
removeZero (Node x xs) = Node x $ map removeZero $ filter (not . isZero . rootLabel) xs
    where isZero Val{..} = timeTot == 0


-- | A root has at least two distinct parents and isn't a local binding
findRoots :: (String -> Maybe Config) -> Tree Val -> Set.Set String
findRoots config x = Map.keysSet $
    Map.filterWithKey (\k v -> case config k of
        Just Root -> True
        Just Bury -> False
        Nothing -> not (isLocal k) && Set.size v > 1) $
    Map.fromListWith (<>) $ f x
    where
        f (Node v xs) = [(name $ rootLabel x, Set.singleton $ name v) | x <- xs] ++
                        concatMap f xs
        isLocal (word1 -> (_, x)) =  any isAlpha x && '.' `elem` x

liftRoots :: Set.Set String -> Tree Val -> [Tree Val]
liftRoots = fs
    where
        fs set x = let (y,_,ys) = f set x in y:ys

        -- return (this tree, discount to apply up, new roots)
        f :: Set.Set String -> Tree Val -> (Tree Val, Double, [Tree Val])
        f set (Node x ys)
            | name x `Set.member` set = (Node x{timeInh=0,timeInd=0} [], timeInh x, fs (Set.delete (name x) set) $ Node x ys)
            | otherwise = (Node x{timeInh = timeInh x - disc} child, disc, root)
                where (child, sum -> disc, concat -> root) = unzip3 $ map (f set) ys

mergeRoots :: [Tree Val] -> [Tree Val]
mergeRoots xs = Map.elems $ Map.fromListWith f [(name $ rootLabel x, x) | x <- xs]
    where f (Node x xs) (Node y ys) = Node (mergeVal x y) $ mergeRoots $ xs ++ ys

{-# LANGUAGE RecordWildCards, OverloadedStrings, ViewPatterns #-}

module Type(
    Val(..),
    mergeVal,
    valFromProfile
    ) where

import GHC.Prof
import Data.Maybe
import Data.Scientific
import Data.Tree
import qualified Data.Text as T


data Val = Val
    {name :: String -- Name of this node
    ,timeTot :: Double -- Time spent under this node
    ,timeInh :: Double -- Time spent under this node excluding rerooted
    ,timeInd :: Double -- Time spent in this code
    ,entries :: Integer -- Number of times this node was called
    } deriving Show


mergeVal :: Val -> Val -> Val
mergeVal x y
    | name x /= name y = error $ "mergeRoots, invariant violated"
    | otherwise = Val
        {name = name x
        ,timeTot = timeTot x + timeTot y
        ,timeInh = timeInh x + timeInh y
        ,timeInd = timeInd x + timeInd y
        ,entries = entries x + entries y}


valFromProfile :: Profile -> Tree Val
valFromProfile prf = case costCentres prf of
    Nothing -> error "Failed to generate value tree from profile (no idea why - corrupt profile?)"
    Just x
        | isNothing $ costCentreTicks $ rootLabel x -> fmap toValTime x
        | otherwise -> fixTotInh $ fmap (toValTick $ (* 0.01) $ fromInteger $ totalTimeTicks $ profileTotalTime prf) x

toValTime :: CostCentre -> Val
toValTime CostCentre{..} = Val
    (T.unpack costCentreModule ++ " " ++ T.unpack costCentreName)
    inh inh (toRealFloat costCentreIndTime)
    costCentreEntries
    where inh = toRealFloat costCentreInhTime

toValTick :: Double -> CostCentre -> Val
toValTick tot cc = (toValTime cc){timeInd = fromInteger (fromJust $ costCentreTicks cc) / tot}

fixTotInh :: Tree Val -> Tree Val
fixTotInh (Node x xs) = Node x{timeTot=t, timeInh=t} ys
    where
        ys = map fixTotInh xs
        t = timeInd x + sum (map (timeInh . rootLabel) ys)

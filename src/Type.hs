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
valFromProfile = fmap toVal . fromJust . costCentres

toVal :: CostCentre -> Val
toVal CostCentre{..} = Val
    (T.unpack costCentreModule ++ " " ++ T.unpack costCentreName)
    inh inh (toRealFloat costCentreIndTime)
    costCentreEntries
    where inh = toRealFloat costCentreInhTime

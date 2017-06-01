{-# LANGUAGE RecordWildCards, OverloadedStrings, ViewPatterns #-}

module Type(
    Val(..),
    valFromProfile
    ) where

import GHC.Prof
import Data.Maybe
import Data.Scientific
import Data.Tree
import qualified Data.Text as T


data Val = Val
    {name :: String -- Name of this node
    ,timeTot :: Scientific -- Time spent under this node
    ,timeInh :: Scientific -- Time spent under this node excluding rerooted
    ,timeInd :: Scientific -- Time spent in this code
    ,entries :: Integer -- Number of times this node was called
    } deriving Show

valFromProfile :: Profile -> Tree Val
valFromProfile = fmap toVal . fromJust . costCentres

toVal :: CostCentre -> Val
toVal CostCentre{..} = Val
    (T.unpack costCentreModule ++ " " ++ T.unpack costCentreName)
    costCentreInhTime costCentreInhTime costCentreIndTime
    costCentreEntries

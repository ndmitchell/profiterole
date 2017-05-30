{-# LANGUAGE RecordWildCards, OverloadedStrings #-}

{-
data CostCentreTree = CostCentreTree
    {costCentreNodes :: IntMap CostCenter
    ,costCentreParents :: IntMap CostCenterNo
    ,costCentreChildren :: IntMap (Set CostCenter)
    ,costCenterCallSites :: 
                    costCentreCallSites :: !containers-0.5.7.1:Data.Map.Base.Map
                                              (Data.Text.Internal.Text, Data.Text.Internal.Text)
                                              (containers-0.5.7.1:Data.Set.Base.Set CostCentre),
                    costCentreAggregate :: !containers-0.5.7.1:Data.Map.Base.Map
                                              Data.Text.Internal.Text
                                              (containers-0.5.7.1:Data.Map.Base.Map
                                                 Data.Text.Internal.Text AggregateCostCentre)}
        -- Defined in `GHC.Prof.Types'
-}

import GHC.Prof
import GHC.Prof.Types
import Data.List.Extra
import Data.Monoid
import Data.Maybe
import Data.Tuple.Extra
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as T
import System.Environment


type CC = IntMap.IntMap (Maybe CostCentreNo, CostCentre)

toCC :: CostCentreTree -> CC
toCC cct = IntMap.map f $ costCentreNodes cct
    where f x = (IntMap.lookup (costCentreNo x) (costCentreParents cct), x)


main :: IO ()
main = do
    args <- getArgs
    Right prf <- decode <$> T.readFile (head args)
    let cc = deleteZeros $ toCC $ profileCostCentreTree prf
    let same = alwaysSameParent cc
    putStr $ unlines $ take 25 $ map (\(lbl,t) -> show t ++ " " ++ lbl) $
        sortOn (negate . snd) $ map (second sum) $ groupSort
        [ (T.unpack $ costCentreId v, costCentreInhTime v)
        | (k,(_,v)) <- IntMap.toList cc
        , not $ same k]

costCentreId x = costCentreModule x <> " " <> costCentreName x


-- | Does this cost center and all others with the same Id always
--   have a parent with the same Id
alwaysSameParent :: CC -> (CostCentreNo -> Bool)
alwaysSameParent cc = flip Set.member $
    Set.unions $ map snd $
    filter ((==) 1 . Set.size . fst) $
    Map.elems $ Map.fromListWith (<>)
    [ (costCentreId v, (Set.singleton $ costCentreId . snd . (cc IntMap.!) <$> p , Set.singleton k))
    | (k,(p,v)) <- IntMap.toList cc]


-- | Delete cost centers which have zero runtime
deleteZeros :: CC -> CC
deleteZeros = IntMap.filter (not . isZero . snd)
    where isZero CostCentre{..} = costCentreInhTime == 0 && costCentreInhAlloc == 0

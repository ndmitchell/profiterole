{-# LANGUAGE RecordWildCards, OverloadedStrings #-}

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


main :: IO ()
main = do
    Right Profile{..} <- decode <$> T.readFile "C:/Neil/hlint/hlint.prof"
    let cc = deleteZeros profileCostCentreTree
    let root = rootCostCentre cc
    putStr $ unlines $ map (\(lbl,t) -> show t ++ " " ++ lbl) $
        sortOn (negate . snd) $ map (second sum) $ groupSort
        [ (T.unpack $ costCentreId v, costCentreInhTime v)
        | (k,v) <- IntMap.toList $ costCentreNodes cc
        , root k]

costCentreId x = costCentreModule x <> " " <> costCentreName x

-- | Find all the cost centers which should be roots (they don't have exactly 1 parent)
rootCostCentre :: CostCentreTree -> (CostCentreNo -> Bool)
rootCostCentre cc = flip Set.notMember singleParent
    where
        singleParent = Set.fromList $ concat
            [ map costCentreNo xs
            | xs <- map Set.toList $ Map.elems $ costCentreCallSites cc
            , let parents = nubOrd $ map (costCentreId . (costCentreNodes cc IntMap.!)) $ mapMaybe (flip IntMap.lookup (costCentreParents cc) . costCentreNo) xs
            , length parents == 1]


deleteZeros :: CostCentreTree -> CostCentreTree
deleteZeros CostCentreTree{..} = CostCentreTree nodes parents children callsites Map.empty
    where
        isZero CostCentre{..} = costCentreInhTime == 0 && costCentreInhAlloc == 0
        goodId x = IntMap.member x nodes
        good = goodId . costCentreNo

        nodes = IntMap.filter (not . isZero) costCentreNodes
        parents = IntMap.filterWithKey (\k v -> goodId k && goodId v) costCentreParents
        children = IntMap.filter (not . Set.null) $ IntMap.map (Set.filter good) costCentreChildren
        callsites = Map.filter (not . Set.null) $ Map.map (Set.filter good) costCentreCallSites

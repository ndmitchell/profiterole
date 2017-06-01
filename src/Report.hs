{-# LANGUAGE RecordWildCards, OverloadedStrings, ViewPatterns #-}

module Report(displayVals) where

import Data.List.Extra
import Data.Tree
import Numeric.Extra
import Util
import Type


displayVals :: [Tree Val] -> [String]
displayVals vals =
    let vals2 =  sortOn (negate . timeInh . rootLabel) $
                 fmapForest (sortOn (negate . timeTot . rootLabel)) vals
        indent i x = x{name = replicate (i*2) ' ' ++ name x}
    in intercalate ["",""] $
        (" TOT   INH   IND" : showVals (map rootLabel $ take 25 vals2)) :
        [showVals $ flatten $ fmapTreeDepth indent x | x <- vals2]


showVals :: [Val] -> [String]
showVals xs = [intercalate "  " $ [f timeTot, f timeInh, f timeInd, name ++ " (" ++ show entries ++ ")"] | Val{..} <- xs]
    where
        f x = case showDP 1 x of
            "0.0" -> "   -"
            "100.0" -> "99.9" -- avoid making the column bigger for a corner case
            ['0','.',x] -> [' ',' ','.',x]
            x -> replicate (4 - length x) ' ' ++ x

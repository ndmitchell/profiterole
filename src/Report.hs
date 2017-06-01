{-# LANGUAGE RecordWildCards, OverloadedStrings, ViewPatterns #-}

module Report(reportText) where

import Data.List.Extra
import Data.Tree
import Numeric.Extra
import Util
import Type


presort :: [Tree Val] -> [Tree Val]
presort =
    sortOn (negate . timeInh . rootLabel) .
    fmapForest (sortOn (negate . timeTot . rootLabel))


reportText :: [Tree Val] -> [String]
reportText vals =
    let vals2 = presort vals
        indent i x = x{name = replicate (i*2) ' ' ++ name x}
    in intercalate ["",""] $
        (" TOT   INH   IND" : map (showText . rootLabel) (take 25 vals2)) :
        [map showText $ flatten $ fmapTreeDepth indent x | x <- vals2]

showText :: Val -> String
showText Val{..} = intercalate "  "
    [showDouble timeTot, showDouble timeInh, showDouble timeInd, name ++ " (" ++ show entries ++ ")"]

showDouble :: Double -> String
showDouble x = case showDP 1 x of
    "0.0" -> "   -"
    "100.0" -> "99.9" -- avoid making the column bigger for a corner case
    ['0','.',x] -> [' ',' ','.',x]
    x -> replicate (4 - length x) ' ' ++ x

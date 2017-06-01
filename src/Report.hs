{-# LANGUAGE RecordWildCards #-}

module Report(
    reportText,
    reportHTML
    ) where

import Data.List.Extra
import Data.Tree
import Data.Char
import Data.Hashable
import qualified Data.Set as Set
import Numeric.Extra
import Util
import Type


presort :: [Tree Val] -> [Tree Val]
presort =
    sortOn (negate . timeInh . rootLabel) .
    fmapForest (sortOn (negate . timeTot . rootLabel))


reportHTML :: [Tree Val] -> [String]
reportHTML vals =
    let vals2 = presort vals
        indent i x = x{name = replicate (i*2) ' ' ++ name x}
        links = Set.fromList $ map (name . rootLabel) vals2
        anchor x = "<a id='" ++ show (hash x) ++ "'></a>"
    in (["<pre>"] ++) $ (++ ["</pre>"]) $ intercalate [""] $
        (" TOT   INH   IND" : map (showHTML links . rootLabel) (take 25 vals2)) :
        [ anchor (name $ rootLabel x) :
          map (showHTML $ Set.delete (name $ rootLabel x) links) (flatten $ fmapTreeDepth indent x)
        | x <- vals2]

showHTML :: Set.Set String -> Val -> String
showHTML xs Val{..} = intercalate "  "
    [showDouble timeTot, showDouble timeInh, showDouble timeInd
    ,spc ++ (if nam `Set.member` xs then link else nam) ++ " (" ++ show entries ++ ")"]
    where
        link = "<a href='#" ++ show (hash nam) ++ "'>" ++ nam ++ "</a>"
        (spc,nam) = span isSpace name


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

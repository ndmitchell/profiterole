{-# LANGUAGE ViewPatterns #-}

module Config(emptyConfig, readConfig, Config(..)) where

import Data.List.Extra
import qualified Data.Map as Map
import System.IO.Extra


data Config = Root | Bury deriving Eq

readConfig :: FilePath -> IO (String -> Maybe Config)
readConfig file = do
    let f (stripPrefix "root: " -> Just x) = (x, Root)
        f (stripPrefix "bury: " -> Just x) = (x, Bury)
        f x = error $ "Bad config, got " ++ show x
    mp <- Map.fromList . map f .  lines <$> readFile' file
    return $ flip Map.lookup mp

emptyConfig :: String -> Maybe Config
emptyConfig _ = Nothing

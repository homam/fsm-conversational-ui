{-# LANGUAGE OverloadedStrings, FlexibleInstances, ScopedTypeVariables, RankNTypes #-}

module HLib
    ( someFunc
    )
where

import qualified Text.XML.HXT.Core as X
import qualified Data.ByteString.Char8 as Char8
import Data.String.Conversions (cs)
import qualified Data.Map as M
import qualified Data.List as L
import Data.Csv

instance ToRecord (String, M.Map String String) where
  toRecord (key, dic) = record $ toField key : map (\ (_, val) -> toField val) (L.sortOn fst $ M.toList dic)

someFunc :: IO ()
someFunc = do
  ens <- X.runX $
    X.readDocument [] "./Website_.xml"
    -- >>> root
    X.>>> X.getChildren

    X.>>> X.getChildren
    X.>>> (X.getName X.&&& (X.getChildren X.>>> (X.getName X.&&& X.deep X.getText)))

  -- let m = M.from ens
  let translations = M.map M.fromList $ M.unionsWith (++) $ map (M.fromList . (\(a, b) -> [(a, [b])])) ens
  let allLangs = L.nub . concatMap M.keys . M.elems $ translations
  let emptyLangs = M.fromList $ map (\l -> (l, "")) allLangs
  let normalizedTranslations = M.map (`M.union` emptyLangs) translations

  Char8.writeFile "./website_.csv" (cs $ encode $ M.toList normalizedTranslations)

  -- print ens
  -- Char8.putStrLn $ cs $ concatMap (\(a, (b, c)) -> a ++ b ++ c) ens
  -- Char8.putStrLn $ cs $
  -- print ens
  return ()

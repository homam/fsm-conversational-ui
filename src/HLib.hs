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
import qualified Data.ByteString as BL
import Data.Csv
import qualified Data.Vector as V
import Debug.Trace (trace)
import Data.Maybe (fromMaybe)
import Control.Applicative ((<|>))

column :: Int -> V.Vector [a] -> [a]
column i = map (!! i) . V.toList

toDic :: (Ord a, Show a) => V.Vector [a] -> M.Map a a
toDic = M.fromList . map (\ (a:b:_) -> trace (show a) (a, b) ) . V.toList

instance ToRecord (String, M.Map String String) where
  toRecord (key, dic) = record $ toField key : map (\ (_, val) -> toField val) (L.sortOn fst $ M.toList dic)

websiteXMLTranslationMatrix :: String -> IO ([String], M.Map String (M.Map String String))
websiteXMLTranslationMatrix fileName = do
  ens <- X.runX $
    X.readDocument [] fileName
    X.>>> X.getChildren
    X.>>> X.getChildren
    X.>>> (X.getName X.&&& (X.getChildren X.>>> (X.getName X.&&& X.deep X.getText)))

  let translations = M.map M.fromList $ M.unionsWith (++) $ map (M.fromList . (\(a, b) -> [(a, [b])])) ens
  let allLangs = L.nub . concatMap M.keys . M.elems $ translations

  let emptyLangs = M.fromList $ map (\l -> (l, "")) allLangs
  let normalizedTranslations = M.map (`M.union` emptyLangs) translations

  return (allLangs, normalizedTranslations)

writeWebstireTranslationMatrix :: String -> [String] -> M.Map String (M.Map String String) -> IO ()
writeWebstireTranslationMatrix path allLangs matrix = Char8.writeFile path (cs $ encode $ ("_", M.fromList $ map (\x -> (x,x)) allLangs) : M.toList matrix)

someFunc :: IO ()
someFunc = do
  (allLangs, normalizedTranslations) <- websiteXMLTranslationMatrix "./Website_.xml"
  -- Char8.writeFile "./website_.csv" (cs $ encode $ ("_", M.fromList $ map (\x -> (x,x)) allLangs) : M.toList normalizedTranslations)

  csvData <- cs <$> BL.readFile "./fi.csv"
  case decode NoHeader csvData of
    Left err -> print err
    Right vs -> do
      let english1 = column 0 vs :: [String]
      let dic = toDic vs :: M.Map String String
      let updatedT = updateTranslations dic normalizedTranslations
      writeWebstireTranslationMatrix "./website_.csv" allLangs updatedT

  return ()

empty [] = True
empty _ = False

updateTranslations :: M.Map String String -> M.Map String (M.Map String String) -> M.Map String (M.Map String String)
updateTranslations dic = M.map (\d -> M.update (\ e ->
  let en = fromMaybe "-" (M.lookup "en" d)
  in (if not (empty e) then Just e else Nothing) <|> (M.lookup en dic <|> Just "-") ) "fi" d)

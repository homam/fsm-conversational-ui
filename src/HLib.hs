{-# LANGUAGE OverloadedStrings, FlexibleInstances, ScopedTypeVariables, RankNTypes #-}

module HLib
    ( main
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

type Language = String

toDic :: (Ord a, Show a) => V.Vector [a] -> M.Map a a
toDic = M.fromList . map (\ (a:b:_) -> (a, b) ) . V.toList

instance ToRecord (String, M.Map String String) where
  toRecord (key, dic) = record $ toField key : map (\ (_, val) -> toField val) (L.sortOn fst $ M.toList dic)

websiteXMLTranslationMatrix :: String -> IO ([Language], M.Map String (M.Map Language String))
websiteXMLTranslationMatrix fileName = do
  nodes <- X.runX $
    X.readDocument [] fileName
    X.>>> X.getChildren
    X.>>> X.getChildren
    X.>>> (X.getName X.&&& (X.getChildren X.>>> (X.getName X.&&& X.deep X.getText)))

  let translations = M.map M.fromList $ M.unionsWith (++) $ map (M.fromList . (\(a, b) -> [(a, [b])])) nodes
  let allLangs = L.nub . concatMap M.keys . M.elems $ translations

  let emptyLangs = M.fromList $ map (\l -> (l, "")) allLangs
  let normalizedTranslations = M.map (`M.union` emptyLangs) translations

  return (allLangs, normalizedTranslations)

writeWebstireTranslationMatrix :: String -> [Language] -> M.Map String (M.Map Language String) -> IO ()
writeWebstireTranslationMatrix path allLangs matrix = Char8.writeFile path (cs . encode $ ("_", M.fromList $ map (\x -> (x,x)) allLangs) : M.toList matrix)

updateTranslations :: M.Map String String -> M.Map String (M.Map Language String) -> M.Map String (M.Map Language String)
updateTranslations dic = M.map (\d -> M.update (\ e ->
  let en = fromMaybe "-" (M.lookup "en" d)
  in (if not (null e) then Just e else Nothing) <|> (M.lookup en dic <|> Just "-") ) "fi" d)


main :: IO ()
main = do
  (allLangs, normalizedTranslations) <- websiteXMLTranslationMatrix "./Website_.xml"
  -- Char8.writeFile "./website_.csv" (cs $ encode $ ("_", M.fromList $ map (\x -> (x,x)) allLangs) : M.toList normalizedTranslations)

  csvData <- cs <$> BL.readFile "./fi.csv"
  case decode NoHeader csvData of
    Left err -> print err
    Right vs -> do
      let englishFinnishDic = toDic vs :: M.Map String String -- english finnish
      let updatedTranslationMatrix = updateTranslations englishFinnishDic normalizedTranslations
      writeWebstireTranslationMatrix "./website_.csv" allLangs updatedTranslationMatrix

  return ()

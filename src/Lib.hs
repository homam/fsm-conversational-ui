{-# LANGUAGE OverloadedStrings, FlexibleInstances, ScopedTypeVariables, RankNTypes #-}

module Lib
    ( someFunc
    )
where

import qualified Data.Map as M
import Data.List.Split (splitOn, splitWhen)
import qualified Data.Text as T
import Data.Csv
import qualified Data.Vector as V
import Data.Either
import Text.Read (readEither)
import qualified Text.XML.Light.Input as XI
import qualified Text.XML.Light as X
import Debug.Trace (trace)
import qualified Data.List as L
import Data.ByteString.UTF8 (fromString)
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BL
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.UTF8 as UTF8
import Data.String.Conversions (cs)


someFunc :: IO ()
someFunc = do
  csvData <- cs . UTF8.toString <$> BL.readFile "./fi.csv"
  case decode NoHeader csvData of
    Left err -> print err
    Right vs -> -- putStrLn $ (V.head v) !! 1
      -- V.mapM_ (putStrLn . head) vs
      let english = column 0 vs :: [String]
      in print english
      -- mapM_ (putStrLn ) (column 0 vs)

column :: Int -> V.Vector [a] -> [a]
column i = map (!! i) . V.toList

parseTranslationXml :: [X.Content] -> M.Map String (M.Map String String)
parseTranslationXml = M.unions . map (go 0 M.empty) where

  go :: Int -> M.Map String (M.Map String String) -> X.Content -> M.Map String (M.Map String String)

  go 0 dic (X.Elem d) = M.unions $ map (go 1 dic) (X.elContent d)
  go 1 dic (X.Elem d) = M.insert (X.qName $ X.elName d) (M.unions $ map (go2 M.empty) (X.elContent d)) dic
  go _ dic _ = dic

  go2 :: M.Map String String -> X.Content -> M.Map String String
  go2 dic (X.Elem d) = M.insert (X.qName $ X.elName d) (X.showContent $ head $ X.elContent d) dic
  go2 dic _ = dic

  -- go 2 dic _

duck :: IO ()
duck = do
  xmlData <- BL.readFile "./Website_.xml"
  let content = XI.parseXML xmlData
  let translations = parseTranslationXml content

  csvData <- cs <$> BL.readFile "./fi.csv"
  case decode NoHeader csvData of
    Left err -> print err
    Right vs -> do
      let english1 = column 0 vs :: [String]
      let english2 = M.keys translations
      print $ english1 `L.intersect` english2


-- String = record name
-- Map String = language, String = translated value
instance ToRecord (String, M.Map String String) where
  toRecord (key, dic) = record $ toField key : map (\ (_, val) -> toField val) (L.sortOn fst $ M.toList dic)

websiteXMLTranslationMatrix :: IO ()
websiteXMLTranslationMatrix = do
  xmlData <- Char8.readFile "./Website_.xml"

  let content = XI.parseXML xmlData

  Char8.putStrLn $ Char8.concat $ map (cs . X.showContent) content

  let translations = parseTranslationXml content


  let allLangs = L.nub . concatMap M.keys . M.elems $ translations
  let emptyLangs = M.fromList $ map (\l -> (l, "")) allLangs
  let normalizedTranslations = M.map (`M.union` emptyLangs) translations
  -- Char8.putStrLn $ cs $ encode $ M.toList normalizedTranslations
  -- Char8.writeFile "./website_.csv" (cs $ encode $ M.toList normalizedTranslations)

  print ""
  -- mapM_ (go 0) content where
  --   go depth (X.Elem d) = putStrLn ("depth " ++ (show depth) ++ " " ++ show (X.elName d)) >> mapM_ (go $ depth + 1) (X.elContent d)
  --   go depth (X.Text d) = return ()

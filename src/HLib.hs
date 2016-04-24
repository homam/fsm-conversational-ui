{-# LANGUAGE OverloadedStrings, FlexibleInstances, ScopedTypeVariables, RankNTypes #-}

module HLib
    (
        websiteCSVToWebsiteXML
      , websiteXMLToWebsiteCSV
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
import Control.Arrow ((>>>), (&&&), (<<<), (<<^))
import qualified Text.XML.HXT.Parser.XmlParsec as XP

type Language = String
type TranslationMatrix = M.Map String (M.Map Language String)
type Dic = M.Map String String
-- data Dictionary = Dictionary {keyLang :: String, valueLang :: String, dictionary :: M.Map String String}
data Dictionary a = Dictionary {keyLang :: String, valueLang :: String, dictionary :: a}

missingTranslation :: String
missingTranslation = "-"

toDic :: (Ord a) => V.Vector [a] -> M.Map a a
toDic = M.fromList . map (\ (a:b:_) -> (a, b) ) . V.toList

toMatrix :: (Ord a) => V.Vector [a] -> M.Map a (M.Map a a)
toMatrix = process . V.toList
  where
    process :: (Ord a) => [[a]] -> M.Map a (M.Map a a)
    process (h:rest) =
      M.fromList $ map (\(key:vals) -> (key, M.fromList $ langs `zip` vals) ) rest
      where
        langs = drop 1 h

instance ToRecord (String, M.Map String String) where
  toRecord (key, dic) = record $ toField key : map (\ (_, val) -> toField val) (L.sortOn fst $ M.toList dic)

websiteXMLTranslationMatrix :: FilePath -> IO ([Language], TranslationMatrix)
websiteXMLTranslationMatrix fileName = do
  nodes <- X.runX $
    X.readDocument [] fileName
    >>> X.getChildren
    >>> X.getChildren
    >>> (X.getName &&& (X.getChildren >>> (X.getName &&& X.deep X.getText)))

  let translations = M.map M.fromList $ M.unionsWith (++) $ map (M.fromList . (\(a, b) -> [(a, [b])])) nodes
  let allLangs = L.nub . concatMap M.keys . M.elems $ translations

  let emptyLangs = M.fromList $ map (\l -> (l, "")) allLangs
  let normalizedTranslations = M.map (`M.union` emptyLangs) translations

  return (allLangs, normalizedTranslations)

writeWebstireTranslationMatrixCSV :: FilePath -> [Language] -> TranslationMatrix -> IO ()
writeWebstireTranslationMatrixCSV path allLangs matrix = Char8.writeFile path (cs . encode $ ("_", M.fromList $ map (\x -> (x,x)) allLangs) : M.toList matrix)

-- usage: websiteXMLToWebsiteCSV "./Website_.xml" "./Website_.csv"
websiteXMLToWebsiteCSV :: FilePath -> FilePath -> IO ()
websiteXMLToWebsiteCSV xmlPath csvPath = websiteXMLTranslationMatrix xmlPath >>= uncurry (writeWebstireTranslationMatrixCSV csvPath)

-- usage: websiteCSVToWebsiteXML "./Website_.csv" "./Updated-Website_.xml"
websiteCSVToWebsiteXML :: FilePath -> FilePath -> IO ()
websiteCSVToWebsiteXML csvPath xmlPath = do
  csvData <- cs <$> BL.readFile csvPath
  case decode NoHeader csvData of
    Left err -> error err
    Right (vs :: V.Vector [String]) -> do
      _ <- writeWebsiteCSV xmlPath (toMatrix vs)
      return ()

translationMatrixToWebsiteXML :: X.ArrowXml a => TranslationMatrix -> a X.XmlTree X.XmlTree
translationMatrixToWebsiteXML texts = X.mkelem "texts" [] children
  where
    children = map (\ (text, dic) -> X.mkelem text [] (dicChildren dic)) $ M.toList texts
    dicChildren = concatMap (\ (lang, text) -> if isEmpty text then [] else [X.mkelem lang [] [cDataTxt text]]) . M.toList

    cDataTxt :: X.ArrowXml a => String -> a n X.XmlTree
    cDataTxt text = if '<' `L.elem` text then X.constA text >>> X.mkCdata else X.txt text

    isEmpty x = null x || missingTranslation == x

writeWebsiteCSV :: FilePath -> TranslationMatrix -> IO [X.XmlTree]
writeWebsiteCSV file csv = X.runX $
  X.root [] [translationMatrixToWebsiteXML csv]
  >>>
  X.indentDoc
  >>>
  X.putXmlDocument True ""

  >>>
  X.writeDocument [X.withSubstHTMLEntities False, X.withIndent X.yes] file


--

updateTranslationMatrixWithDictionary :: Dictionary Dic -> TranslationMatrix -> TranslationMatrix
updateTranslationMatrixWithDictionary dics = M.map (\d -> M.update (\ e ->
  let en = fromMaybe "-" (M.lookup keyL d)
  in (if not (null e) then Just e else Nothing) <|> (M.lookup en dic <|> Just missingTranslation) ) valL d)
  where
    dic = dictionary dics
    keyL = keyLang dics
    valL = valueLang dics

websiteXMLToWebsiteCSVUsingDictionaryCSV :: Dictionary FilePath -> FilePath -> FilePath -> IO ()
websiteXMLToWebsiteCSVUsingDictionaryCSV dics xmlPath csvPath = do

  (allLangs, normalizedTranslations) <- websiteXMLTranslationMatrix xmlPath
  -- Char8.writeFile "./website_.csv" (cs $ encode $ ("_", M.fromList $ map (\x -> (x,x)) allLangs) : M.toList normalizedTranslations)

  csvData <- cs <$> BL.readFile dic
  case decode NoHeader csvData of
    Left err -> print err
    Right vs -> do
      let englishFinnishDic = Dictionary {keyLang = keyL, valueLang = valL, dictionary = toDic vs}
      let updatedTranslationMatrix = updateTranslationMatrixWithDictionary englishFinnishDic normalizedTranslations
      writeWebstireTranslationMatrixCSV csvPath allLangs updatedTranslationMatrix

  return ()

  where
    dic = dictionary dics
    keyL = keyLang dics
    valL = valueLang dics

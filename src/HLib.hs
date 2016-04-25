{-# LANGUAGE OverloadedStrings, FlexibleInstances, ScopedTypeVariables, RankNTypes, FlexibleContexts #-}

module HLib
    (
        websiteCSVToWebsiteXML
      , pipeWebsiteCSVToWebsiteXML
      , websiteXMLToWebsiteCSV
      , pipeWebsiteXMLToWebsiteCSV
    )
where

import Control.Monad (void)
import qualified Text.XML.HXT.Core as X
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy.Internal as BLI
import Data.String.Conversions (cs, ConvertibleStrings)
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
data Dictionary a = Dictionary {keyLang :: String, valueLang :: String, dictionary :: a}

missingTranslation :: String
missingTranslation = "-"

----------------------------------------
-- XML to CSV Conversion

instance ToRecord (String, M.Map String String) where
  toRecord (key, dic) = record $ toField key : map (\ (_, val) -> toField val) (L.sortOn fst $ M.toList dic)

websiteXMLTranslationMatrix :: FilePath -> IO ([Language], TranslationMatrix)
websiteXMLTranslationMatrix fileName = processWebsiteXMLTranslationMatrix (X.readDocument [] fileName)


processWebsiteXMLTranslationMatrix reader = do
  nodes <- X.runX $
    reader
    >>> X.getChildren
    >>> X.getChildren
    >>> (X.getName &&& (X.getChildren >>> (X.getName &&& X.deep X.getText)))

  let translations = M.map M.fromList $ M.unionsWith (++) $ map (M.fromList . (\(a, b) -> [(a, [b])])) nodes
  let allLangs = L.nub . concatMap M.keys . M.elems $ translations

  let emptyLangs = M.fromList $ map (\l -> (l, "")) allLangs
  let normalizedTranslations = M.map (`M.union` emptyLangs) translations

  return (allLangs, normalizedTranslations)


processWebsiteTranslationMatrixCSV :: (BL.ByteString -> IO ()) -> [Language] -> TranslationMatrix -> IO ()
processWebsiteTranslationMatrixCSV act allLangs matrix = act (cs . encode $ ("_", M.fromList $ map (\x -> (x,x)) allLangs) : M.toList matrix)

writeWebstireTranslationMatrixCSV :: FilePath -> [Language] -> TranslationMatrix -> IO ()
writeWebstireTranslationMatrixCSV csvPath = processWebsiteTranslationMatrixCSV (Char8.writeFile csvPath)

-- usage: websiteXMLToWebsiteCSV "./Website_.xml" "./Website_.csv"
websiteXMLToWebsiteCSV :: FilePath -> FilePath -> IO ()
websiteXMLToWebsiteCSV xmlPath csvPath = websiteXMLTranslationMatrix xmlPath >>= uncurry (writeWebstireTranslationMatrixCSV csvPath)

pipeWebsiteXMLToWebsiteCSV :: String -> IO ()
pipeWebsiteXMLToWebsiteCSV content = processWebsiteXMLTranslationMatrix (X.readString [] content) >>= uncurry (processWebsiteTranslationMatrixCSV Char8.putStrLn)



----------------------------------------
-- CSV to XML conversion

toMatrix :: (Ord a) => V.Vector [a] -> M.Map a (M.Map a a)
toMatrix = process . V.toList
  where
    process :: (Ord a) => [[a]] -> M.Map a (M.Map a a)
    process (h:rest) =
      M.fromList $ map (\(key:vals) -> (key, M.fromList $ langs `zip` vals) ) rest
      where
        langs = drop 1 h

websiteCSVToWebsiteXML :: FilePath -> FilePath -> IO ()
websiteCSVToWebsiteXML csvPath xmlPath = do
  csvData <- BL.readFile csvPath
  processWebsiteCSVToXml (writeWebsiteCSV xmlPath . toMatrix) csvData

pipeWebsiteCSVToWebsiteXML :: ConvertibleStrings a BLI.ByteString => a -> IO ()
pipeWebsiteCSVToWebsiteXML = processWebsiteCSVToXml (pipeWebsiteCSV . toMatrix)

processWebsiteCSVToXml act csvData =
  case decode NoHeader $ cs csvData of
    Left err -> error err
    Right (vs :: V.Vector [String]) -> void $ act vs

translationMatrixToWebsiteXML :: X.ArrowXml a => TranslationMatrix -> a X.XmlTree X.XmlTree
translationMatrixToWebsiteXML texts = X.mkelem "texts" [] children
  where
    children = map (\ (text, dic) -> X.mkelem text [] (dicChildren dic)) $ M.toList texts
    dicChildren = concatMap (\ (lang, text) -> if isEmpty text then [] else [X.mkelem lang [] [cDataTxt text]]) . M.toList

    cDataTxt :: X.ArrowXml a => String -> a n X.XmlTree
    cDataTxt text = if '<' `L.elem` text then X.constA text >>> X.mkCdata else X.txt text

    isEmpty x = null x || missingTranslation == x

writeWebsiteCSV :: FilePath -> TranslationMatrix -> IO [X.XmlTree]
writeWebsiteCSV file = processWebsiteCSV (X.writeDocument [X.withSubstHTMLEntities False, X.withIndent X.yes] file)

pipeWebsiteCSV :: TranslationMatrix -> IO [X.XmlTree]
pipeWebsiteCSV = processWebsiteCSV (X.putXmlDocument True "")

processWebsiteCSV :: X.IOSLA (X.XIOState ()) X.XmlTree c -> TranslationMatrix -> IO [c]
processWebsiteCSV cont csv = X.runX $
  X.root [] [translationMatrixToWebsiteXML csv]
  >>>
  X.indentDoc
  >>>
  cont






-- One-time utilities

toDic :: (Ord a) => V.Vector [a] -> M.Map a a
toDic = M.fromList . map (\ (a:b:_) -> (a, b) ) . V.toList

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

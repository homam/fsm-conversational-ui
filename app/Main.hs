module Main where

import HLib

main :: IO ()
main = do
  putStrLn "Usage: "
  putStrLn "websiteXMLToWebsiteCSV \"./Website_.xml\" \"./Website_.csv\""
  putStrLn "websiteCSVToWebsiteXML \"./Website_.csv\" \"./Updated-Website_.xml\""

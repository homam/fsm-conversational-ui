module Main where

import HLib
import System.Environment
import Options.Applicative
import Data.Monoid (mconcat)
import System.Directory (doesFileExist)



data CommandOptions = CommandOptions {input :: String, output :: String}
data Command = CsvToXml CommandOptions | XmlToCsv CommandOptions




optionsParser :: Parser CommandOptions
optionsParser = CommandOptions <$>
      strOption (
       long "input"
    <> short 'i'
    <> metavar "FILE"
    <> help "Read input from FILE" )
  <*> strOption (
       long "output"
    <> short 'o'
    <> metavar "FILE"
    <> help "Write output to FILE" )

csvToXmlParser :: Parser Command
csvToXmlParser = CsvToXml <$> optionsParser

xmlToCsvParser :: Parser Command
xmlToCsvParser = XmlToCsv <$> optionsParser

doIf :: IO Bool -> IO () -> IO () -> IO ()
doIf cond no yes = do
  can <- cond
  if can then yes else no

doIfInputOutput :: String -> String -> (String -> String -> IO ()) -> IO ()
doIfInputOutput input output action =
  doIf
    (doesFileExist input)
    (putStrLn $ "Input file is missing: " ++ input)
    $ doIf
      (not <$> doesFileExist output)
      (putStrLn $ "Output file is already there " ++ output)
      $ action input output

doCommand :: Command -> IO ()
doCommand (CsvToXml (CommandOptions input output)) = doIfInputOutput input output websiteCSVToWebsiteXML
doCommand (XmlToCsv (CommandOptions input output)) = doIfInputOutput input output websiteXMLToWebsiteCSV

commandParser :: ParserInfo Command
commandParser = info commands $ progDesc "Use ma-csv-xml-exe csv to convert CSV to XML.\nUse ma-csv-xml-exe xml to convert XML to CSV" where
  commands = subparser $ mconcat [
      command "csv" (info csvToXmlParser (progDesc "Covert CSV to XML"))
    , command "xml" (info xmlToCsvParser (progDesc "Covert XML to CSV"))
    ]

main :: IO ()
main = execParser commandParser >>= doCommand

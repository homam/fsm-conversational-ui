{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, DeriveGeneric #-}

module Weather
  (getWeather, Weather(..))
where

import Network.Wreq
import Control.Lens
import qualified Data.Text as T
import Data.Aeson
import Data.Typeable
import Data.Data
import GHC.Generics


data WeatherReport = WeatherReport { main :: WeatherMain, weather :: [WeatherInstance] } deriving (Data, Typeable, Show, Generic)
data WeatherInstance = WeatherInstance { id :: Integer, description :: String, icon :: String } deriving (Data, Typeable, Show, Generic)
data WeatherMain = WeatherMain { temp :: Float } deriving (Data, Typeable, Show, Generic)

instance ToJSON WeatherInstance where
  toJSON = genericToJSON defaultOptions
instance FromJSON WeatherInstance

instance ToJSON WeatherReport where
  toJSON = genericToJSON defaultOptions
instance FromJSON WeatherReport

instance ToJSON WeatherMain where
  toJSON = genericToJSON defaultOptions
instance FromJSON WeatherMain

data Weather = Weather {  weatherDescription :: String, weatherTemp :: Float } deriving (Read, Show)

getWeather :: String -> IO (Maybe Weather)
getWeather city = do
  r <- get $ "http://api.openweathermap.org/data/2.5/weather?appid=cac462b7a568be8d2f835d02c28a49e3&units=metric&q=" ++ city
  let a = r ^. responseBody
  let b = decode a :: Maybe WeatherReport
  return $ Weather <$> ((description . head . weather) <$> b) <*> ((temp . main) <$> b)

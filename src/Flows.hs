module Flows

where

import qualified Data.Map as M
import qualified Data.List as L
import HLib
import qualified Weather as W

data NodeId = NodeHello | NodeWeatherUnknownCity | NodeWeatherKnownCity deriving (Eq, Ord, Show, Read)
data ConversationId = WeatherConversation
data UserState = UserState {
  userAllMessages :: [String],
  userName :: String,
  userCity :: String
}

nodeHello :: Node NodeId UserState
nodeHello = ResponseNode {
  nodeId = NodeHello,
  question = "Hello there",
  receive = \ input ustate -> ComputationResult $ do
    a <- toEitherM APIError (W.getWeather input)
    return $ (\s -> (show s, ustate)) <$> a
}

nodeWeatherKnownCity :: Node NodeId UserState
nodeWeatherKnownCity = AutoExecNode {
  nodeId = NodeWeatherKnownCity,
  exec = \ ustate -> ComputationResult $ do
    let ucity = userCity ustate
    a <- toEitherM APIError (W.getWeather ucity)
    return $ (\s -> ("Weather in " ++ ucity ++ show s, ustate)) <$> a
}

nodeWeatherUnknownCity :: Node NodeId UserState
nodeWeatherUnknownCity = ResponseNode {
  nodeId = NodeWeatherUnknownCity,
  question = "Which city do you want to get the weather data for? (we'll save the city in your profile for future)",
  receive = \ input ustate -> ComputationResult $ do
    a <- toEitherM APIError (W.getWeather input)
    return $ (\s -> (show s, ustate {userCity = input} )) <$> a
}

weatherConversation :: Conversation ConversationId NodeId UserState
weatherConversation = Conversation {
  conversationId = WeatherConversation,
  starter = \ us -> nodeWeatherUnknownCity,
  edges = M.fromList [
    (nodeWeatherUnknownCity, \ us -> NodeTarget nodeWeatherKnownCity)
  ]
}

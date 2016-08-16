module Flows

where

import HLib

data NodeId = NodeHello | NodeWeatherCity
data UserState = UserState {
  userAllMessages :: [String],
  userName :: String,
  userCity :: String
}

nodeHello :: Node NodeId UserState
nodeHello = Node {
  nodeId = NodeHello,
  question = "Hello there",
  receive = \ input ustate -> undefined
}

nodeWeatherCity :: Node NodeId UserState
nodeWeatherCity = Node {
  nodeId = NodeWeatherCity,
  question = "Which city?",
  receive = \ input ustate -> undefined
}

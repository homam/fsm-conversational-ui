module Node
(
  Node(..), NodeId(..),
  FSM(..), FSMId(..),
  Edge(..),
  UFSM(..)
)
where

import qualified Data.Map as M

data NodeId = NodeWhatIsYourWeight | NodeWhatIsYourHeight deriving (Read, Show, Eq, Ord)
data FSMId = FSMSizeSelection deriving (Read, Show, Eq, Ord)


data EdgeTarget userState = Inside (Node userState) | Outside (FSM userState)


-- Node

data Node userState = Node {
  nodeId :: NodeId,
  ask :: String,
  receive :: String -> userState -> Either String userState
}
instance Eq (Node userState) where
  n1 == n2 = nodeId n1 == nodeId n2
instance Ord (Node userState) where
  n1 `compare` n2 = nodeId n1 `compare` nodeId n2


data Edge userState = Edge {
  node :: Node userState,
  exec :: userState -> Maybe (Node userState) -- EdgeTarget userState
}

data FSM userState = FSM {
  fsmId :: FSMId,
  edges :: M.Map NodeId (Edge userState)
}

data UFSM userState = UFSM {
  fsm :: FSM userState,
  currentNode :: Node userState,
  answered :: Bool,
  userState :: userState
}

module Node
(
  Node(..),
  Conversation(..),
  Edge(..),
  UConversation(..),
  findEdge, findEdgeUConversation, makeConversation
)
where

import qualified Data.Map as M

-- data EdgeTarget us = Inside (Node us) | Outside (Conversation us)


-- Node
-- @nid@ :: NodeId
-- @us@ :: UserState
data Node nid us = Node {
  nodeId :: nid,
  question :: String,
  receive :: String -> us -> Either String us
}

instance (Eq nid) => Eq (Node nid us) where
  n1 == n2 = nodeId n1 == nodeId n2

instance (Ord nid) => Ord (Node nid us) where
  n1 `compare` n2 = nodeId n1 `compare` nodeId n2


-- Edge
data Edge nid us = Edge {
  node :: Node nid us,
  exec :: us -> Maybe (Node nid us) -- EdgeTarget us
}

-- Conversation
data Conversation cid nid us = Conversation {
  conversationId :: cid,
  edges :: M.Map nid (Edge nid us)
}

data UConversation cid nid us = UConversation {
  conversation :: Conversation cid nid us,
  currentNode :: Node nid us,
  answered :: Bool,
  userState :: us
}


findEdge :: (Ord nid) => nid -> Conversation cid nid us -> Maybe (Edge nid us)
findEdge nodeId conversation = M.lookup nodeId (edges conversation)

findEdgeUConversation :: (Ord nid) => UConversation cid nid us -> Maybe (Edge nid us)
findEdgeUConversation user = findEdge (nodeId $ currentNode user) (conversation user)

makeConversation :: (Ord nid) => cid -> [Edge nid us] -> Conversation cid nid us
makeConversation convId list  = Conversation convId (M.fromList $ map (\e -> (nodeId $ node e, e)) list)

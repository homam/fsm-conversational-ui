{-# LANGUAGE MultiParamTypeClasses #-}
module HLib
where

import qualified Data.Map as M
import Control.Monad.Trans
type Question = String
data ErrorType =  InvalidInputError | EdgeNotFoundError


toEither :: a -> Maybe b -> Either a b
toEither _ (Just b) = Right b
toEither a _ = Left a

-- Node
-- @nid@ :: NodeId
-- @us@ :: UserState
data Node nid us = Node {
  nodeId :: nid,
  question :: Question,
  receive :: String -> us -> Either ErrorType us
}

instance (Eq nid) => Eq (Node nid us) where
  n1 == n2 = nodeId n1 == nodeId n2

instance (Ord nid) => Ord (Node nid us) where
  n1 `compare` n2 = nodeId n1 `compare` nodeId n2

data EdgeTarget cid nid us = EOC | NodeTarget (Node nid us) | ConversationTarget (Conversation cid nid us)

data Conversation cid nid us = Conversation {
  conversationId :: cid,
  starter :: Node nid us,
  edges :: M.Map (Node nid us) (us -> EdgeTarget cid nid us)
}

data User cid nid us = User {
  conversations :: [(Conversation cid nid us, Node nid us)],
  userState :: us
}

processMessage ::  (Ord nid) => User cid nid us -> String -> Either ErrorType (us, EdgeTarget cid nid us)
processMessage user input =
  let (conv, node) = head $ conversations user
      ustate = userState user
      edge = toEither EdgeNotFoundError (M.lookup node (edges conv))
  in do
    ef <- edge
    ustate' <- receive node input ustate
    return (ustate', ef ustate')

updateUser :: User cid nid us -> (us, EdgeTarget cid nid us) -> User cid nid us
updateUser user (_, EOC) = user { conversations = drop 1 (conversations user) }
updateUser user (_, NodeTarget node) = let convs = conversations user in
  user { conversations = (fst $ head convs, node) : tail convs }
updateUser user (ustate', ConversationTarget nconv) = user {
    conversations = (nconv, starter nconv) : conversations user,
    userState = ustate'
  }

processMessageAndUpdateUser :: (Ord nid) => User cid nid us -> String -> Either ErrorType (User cid nid us)
processMessageAndUpdateUser user input = updateUser user <$> processMessage user input




data SerializableUser cid nid us = SerializableUser {
  serializableConversations :: [(cid, nid)],
  serializableUserState :: us
}

processSerializedUserMessageAndUpdateUser :: (Monad f, Monad m, Ord nid) =>
   (suser -> m (f (User cid nid us))) ->
   suser -> String -> m (f (Either ErrorType (User cid nid us)))
processSerializedUserMessageAndUpdateUser userDeserializer suser input = do
  euser <- userDeserializer suser
  return $ (`processMessageAndUpdateUser` input) <$> euser


-- needs: (a -> m b) -> (c -> m d) -> [(a, c)] -> m [(b, d)]
defaultUserDeserializer :: (Monad m) => (cid -> m (Conversation cid nid us)) -> (nid -> m (Node nid us)) -> SerializableUser cid nid us -> m (User cid nid us)
defaultUserDeserializer conversationGetter nodeGetter suser = undefined

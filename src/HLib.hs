{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts #-}
module HLib
(
  Node(..), Conversation(..), SerializableUser(..),
  processSerializedUserMessageAndUpdateUser
)
where

import qualified Data.Map as M
import Control.Monad.Trans
import Control.Arrow ((***))
type Question = String
data ErrorType =  InvalidInputError | EdgeNotFoundError | ConversationNotFound | NodeNotFound


toEither :: a -> Maybe b -> Either a b
toEither _ (Just b) = Right b
toEither a _ = Left a

mapMTuples :: (Monad m) => (a -> m b) -> (c -> m d) -> [(a, c)] -> m [(b, d)]
mapMTuples f g = mapM (sequenceTuple . (f *** g)) where
  sequenceTuple :: (Monad m) => (m a, m b) -> m (a, b)
  sequenceTuple (ma, mb) = (,) <$> ma <*> mb


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

defaultUserDeserializer :: (Monad m) => (cid -> m (Conversation cid nid us)) -> (nid -> m (Node nid us)) -> SerializableUser cid nid us -> m (User cid nid us)
defaultUserDeserializer conversationGetter nodeGetter suser = do
  convs <- mapMTuples conversationGetter nodeGetter (serializableConversations suser)
  return $ User convs (serializableUserState suser)

type UserId = String

-- main export
processSerializedUserMessageAndUpdateUser :: (Monad m, Ord nid) =>
  (cid -> Maybe (Conversation cid nid us)) ->
  (nid -> Maybe (Node nid us)) ->
  (UserId -> m (Either ErrorType (SerializableUser cid nid us))) ->
  UserId -> String -> m (Either ErrorType (User cid nid us))
processSerializedUserMessageAndUpdateUser conversationGetter nodeGetter userGetter userId input =
  processEitherUserMessageAndUpdateUser <$> userGetter userId where

    processEitherUserMessageAndUpdateUser fuser = do
      suser <- fuser
      user <- defaultUserDeserializer (toEither ConversationNotFound . conversationGetter) (toEither NodeNotFound . nodeGetter) suser
      processMessageAndUpdateUser user input

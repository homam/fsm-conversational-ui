{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, DeriveFunctor, GeneralizedNewtypeDeriving #-}
module HLib
(
  Node(..), Conversation(..), SerializableUser(..), EdgeTarget(..),
  ComputationResult(..),
  ErrorType(..),
  processSerializedUserMessageAndUpdateUser,
  toEither, toEitherM
)
where

import qualified Data.Map as M
import Control.Monad.Trans
import Control.Arrow ((***))
type Question = String
type Answer = String
data ErrorType =  InvalidInputError | EdgeNotFoundError | ConversationNotFound | NodeNotFound | APIError


toEither :: a -> Maybe b -> Either a b
toEither _ (Just b) = Right b
toEither a _ = Left a

toEitherM :: (Monad m) => a -> m (Maybe b) -> m (Either a b)
toEitherM = fmap . toEither

toComputationResult :: a -> Maybe b -> ComputationResult a b
toComputationResult _ (Just b) = return b
toComputationResult a _ = ComputationResult $ return (Left a)

mapMTuples :: (Monad m) => (a -> m b) -> (c -> m d) -> [(a, c)] -> m [(b, d)]
mapMTuples f g = mapM (sequenceTuple . (f *** g)) where
  sequenceTuple :: (Monad m) => (m a, m b) -> m (a, b)
  sequenceTuple (ma, mb) = (,) <$> ma <*> mb

newtype ComputationResult a b = ComputationResult (IO (Either a b)) deriving (Functor)

instance Applicative (ComputationResult a) where
  pure = ComputationResult . return . Right
  (ComputationResult mf) <*> (ComputationResult ma) = ComputationResult $ do
    f <- mf
    a <- ma
    return $ f <*> a

instance Monad (ComputationResult a) where
  return = pure
  (ComputationResult ma) >>= f = ComputationResult $ do
    a <- ma
    let b = f <$> a
    case b of
      (Left e) -> return $ Left e
      (Right (ComputationResult x)) -> x

-- Node
-- @nid@ :: NodeId
-- @us@ :: UserState
data Node nid us = ResponseNode {
  nodeId :: nid,
  question :: Question,
  receive :: String -> us -> ComputationResult ErrorType (Answer, us)
} | AutoExecNode { nodeId :: nid, exec :: us -> ComputationResult ErrorType (Answer, us) }

instance (Eq nid) => Eq (Node nid us) where
  n1 == n2 = nodeId n1 == nodeId n2

instance (Ord nid) => Ord (Node nid us) where
  n1 `compare` n2 = nodeId n1 `compare` nodeId n2

data EdgeTarget cid nid us = EOC | NodeTarget (Node nid us) | ConversationTarget (Conversation cid nid us)

data Conversation cid nid us = Conversation {
  conversationId :: cid,
  starter :: us -> Node nid us,
  edges :: M.Map (Node nid us) (us -> EdgeTarget cid nid us)
}

data User cid nid us = User {
  conversations :: [(Conversation cid nid us, Node nid us)],
  userState :: us
}

processMessage ::  (Ord nid) => User cid nid us -> String -> ComputationResult ErrorType (Answer, us, EdgeTarget cid nid us)
processMessage user input =
  let (conv, node) = head $ conversations user
      ustate = userState user
      edge = toComputationResult EdgeNotFoundError (M.lookup node (edges conv))
  in do
    ef <- edge
    (ans, ustate') <- receive node input ustate
    return (ans, ustate', ef ustate')


updateUser :: User cid nid us -> (us, EdgeTarget cid nid us) -> User cid nid us
updateUser user (_, EOC) = user { conversations = drop 1 (conversations user) }
updateUser user (_, NodeTarget node) = let convs = conversations user in
  user { conversations = (fst $ head convs, node) : tail convs }
updateUser user (ustate', ConversationTarget nconv) = user {
    conversations = (nconv, starter nconv ustate') : conversations user,
    userState = ustate'
  }

processMessageAndUpdateUser :: (Ord nid) => User cid nid us -> String -> ComputationResult ErrorType (Answer, User cid nid us)
processMessageAndUpdateUser user input = do
  (ans, ustate', et) <- processMessage user input
  return (ans, updateUser user (ustate', et))

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
processSerializedUserMessageAndUpdateUser :: (Ord nid) =>
  (cid -> Maybe (Conversation cid nid us)) ->
  (nid -> Maybe (Node nid us)) ->
  (UserId -> ComputationResult ErrorType (SerializableUser cid nid us)) ->
  UserId -> String -> ComputationResult ErrorType (Answer, User cid nid us)
processSerializedUserMessageAndUpdateUser conversationGetter nodeGetter userGetter userId input =
  processEitherUserMessageAndUpdateUser (userGetter userId) where

    processEitherUserMessageAndUpdateUser fuser = do
      suser <- fuser
      user <- defaultUserDeserializer (toComputationResult ConversationNotFound . conversationGetter) (toComputationResult NodeNotFound . nodeGetter) suser
      processMessageAndUpdateUser user input

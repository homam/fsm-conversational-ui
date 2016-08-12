{-# LANGUAGE MultiParamTypeClasses, OverloadedStrings, FlexibleInstances, ScopedTypeVariables, RankNTypes #-}

module UserState
(
  UserState(..), MyNode, MyConversation, MyUConversation,
  SerializableUserState(..),
  getNode, getConversation, findEdge, findEdgeUConversation,
  whatIsYourHeight, whatIsYourWeight,
  sizeSelection
) where

import qualified Data.Map as M
import Serializable
import Node

data NodeId = NodeWhatIsYourWeight | NodeWhatIsYourHeight deriving (Read, Show, Eq, Ord)
data ConversationId = ConversationSizeSelection deriving (Read, Show, Eq, Ord)

data UserState = UserState {
  height :: Maybe Int,
  weight :: Maybe Int
} deriving (Read, Show)

type MyNode = Node NodeId UserState
type MyConversation = Conversation ConversationId NodeId UserState
type MyUConversation = UConversation ConversationId NodeId UserState

type SerializableUserState = (ConversationId, NodeId, Bool, UserState)

instance Serializable MyUConversation String where
  serialize uconversation = show (conversationId $ conversation uconversation, nodeId $ currentNode uconversation, answered uconversation, userState uconversation)
  deserialize = construct . read where
    construct :: SerializableUserState -> MyUConversation
    construct (_conversationId, _nodeId, _answered, _userState) = UConversation (getConversation _conversationId) (getNode _nodeId) _answered _userState


-- all Nodes are defined in the code
getNode :: NodeId -> MyNode
getNode NodeWhatIsYourHeight = whatIsYourHeight
getNode NodeWhatIsYourWeight = whatIsYourWeight

-- all Conversations are defined in the code
getConversation :: ConversationId -> MyConversation
getConversation ConversationSizeSelection = sizeSelection


-- Nodes
whatIsYourHeight :: MyNode
whatIsYourHeight = Node NodeWhatIsYourHeight "What is your height?" (\ a userState -> Right $ userState {height = read a})

whatIsYourWeight :: MyNode
whatIsYourWeight = Node NodeWhatIsYourWeight "What is your weight?" (\ a userState -> Right $ userState {weight = read a})

-- Conversations
sizeSelection :: MyConversation
sizeSelection = makeConversation ConversationSizeSelection [
    Edge whatIsYourHeight (const $ Just whatIsYourWeight),
    Edge whatIsYourWeight (const Nothing)
  ]

{-# LANGUAGE MultiParamTypeClasses, OverloadedStrings, FlexibleInstances, ScopedTypeVariables, RankNTypes #-}

module UserState
(
  UserState(..),
  SerializableUserState(..),
  getNode, getFSM, findEdge, findEdgeUFSM,
  whatIsYourHeight, whatIsYourWeight,
  sizeSelection
) where

import qualified Data.Map as M
import Serializable
import Node


data UserState = UserState {
  height :: Maybe Int,
  weight :: Maybe Int
} deriving (Read, Show)

type SerializableUserState = (FSMId, NodeId, Bool, UserState)

instance Serializable (UFSM UserState) String where
  serialize ufsm = show (fsmId $ fsm ufsm, nodeId $ currentNode ufsm, answered ufsm, userState ufsm)
  deserialize = construct . read where
    construct :: SerializableUserState -> UFSM UserState
    construct (_fsmId, _nodeId, _answered, _userState) = UFSM (getFSM _fsmId) (getNode _nodeId) _answered _userState


-- all Nodes are defined in the code
getNode :: NodeId -> Node UserState
getNode NodeWhatIsYourHeight = whatIsYourHeight
getNode NodeWhatIsYourWeight = whatIsYourWeight

-- all FSMs are defined in the code
getFSM :: FSMId -> FSM UserState
getFSM FSMSizeSelection = sizeSelection


findEdge :: NodeId -> FSM userState -> Maybe (Edge userState)
findEdge nodeId fsm = M.lookup nodeId (edges fsm)

findEdgeUFSM :: UFSM userState -> Maybe (Edge userState)
findEdgeUFSM user = findEdge (nodeId $ currentNode user) (fsm user)

-- Nodes
whatIsYourHeight :: Node UserState
whatIsYourHeight = Node NodeWhatIsYourHeight "What is your height?" (\ a userState -> Right $ userState {height = read a})

whatIsYourWeight :: Node UserState
whatIsYourWeight = Node NodeWhatIsYourWeight "What is your weight?" (\ a userState -> Right $ userState {weight = read a})

makeConversation :: FSMId -> [Edge userState] -> FSM userState
makeConversation convId list  = FSM convId (M.fromList $ map (\e -> (nodeId $ node e, e)) list)

-- FSMs
sizeSelection :: FSM UserState
sizeSelection = makeConversation FSMSizeSelection [
    Edge whatIsYourHeight (const $ Just whatIsYourWeight),
    Edge whatIsYourWeight (const Nothing)
  ]

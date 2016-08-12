{-# LANGUAGE MultiParamTypeClasses, OverloadedStrings, FlexibleInstances, ScopedTypeVariables, RankNTypes #-}

module Lib
    ( someFunc
    )
where

import Control.Arrow ((>>>), (&&&), (<<<), (<<^), (<+>), arr)
import Control.Monad ((>=>), liftM)
import qualified Data.Map as M
import Data.List (isSubsequenceOf)

type NodeId = String
type FSMId = String
type UserId = String

class (Read b, Show b) => Serializable a b where
  serialize :: a -> b
  deserialize :: b -> a

data Node userState = Node {
  nodeId :: NodeId,
  ask :: String,
  -- jumpIf :: userState -> Bool
  receive :: String -> userState -> Either String userState
}

data EdgeTarget userState = Inside (Node userState) | Outside (FSM UserState)

data Edge userState = Edge {
  node :: Node userState,
  exec :: userState -> Maybe (Node userState) -- EdgeTarget userState
}

data FSM userState = FSM {
  fsmId :: FSMId,
  nodes :: [Node userState],
  edges :: [Edge userState]
}

data UFSM userState = UFSM {
  fsm :: FSM userState,
  currentNode :: Node userState,
  answered :: Bool
}

data UserState = UserState {
  height :: Maybe Int,
  weight :: Maybe Int
} deriving (Read, Show)

instance Serializable UserState UserState where
  serialize = id
  deserialize = id

instance Serializable (Node UserState) String where
  serialize = nodeId
  deserialize = getNode

instance Serializable (FSM UserState) String where
  serialize = fsmId
  deserialize = getFSM

instance Serializable (UFSM UserState) String where
  serialize ufsm = serialize (fsm ufsm) ++ ";" ++ serialize (currentNode ufsm) ++ ";" ++ show (answered ufsm)
  deserialize = undefined

whatIsYourHeight :: Node UserState
whatIsYourHeight = Node "whatIsYourHeight" "What is your height?" (\ a userState -> Right $ userState {height = read a})

whatIsYourWeight :: Node UserState
whatIsYourWeight = Node "whatIsYourHeight" "What is your weight?" (\ a userState -> Right $ userState {weight = read a})

sizeSelection :: FSM UserState
sizeSelection = FSM
  "sizeSelection"
  [whatIsYourHeight, whatIsYourWeight] -- is [Node] necessary?
  [
    -- Edge α must be unique, where α is a Node
    Edge whatIsYourHeight (const $ Just whatIsYourWeight),
    Edge whatIsYourWeight (const Nothing)
  ]


getUserState :: UserId -> IO UserState
getUserState = undefined -- from redis

saveUserState :: UserId -> UserState -> IO ()
saveUserState = undefined -- save to redis

getNode :: NodeId -> Node UserState
getNode = undefined -- from memory map

getFSM :: FSMId -> FSM UserState
getFSM = undefined -- from memory map

messageFromUser :: UserId -> String -> IO ()
messageFromUser = undefined -- getUser >>= reconstructUFSM >>= currentNode.receive >>= if Right then fine the edge, edge.exec and update UFSM and send the question to user




someFunc :: IO ()
someFunc = print "hello"

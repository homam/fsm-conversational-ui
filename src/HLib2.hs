{-# LANGUAGE FlexibleContexts, DeriveFunctor, MultiParamTypeClasses, RankNTypes, GADTs, TypeSynonymInstances, FlexibleInstances #-}
module HLib2

where

import qualified Data.Map as M
import Control.Monad.Trans
import Control.Arrow ((***))
import Weather

type Question = String
type UserInput = String
type Answer = String

data Target nid us lsi lso err a = NodeTarget (Node nid us lsi lso err a) | EOC | ConversationTarget deriving (Functor)

data ComputationResult nid us lsi lso err a = ComputationResult {
  computedTarget :: Target nid us lsi lso err a,
  computedState :: (us, lso),
  computedResult :: a
} deriving (Functor)

newtype Computation nid us lsi lso err a = Computation {
  compute :: (us, lsi) -> IO (Either err (ComputationResult nid us lsi lso err a))
}

instance Functor (Computation nid us lsi lso err) where
  fmap f c = Computation $ fmap (fmap (fmap f)) . compute c

data Node nid us lsi lso err a =
  QNode {
    nodeId :: nid,
    question :: Question,
    answer :: UserInput -> Computation nid us lsi lso err a
  } |
  ANode {
    nodeId :: nid,
    exec :: Computation nid us lsi lso err a
  }
  deriving (Functor)

newtype Func lsi gs lso = Func {execFunc :: (lsi, gs) -> (lso, gs)}

-- class (Conversation nid us lsi lso err a) where
--   getNode :: nid -> Node nid us lsi lso err a

class Conversation cid where
  getNode :: cid -> nid -> Node nid us lsi lso err a


------
type NodeId = String
type UserState = M.Map String String
type LocalState = M.Map String String
type Error = String

whichCity :: Node NodeId UserState LocalState LocalState Error Answer
whichCity = QNode {
  nodeId = "WhichCityNode",
  question = "Which city do you want the weather for?",
  answer = \ input -> Computation $ \ (userState, localState) -> return $ Right ComputationResult {
    computedTarget = NodeTarget getWeatherForCity,
    computedState = (userState, M.insert "city" input localState),
    computedResult = "OK, we're getting weather data for " ++ input
  }
}

getWeatherForCity :: Node NodeId UserState LocalState LocalState Error Answer
getWeatherForCity = ANode {
  nodeId = "GetWeatherForCity",
  exec = Computation $ \ (userState, localState) ->
    case M.lookup "city" localState of
      Nothing -> return $ Left "city is not in localState"
      (Just city) -> do
        mw <- getWeather city
        return $ Right $ maybe
          ComputationResult {
            computedTarget = NodeTarget whichCity,
            computedState = (userState, localState),
            computedResult = "Sorry counldn't find weather data for " ++ city ++ ". Trying again..."
          }
          (\ w -> ComputationResult {
              computedTarget = EOC,
              computedState = (userState, localState),
              computedResult = "Weahter in " ++ city ++ " is " ++ show w
            }
          )
          mw
}

data ConversationId = ConversationWeather

instance Conversation ConversationId where
  getNode ConversationWeather "WhichCityNode" = undefined

-- instance Conversation NodeId UserState LocalState LocalState Error Answer where
--   getNode "whichCity" = whichCity

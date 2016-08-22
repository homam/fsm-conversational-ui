{-# LANGUAGE FlexibleContexts, DeriveFunctor, MultiParamTypeClasses, RankNTypes, GADTs, ExistentialQuantification, InstanceSigs, TypeSynonymInstances, FlexibleInstances #-}
module HLib3

where

import qualified Data.Map as M
import Control.Monad.Trans
import Control.Arrow ((***))
import Weather

type Question = String
type UserInput = String
type Answer = String

data Target err a us lsi lso = NodeTarget (Node err a us lsi lso) | EOC | ConversationTarget deriving (Functor)

data ComputationResult err a us lsi lso = ComputationResult {
  computedTarget :: Target err a us lsi lso,
  computedState :: (us, lso),
  computedResult :: a
} deriving (Functor)

newtype Computation err a us lsi lso = Computation {
  compute :: (us, lsi) -> IO (Either err (ComputationResult err a us lsi lso))
}

instance Functor (Computation us lsi lso err) where
  fmap f c = Computation $ fmap (fmap (fmap f)) . compute c

data Node err a us lsi lso =
  QNode {
    question :: Question,
    answer :: UserInput -> Computation err a us lsi lso
  } |
  ANode {
    exec :: Computation err a us lsi lso
  }
  deriving (Functor)




-- instance Pipeable (Node err a us) where
--   n1 `pipe` n2 = undefined

------
type NodeId = String
type UserState = M.Map String String
type LocalState = M.Map String String
type Error = String

whichCity :: Node Error Answer UserState LocalState LocalState
whichCity = QNode {
  question = "Which city do you want the weather for?",
  answer = \ input -> Computation $ \ (userState, localState) -> return $ Right ComputationResult {
    computedTarget = NodeTarget getWeatherForCity,
    computedState = (userState, M.insert "city" input localState),
    computedResult = "OK, we're getting weather data for " ++ input
  }
}

getWeatherForCity :: Node Error Answer UserState LocalState LocalState
getWeatherForCity = ANode {
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

-- data WeatherConversation = WeatherConversation
--
-- instance Conversation WeatherConversation String Answer where
--   getNode WeatherConversation "WhichCityNode" = whichCity

-- instance Conversation NodeId Error Answer UserState LocalState LocalState where
--   getNode "whichCity" = whichCity

class (Read n, Show n) => Serializable n

data DuckTarget a b = forall c. DuckTarget (Duck a b c) | EOD

data Duck a b c = Duck {
  duckF  :: (Serializable a, Serializable b) => a -> b,
  duckOutput :: (Serializable c, Serializable b) => b -> DuckTarget b c
}

class Pipeable n where
  pipe :: (Serializable a, Serializable b, Serializable c) => n a b c -> n b c d -> n a c d

instance Pipeable Duck where
  a `pipe` b = Duck {
    duckF = duckF b . duckF a,
    duckOutput = duckOutput b
  }

aDuck :: Duck String Int String
aDuck = Duck {
  duckF = read,
  duckOutput = \ n -> if n > 10 then DuckTarget bDuck else DuckTarget cDuck
}

bDuck :: Duck Int String c
bDuck = Duck {
  duckF = show,
  duckOutput = const EOD
}

cDuck :: Duck Int String Int
cDuck = Duck {
  duckF = show,
  duckOutput = const $ DuckTarget aDuck
}

execDuck :: (Serializable a, Serializable b, Serializable c) => Duck a b c -> a -> DuckTarget b c
execDuck d = duckOutput d . duckF d

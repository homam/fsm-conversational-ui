{-# LANGUAGE GADTs, StandaloneDeriving #-}

module V2FlowCont (Cont(..), StageFlow(..)) where

data Cont where
  Cont :: Show s => StageFlow s -> Cont
  Start :: (Show s, Show s') => StageFlow s -> StageFlow s' -> Cont
  End :: Show s => StageFlow s -> Cont

deriving instance Show Cont

data StageFlow a where
  -- FlowYourSize :: StageFlow Size.Suspended
  -- FlowTryAtHome :: StageFlow TryAtHome.Suspended
  FlowYourSize :: Show as => as -> StageFlow as
  FlowTryAtHome :: Show as => as -> StageFlow as

deriving instance Show (StageFlow a)

{-# LANGUAGE GADTs #-}

module V2FlowCont (Cont(..)) where

data Cont where
  Cont :: Show s => s -> Cont
  Start :: (Show s, Show s') => s -> s' -> Cont
  End :: Show s => s -> Cont

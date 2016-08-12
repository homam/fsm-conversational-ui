{-# LANGUAGE MultiParamTypeClasses #-}
module Serializable
(
  Serializable(..)
) where

class (Read b, Show b) => Serializable a b where
  serialize :: a -> b
  deserialize :: b -> a

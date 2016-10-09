module Bar (BarState(..)) where

import Cont

data BarState = BInit | BMid | BEnd deriving (Read, Show)

instance IsState BarState where
    step BInit = cont BMid
    step BMid = end BEnd

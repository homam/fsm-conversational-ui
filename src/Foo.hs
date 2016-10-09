module Foo (FooState(..)) where

import Cont
import Bar

data FooState = FInit | FBar | FEnd deriving (Read, Show)

instance IsState FooState where
    step FInit = start FBar BInit
    step FBar = end FEnd

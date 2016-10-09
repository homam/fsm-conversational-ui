{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module Cont (Cont(..), cont, start, end, State(..), IsState(..)) where

-- | Action operates on 'State'
data Cont = Cont State | Start State State | End State deriving (Show)

-- | Converts actual data to 'State'
cont ∷ IsState s ⇒ s → Cont
cont = Cont . state

start ∷ (IsState s, IsState s') ⇒ s → s' → Cont
start x x' = Start (state x) (state x')

end ∷ IsState s ⇒ s → Cont
end = End . state


-- | State is something, which have next action and string representation
data State = State {
    next ∷ Cont,
    save ∷ String 
}


class (Read s, Show s) ⇒ IsState s where
    step ∷ s → Cont
    -- | No need of implementation, just to allow using when implementing step
    state ∷ s → State
    state x = State (step x) (show x)



instance Show State where
    show = save
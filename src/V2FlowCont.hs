{-# LANGUAGE UnicodeSyntax #-}

module V2FlowCont (Cont(..), cont, start, end, State(..), IsState(..)) where

import Debug.Trace (trace)

-- | Action operates on 'State'
data Cont = Cont State | Start State State | End State deriving (Show)

-- | Converts actual data to 'State'
cont ∷ IsState s ⇒ s → Cont
cont i = Cont $ state i

start ∷ (IsState s, IsState s') ⇒ s → s' → Cont
start x x' = Start (state x) (state x')

end ∷ IsState s ⇒ s → Cont
end s = trace ("-- end s = " ++ show s) (End $ state s)

-- | State is something, which have next action and string representation
data State = State {
  next ∷ String -> Cont,
  save ∷ String
}


class (Read s, Show s) ⇒ IsState s where
  step ∷ s → String → Cont
  -- | No need of implementation, just to allow using when implementing step
  state ∷ s → State
  state x = State {
    next = step x,
    save = show x
  }

instance Show State where
  show = save

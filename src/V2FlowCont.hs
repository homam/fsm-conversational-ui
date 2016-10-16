module V2FlowCont (Answer(..), Cont(..), cont, start, end, State(..), IsState(..), IsQuestion(..)) where

newtype Answer = Answer { unAnswer :: String }

-- | Action operates on 'State'
data Cont = Cont State | Start State State | End State deriving (Show)

-- | Continue in the same flow
cont :: IsState s => s -> Cont
cont i = Cont $ state i

-- | Start a new flow (from inside the current flow)
start :: (IsState s, IsState s') => s -> s' -> Cont
start s s' = Start (state s) (state s')

-- | End the current flow
end :: IsState s => s -> Cont
end s = End $ state s

-- | Whether the state is a Question
class IsQuestion s where
  ask :: s -> Maybe String

-- | State is something, which has the next action, a string representation and maybe a question
data State = State {
  next :: Answer -> Cont,
  question :: Maybe String,
  save :: String
}


class (Read s, Show s, IsQuestion s) => IsState s where

  -- | Specifies how to proceed given the current state 's' and an `Answer`
  step :: s -> Answer -> Cont

  state :: s -> State
  state x = State {
    next = step x,
    question = ask x,
    save = show x
  }

instance Show State where
  show = save

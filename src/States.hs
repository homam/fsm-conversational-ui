{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module States () where


import Control.Arrow
import Text.Read (Read(readsPrec), readMaybe)
import Cont
import Bar
import Foo



-- | Stack is list of states
type Stack = [State]

isEmptyStack :: Stack -> Bool
isEmptyStack stack = case stack of
  [] -> True
  _ -> False

run ∷ Stack → Stack
run [] = error "empty stack"
run (s : ss) = proceed (next s) ss

proceed ∷ Cont → Stack → Stack
proceed (Cont s) rest = s : rest
proceed (Start s s') rest = s' : s : rest
proceed (End s) rest = rest

serialize ∷ Stack → [String]
serialize = map save

-- | Here we have to provide some type in order to know, which
-- read functions to use
deserialize ∷ IsState s ⇒ [String] → Maybe [s]
deserialize = mapM readMaybe


-- | Convert actual data to stack
stack ∷ IsState s ⇒ [s] → Stack
stack = map state

-- | Union of states, to specify type of 'deserialize'
data BiState l r = LState l | RState r

instance (Read l, Read r) ⇒ Read (BiState l r) where
    readsPrec p s = map (first LState) (readsPrec p s) ++ map (first RState) (readsPrec p s)

instance (Show l, Show r) ⇒ Show (BiState l r) where
    show (LState x) = show x
    show (RState y) = show y

instance (IsState l, IsState r) ⇒ IsState (BiState l r) where
    step (LState x) = step x
    step (RState y) = step y


loopStack :: Stack -> IO ()
loopStack current = do
  let saved = serialize current
  print saved
  case (deserialize saved ∷ Maybe [BiState FooState BarState]) of
    Just loaded -> do
      let next' = run $ stack loaded
      loopStack next'
    Nothing -> putStrLn "parse error"

-- Usage

test' ∷ IO ()
test' = do
    let start = [state FInit] :: Stack
    loopStack start

main = test'
-- output:
-- [BInit,FBar]
-- [BMid,FBar]
-- [BMid,FBar]
-- [FBar]
-- []
-- done!

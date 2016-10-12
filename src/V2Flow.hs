{-# LANGUAGE UnicodeSyntax #-}

module V2Flow () where

import qualified V2Size as Size
import qualified V2TryAtHome as TryAtHome
import V2FlowCont (Cont(..), cont, start, end, State(..), IsState(..))

import Control.Arrow (first)
import Text.Read (Read(readsPrec), readMaybe)

import qualified System.IO as IO

import Debug.Trace (trace)

-- | Stack is list of states
type Stack = [State]

run ∷ Stack → String → Stack
run [] _ = error "empty stack"
run (s : ss) i = proceed (next s i) ss

proceed ∷ Cont → Stack → Stack
proceed (Cont s) rest = s : rest
proceed (Start s s') rest = s' : s : rest
-- proceed (End s) rest = rest
proceed (End s) rest = let (h:t) = rest in
  proceed (next h (show s)) t

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
  readsPrec p s = trace ("\nRead BiState s = " ++ s ++ "\n") $ map (first LState) (readsPrec p s) ++ map (first RState) (readsPrec p s)

instance (Show l, Show r) ⇒ Show (BiState l r) where
  show (LState x) = show x
  show (RState y) = show y

instance (IsState l, IsState r) ⇒ IsState (BiState l r) where
  step (LState x) = step x
  step (RState y) = step y


loopStack :: Stack → IO ()
loopStack current = do
  let saved = serialize current
  putStrLn $ "saved = " ++ show saved
  case (deserialize saved ∷ Maybe [BiState Size.Suspended TryAtHome.Suspended]) of
    Just loaded -> do
      let next' = run $ stack loaded
      i <- readLn
      loopStack $ next' i
    Nothing -> putStrLn "parse error"


loopStackWORead ∷ Stack → IO ()
loopStackWORead current = do
  print "-- loopStackWORead"
  let saved = serialize current
  print saved
  let next' = run current
  i ← readLn
  loopStackWORead $ next' i


-- Usage

test' ∷ IO ()
test' = do
  let start = [state $ TryAtHome.Suspended TryAtHome.AskProduct ()] :: Stack
  -- loopStack start
  loopStack start

main = do
  IO.hSetBuffering IO.stdin IO.LineBuffering
  test'

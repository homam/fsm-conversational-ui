module V2Flow () where

import qualified V2Size as Size
import qualified V2TryAtHome as TryAtHome
import qualified V2Checkout as Checkout
import V2FlowCont (Answer(..), Cont(..), cont, start, end, State(..), IsQuestion(ask), IsState(step, state))

import Control.Arrow (first)
import Text.Read (Read(readsPrec), readMaybe)
import qualified System.IO as IO

-- | Stack is list of states
type Stack = [State]

run :: Stack -> Answer -> Stack
run [] _ = error "empty stack"
run (s : ss) i = let ns = next s i in proceed ns ss

proceed :: Cont -> Stack -> Stack
proceed (Cont s) rest = s : rest
proceed (Start s s') rest = s' : s : rest
proceed (End s) rest = case rest of
  (h:t) -> proceed (next h (Answer $ show s)) t
  []    -> []

serialize :: Stack -> [String]
serialize = map save

-- | Here we have to provide some type in order to know, which
-- read functions to use
deserialize :: IsState s => [String] -> Maybe [s]
deserialize = mapM readMaybe


-- | Convert actual data to stack
stack :: IsState s => [s] -> Stack
stack = map state

-- | Union of states, to specify type of 'deserialize'
data BiState l r = LState l | RState r

instance (Read l, Read r, Show l, Show r) => Read (BiState l r) where
  readsPrec p s = map (first LState) (readsPrec p s) ++ map (first RState) (readsPrec p s)

instance (Show l, Show r) => Show (BiState l r) where
  show (LState x) = show x
  show (RState x) = show x

instance (IsQuestion l, IsQuestion r) => IsQuestion (BiState l r) where
  ask (LState x) = ask x
  ask (RState x) = ask x

instance (IsState l, IsState r) => IsState (BiState l r) where
  step (LState x) = step x
  step (RState x) = step x


loopStack :: Stack -> IO ()
loopStack [] = putStrLn "Fin"
loopStack current@(h:_) = do
  maybe (return ()) (putStrLn . (">> " ++)) (question h)
  let saved = serialize current
  putStrLn $ "saved = " ++ show saved
  case (deserialize saved :: Maybe [BiState (BiState Checkout.Suspended Size.Suspended) TryAtHome.Suspended]) of
    Just loaded -> do
      let next' = run $ stack loaded
      i <- Answer <$> readLn
      let ns = next' i
      loopStack ns
    Nothing -> putStrLn "parse error"


-- loopStackWORead :: Stack -> IO ()
-- loopStackWORead current = do
--   print "-- loopStackWORead"
--   let saved = serialize current
--   print saved
--   let next' = run current
--   i <- Answer <$> readLn
--   loopStackWORead $ next' i


-- Usage

test' :: IO ()
test' = do
  let start = [state $ TryAtHome.Suspended TryAtHome.AskProduct ()] :: Stack
  loopStack start

main = do
  IO.hSetBuffering IO.stdin IO.LineBuffering
  test'

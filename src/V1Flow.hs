module V1Flow
  (
    Flow(..),
    resumeFlow, persist,
    askFlow, (>>++)
  )
where

import V1IState

newtype Flow sp as o a  = Flow { unFlow :: sp -> StateMachine as o a }

resumeFlow :: (Read sp, Show o, Show a) => i -> String -> Flow sp i o a -> IO (o, a)
resumeFlow i st flow = runIState (unFlow flow (read st)) i

persist :: Show sp => sp -> IO ()
persist = print

askFlow :: (Read sp) => String -> Flow sp i o a -> StateMachine i o a
askFlow st flow =
  ilift (print $ "New Flow Starts with state: " ++ st) >>>
  unFlow flow (read st)
  >>>= \a -> ilift (print "New Flow Ended") >>> ireturn a

addReturn :: StateMachine i o a -> StateMachine i o o
addReturn sm = sm >>> iget >>>= \ o -> ireturn o

(>>++) :: StateMachine i o a1 -> (o -> i -> o') -> StateMachine i o' o
sm1 >>++ f = iget >>>= \ as -> addReturn sm1 >>>= \ a -> iput (f a as) >>> ireturn a

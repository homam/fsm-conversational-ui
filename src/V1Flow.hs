module V1Flow
  (
    Flow(..),
    resumeFlow, persist
  )
where

import V1IState

newtype Flow sp as o a  = Flow { unFlow :: sp -> StateMachine as o a }

resumeFlow :: (Read sp, Show o, Show a) => i -> String -> Flow sp i o a -> IO (o, a)
resumeFlow i st flow = runIState (unFlow flow (read st)) i

persist :: Show sp => sp -> IO ()
persist = print

askFlow :: (Read sp) => String -> Flow sp i o a -> StateMachine i o a
askFlow st flow = unFlow flow (read st)

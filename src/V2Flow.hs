{-# LANGUAGE GADTs, StandaloneDeriving #-}

module V2Flow (StageFlow(..)) where
import qualified V2Size as Size
import qualified V2TryAtHome as TryAtHome
import V2FlowCont (Cont(..), StageFlow(..))


-- data Suspended where
--     Suspended :: Show as => Stage as -> as -> Suspended
--
-- instance Show Suspended where
--   show (Suspended stage as) = show stage ++ ", " ++ show as


data SuspendedFlow where
  SuspendedFlow :: Show as => StageFlow as -> Maybe as -> SuspendedFlow

instance Show SuspendedFlow where
  show (SuspendedFlow stage as) = show stage ++ ", " ++ show as

newtype FlowStack = FlowStack { unFlowStack :: [SuspendedFlow] }

deriving instance Show FlowStack

run :: FlowStack -> String -> FlowStack
run stack i = let h:_ = unFlowStack stack in case resume h i of
  Cont s -> cont stack s
  Start s s' -> start stack s s'

resume :: SuspendedFlow -> String -> Cont
resume (SuspendedFlow (FlowTryAtHome as) _) i = undefined

cont :: Show a => FlowStack -> StageFlow a -> FlowStack
cont stack stage = let _:rest = unFlowStack stack in FlowStack $ SuspendedFlow stage Nothing : rest

start :: (Show s, Show s') => FlowStack -> StageFlow s -> StageFlow s' -> FlowStack
-- start = undefined
start stack stage stage' = FlowStack $ SuspendedFlow stage' Nothing : unFlowStack (cont stack stage)

main = do
  let stage = FlowTryAtHome () :: StageFlow ()
  let suspended = SuspendedFlow stage Nothing :: SuspendedFlow
  let stack = FlowStack [suspended] :: FlowStack
  -- TryAtHome.run (TryAtHome.Suspended TryAtHome.AskProduct ()) "123"
  print 0

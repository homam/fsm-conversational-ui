{-# LANGUAGE GADTs, StandaloneDeriving #-}

module V2Flow (StageFlow(..)) where
import qualified V2Size as Size
import qualified V2TryAtHome as TryAtHome
import V2FlowCont (Cont(..))

data StageFlow a where
  FlowYourSize :: StageFlow Size.Suspended
  FlowTryAtHome :: StageFlow TryAtHome.Suspended

deriving instance Show (StageFlow a)

data SuspendedFlow where
  SuspendedFlow :: Show as => StageFlow as -> Maybe as -> SuspendedFlow

instance Show SuspendedFlow where
  show (SuspendedFlow stage as) = show stage ++ ", " ++ show as

newtype FlowStack = FlowStack { unFlowStack :: [SuspendedFlow] }

run :: FlowStack -> String -> FlowStack
run stack i = let h:_ = unFlowStack stack in case resume h i of
  Cont s -> cont stack undefined

resume :: SuspendedFlow -> String -> Cont
resume = undefined

cont :: FlowStack -> StageFlow a -> FlowStack
cont = undefined
-- cont fs s = let (fid, _):rest = unFlowStack fs in FlowStack $ (fid, s):rest

start :: Show a => FlowStack -> StageFlow a -> FlowStack
-- start = undefined
start stack stage = FlowStack $ SuspendedFlow stage Nothing : unFlowStack stack

main = print 0

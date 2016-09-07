{-# LANGUAGE GADTs #-}

module V1Flows
where

import qualified V1TryAtHome as TAH
import qualified V1SizeFlow as SF
import V1Flow
import V1IState

data Flows sp as o a where
  SizeFlow :: Flow SF.Suspended as SF.SizeFlowResult () -> Flows sp as o a
  TryAtHomeFlow :: Flow TAH.Suspended as TAH.TryAtHomeFlowResult () -> Flows sp as o a

runFlow :: Flows sp as o a -> IO ()
runFlow (SizeFlow f) = resumeFlow () "AskKnownSize, ()" SF.sizeFlow >>= print
runFlow (TryAtHomeFlow f) = resumeFlow () "AskProduct, ()" TAH.tryAtHomeFlow >>= print

getFlow "SizeFlow" = SizeFlow SF.sizeFlow
getFlow "TryAtHomeFlow" = TryAtHomeFlow TAH.tryAtHomeFlow

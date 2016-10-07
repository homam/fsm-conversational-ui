{-# LANGUAGE GADTs, StandaloneDeriving #-}

module V2TryAtHome (run, Stage(..)) where

import V2FlowCont (Cont(..), StageFlow(FlowTryAtHome, FlowYourSize))
import qualified V2Size as Size

newtype Product = Product Int deriving (Read, Show)
newtype Address = Address String deriving (Read, Show)

data Suspended where
    Suspended :: Show as => Stage as -> as -> Suspended

instance Show Suspended where
  show (Suspended stage as) = show stage ++ ", " ++ show as

data Stage a where
  AskProduct :: Stage ()
  AskSize    :: Stage (Product, ())
  AskAddress :: Stage (Size.FinalResult, (Product, ()))
  AskFinal   :: Stage (Address, (Size.FinalResult, (Product, ())))

deriving instance Show (Stage a)


getProduct :: s -> String -> (Product, s)
getProduct s i = (Product $ read i, s)

getAddress :: s -> String -> (Address, s)
getAddress s i = (Address i, s)

run :: Stage s -> s -> String -> Cont
run AskProduct s i = Start (FlowTryAtHome $ Suspended AskSize (getProduct s i)) (FlowYourSize $ Size.Suspended Size.AskDoYou ())
run AskSize s i = Cont . FlowTryAtHome $ Suspended AskAddress undefined
run AskAddress s i = End . FlowTryAtHome $ Suspended AskFinal (getAddress s i)

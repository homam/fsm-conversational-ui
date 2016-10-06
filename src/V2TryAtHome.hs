{-# LANGUAGE GADTs, StandaloneDeriving #-}

module V2TryAtHome (run, Suspended) where

import V2FlowCont (Cont(..))
import qualified V2Size as Size

newtype Product = Product Int deriving (Read, Show)
newtype Address = Address String deriving (Read, Show)

data Stage a where
  AskProduct :: Stage ()
  AskSize    :: Stage (Product, ())
  AskAddress :: Stage (Size.FinalResult, (Product, ()))
  AskFinal   :: Stage (Address, (Size.FinalResult, (Product, ())))

deriving instance Show (Stage a)

data Suspended where
    Suspended :: Show as => Stage as -> as -> Suspended

instance Show Suspended where
  show (Suspended stage as) = show stage ++ ", " ++ show as

getProduct :: s -> String -> (Product, s)
getProduct s i = (Product $ read i, s)

getAddress :: s -> String -> (Address, s)
getAddress s i = (Address i, s)

run :: Suspended -> String -> Cont
run (Suspended AskProduct s) i = Start (Suspended AskSize (getProduct s i)) (Size.Suspended Size.AskDoYou ())
run (Suspended AskAddress s) i = End (Suspended AskFinal (getAddress s i))

{-# LANGUAGE GADTs, StandaloneDeriving #-}

module V2TryAtHome (Stage(..), Suspended(Suspended)) where

import V2FlowCont (Cont(..), IsState(..), start, cont, end)
import qualified V2Size as Size
import Debug.Trace (trace)

newtype Product = Product Int deriving (Read, Show)
newtype Address = Address String deriving (Read, Show)

data Suspended where
  Suspended :: Show as => Stage as -> as -> Suspended

instance Show Suspended where
  show (Suspended stage as) = show stage ++ ", " ++ show as

instance Read Suspended where
  readsPrec = undefined

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

instance IsState Suspended where
  step (Suspended AskProduct s) i = start (Suspended AskSize (getProduct s i)) (Size.Suspended Size.AskDoYou ())
  step st@(Suspended AskSize s) i =
    let f = trace ("> " ++ show st ++ " -- " ++ i) (read i :: Size.Suspended)
    in case f of
      Size.Suspended Size.AskFinal s' -> cont $ Suspended AskAddress (s', s)
      _ -> error ("error: " ++ i ++ " is not of type Size.Suspended Size.AskFinal s")

{-# LANGUAGE GADTs, StandaloneDeriving #-}

module V2TryAtHome (Stage(..), Suspended(Suspended)) where

import V2FlowCont (Cont(..), IsState(..), start, cont, end)
import V2ParserUtil (parseSuspended, parseStage, ReadParsec(..))

import qualified V2Size as Size
import qualified V2Checkout as Checkout
import Debug.Trace (trace)

newtype Product = Product Int deriving (Read, Show)
newtype Address = Address String deriving (Read, Show)

data Suspended where
  Suspended :: Show as => Stage as -> as -> Suspended

instance Show Suspended where
  show (Suspended stage as) = "Suspended " ++ show stage ++ " " ++ show as

instance Read Suspended where
  readsPrec = readsPrec1

instance ReadParsec Suspended where
  parsecRead = parseSuspended [
        read' "AskSizeFlow" AskSizeFlow
      , read' "AskProduct" AskProduct
      , read' "AskAddress" AskAddress
      , read' "AskCheckout" AskCheckoutFlow
      , read' "AskFinal" AskFinal
    ]
    where
      read' name = parseStage name  . Suspended

data Stage a where
  AskProduct      :: Stage ()
  AskSizeFlow     :: Stage (Product, ())
  AskAddress      :: Stage (Size.FinalResult, (Product, ()))
  AskCheckoutFlow :: Stage (Address, (Size.FinalResult, (Product, ())))
  AskFinal        :: Stage (Checkout.FinalResult, (Address, (Size.FinalResult, (Product, ()))))

deriving instance Show (Stage a)


getProduct :: s -> String -> (Product, s)
getProduct s i = (Product $ read i, s)

getAddress :: s -> String -> (Address, s)
getAddress s i = (Address i, s)

instance IsState Suspended where
  step (Suspended AskProduct s) i = start (Suspended AskSizeFlow (getProduct s i)) (Size.Suspended Size.AskDoYou ())
  step st@(Suspended AskSizeFlow s) i =
    let f = trace ("> " ++ show st ++ " -- " ++ i) (read i :: Size.Suspended)
    in case f of
      Size.Suspended Size.AskFinal s' -> cont $ Suspended AskAddress (s', s)
      _ -> error ("error: " ++ i ++ " is not of type Size.Suspended Size.AskFinal s")
  step (Suspended AskAddress s) i = cont (Suspended AskCheckoutFlow (Address i, s))
  step (Suspended AskCheckoutFlow s) i = cont (Suspended AskFinal (read i, s))

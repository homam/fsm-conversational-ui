{-# LANGUAGE GADTs, StandaloneDeriving #-}

module V2TryAtHome (Stage(AskProduct, AskFinal), Suspended(Suspended)) where

import V2FlowCont (Answer(..), Cont(..), IsQuestion(..), IsState(..), start, cont, end)
import V2ParserUtil (parseSuspended, parseStage, ReadParsec(..))
import qualified V2Size as Size
import qualified V2Checkout as Checkout

newtype Product = Product Int deriving (Read, Show)
newtype Address = Address String deriving (Read, Show)

data Suspended where
  Suspended :: Show as => Stage as -> as -> Suspended

instance Show Suspended where
  show (Suspended stage as) = "Suspended " ++ show stage ++ " " ++ show as

instance Read Suspended where
  readsPrec = readsPrecRP

instance ReadParsec Suspended where
  readParsec = parseSuspended [
        read' "AskSizeFlow" AskSizeFlow
      , read' "AskProduct" AskProduct
      , read' "AskAddress" AskAddress
      , read' "AskCheckoutFlow" AskCheckoutFlow
      , read' "AskFinal" AskFinal
    ]
    where
      read' name = parseStage name  . Suspended

data Stage a where
  AskProduct      :: Stage ()
  AskSizeFlow     :: Stage (Product, ())
  AskAddress      :: Stage (Size.SizeResult, (Product, ()))
  AskCheckoutFlow :: Stage (Address, (Size.SizeResult, (Product, ())))
  AskFinal        :: Stage (Checkout.FinalResult, (Address, (Size.SizeResult, (Product, ()))))

deriving instance Show (Stage a)


getProduct :: s -> String -> (Product, s)
getProduct s i = (Product $ read i, s)

getAddress :: s -> String -> (Address, s)
getAddress s i = (Address i, s)

instance IsQuestion Suspended where
  ask (Suspended AskProduct      _) = Just "Which product do you want to buy?"
  ask (Suspended AskSizeFlow     _) = Nothing
  ask (Suspended AskAddress      _) = Just "What is your address?"
  ask (Suspended AskCheckoutFlow _) = Nothing
  ask (Suspended AskFinal        _) = Nothing


instance IsState Suspended where
  step (Suspended AskProduct s) (Answer i) = start (Suspended AskSizeFlow (getProduct s i)) (Size.Suspended Size.AskDoYou ())
  step (Suspended AskSizeFlow s) (Answer i) =
    let flowResult = read i :: Size.Suspended
    in case flowResult of
      Size.Suspended Size.AskFinal s' -> cont $ Suspended AskAddress (s', s)
      _ -> error ("error: " ++ i ++ " is not of type Size.Suspended Size.AskFinal s")
  step (Suspended AskAddress s) (Answer i) = start (Suspended AskCheckoutFlow (Address i, s)) (Checkout.Suspended Checkout.AskCheckoutBillingInfo ())
  step (Suspended AskCheckoutFlow s) (Answer i) =
    let flowResult = read i :: Checkout.Suspended
    in case flowResult of
      Checkout.Suspended Checkout.AskFinal s' -> end $ Suspended AskFinal (s', s)
      _ -> error ("error: " ++ i ++ " is not of type Checkout.Suspended Checkout.FinalResult s")

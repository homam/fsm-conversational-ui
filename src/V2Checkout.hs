{-# LANGUAGE GADTs, StandaloneDeriving #-}

module V2Checkout (Suspended(Suspended), Stage(..), FinalResult) where -- AskDoYou, AskCheckoutFinal

import V2FlowCont (Cont(..), IsState(..), start, cont, end)
import V2ParserUtil (parseSuspended, parseStage, ReadParsec(..))

newtype BillingInfo = BillingInfo String deriving (Read, Show)

type FinalResult = (BillingInfo, ())

data Stage a where
  AskCheckoutBillingInfo :: Stage ()
  AskCheckoutFinal       :: Stage (BillingInfo, ())

deriving instance Show (Stage a)

data Suspended where
  Suspended :: Show as => Stage as -> as -> Suspended

instance Show Suspended where
  show (Suspended stage as) = "Suspended " ++ show stage ++ " " ++ show as

instance Read Suspended where
  readsPrec = readsPrec1

instance ReadParsec Suspended where
  parsecRead = parseSuspended [
        read' "AskCheckoutBillingInfo" AskCheckoutBillingInfo
      , read' "AskCheckoutFinal" AskCheckoutFinal
    ]
    where
      read' name = parseStage name  . Suspended


instance IsState Suspended where
  step (Suspended AskCheckoutBillingInfo s) i = end $ Suspended AskCheckoutFinal (BillingInfo i, s)

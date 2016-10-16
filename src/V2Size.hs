{-# LANGUAGE GADTs, StandaloneDeriving #-}

module V2Size (Suspended(Suspended), SizeResult, Stage(AskDoYou, AskFinal), Size(..), Weight(..), Height(..)) where -- AskDoYou, AskFinal

import V2FlowCont (Answer(..), Cont(..), IsQuestion(..), IsState(..), start, cont, end)
import V2ParserUtil (parseSuspended, parseStage, ReadParsec(readsPrecRP, readParsec))

newtype Size = Size Int deriving (Read, Show)
newtype Height = Height Int deriving (Read, Show)
newtype Weight = Weight Int deriving (Read, Show)

data SizeResult = SizeResult {
  knownSize :: Bool,
  size :: Size
} deriving (Read, Show)

data Stage a where
  AskDoYou  :: Stage ()
  AskSize   :: Stage (Bool, ())
  AskWeight :: Stage (Bool, ())
  AskHeight :: Stage (Weight, (Bool, ()))
  AskFinal  :: Stage SizeResult

deriving instance Show (Stage a)

data Suspended where
  Suspended :: Show as => Stage as -> as -> Suspended

instance Show Suspended where
  show (Suspended stage as) = "Suspended " ++ show stage ++ " " ++ show as

instance Read Suspended where
  readsPrec = readsPrecRP

instance ReadParsec Suspended where
  readParsec = parseSuspended [
        read' "AskSize" AskSize
      , read' "AskDoYou" AskDoYou
      , read' "AskWeight" AskWeight
      , read' "AskHeight" AskHeight
      , read' "AskFinal" AskFinal
    ]
    where
      read' name = parseStage name . Suspended

getKnownSize :: (Bool, s) -> String -> SizeResult
getKnownSize (b, _) i = SizeResult { size = Size $ read i, knownSize = b}

getWeight :: s -> String -> (Weight, s)
getWeight s i = (Weight $ read i, s)

getHeight :: (Weight, (Bool, s)) -> String -> SizeResult
getHeight (Weight w, (b, _)) i = SizeResult { size = Size $ w * read i, knownSize = b }

instance IsQuestion Suspended where
  ask (Suspended AskDoYou  _) = Just "Do you know your size?"
  ask (Suspended AskWeight _) = Just "What is your weight?"
  ask (Suspended AskHeight _) = Just "What is your height?"
  ask (Suspended AskSize   _) = Just "What is your size?"
  ask (Suspended AskFinal  _) = Nothing

instance IsState Suspended where
  step (Suspended AskDoYou  s) (Answer i) = cont $ if "y" == i then Suspended AskSize (True, s) else Suspended AskWeight (False, s)
  step (Suspended AskWeight s) (Answer i) = cont $ Suspended AskHeight (getWeight s i)
  step (Suspended AskHeight s) (Answer i) = end $ Suspended AskFinal (getHeight s i)
  step (Suspended AskSize   s) (Answer i) = end $ Suspended AskFinal (getKnownSize s i)
  step (Suspended AskFinal  _) _          = error "Flow already ended."

{-# LANGUAGE GADTs, StandaloneDeriving #-}

module V2Size (Suspended(Suspended), FinalResult, Stage(..), Size(..), Weight(..), Height(..)) where -- AskDoYou, AskFinal

import V2FlowCont (Cont(..), IsState(..), start, cont, end)
import V2ParserUtil (parseSuspended, parseStage, ReadParsec(..))

newtype Size = Size Int deriving (Read, Show)
newtype Height = Height Int deriving (Read, Show)
newtype Weight = Weight Int deriving (Read, Show)

type FinalResult = (Size, (Bool, ()))

data Stage a where
  AskDoYou  :: Stage ()
  AskSize   :: Stage (Bool, ())
  AskWeight :: Stage (Bool, ())
  AskHeight :: Stage (Weight, (Bool, ()))
  AskFinal  :: Stage FinalResult

deriving instance Show (Stage a)

data Suspended where
  Suspended :: Show as => Stage as -> as -> Suspended

instance Show Suspended where
  show (Suspended stage as) = "Suspended " ++ show stage ++ " " ++ show as

instance Read Suspended where
  readsPrec = readsPrec1

instance ReadParsec Suspended where
  parsecRead = parseSuspended [
        read' "AskSize" AskSize
      , read' "AskDoYou" AskDoYou
      , read' "AskWeight" AskWeight
      , read' "AskHeight" AskHeight
      , read' "AskFinal" AskFinal
    ]
    where
      read' name = parseStage name  . Suspended

getKnownSize :: s -> String -> (Size, s)
getKnownSize s i = (Size $ read i, s)

getWeight :: s -> String -> (Weight, s)
getWeight s i = (Weight $ read i, s)

getHeight :: (Weight, s) -> String -> (Size, s)
getHeight (Weight w, s) i = (Size $ w * read i, s)


instance IsState Suspended where
  step (Suspended AskDoYou s) i = cont $ if "y" == i then Suspended AskSize (True, s) else Suspended AskWeight (False, s)
  step (Suspended AskWeight s) i = cont $ Suspended AskHeight (getWeight s i)
  step (Suspended AskHeight s) i = end $ Suspended AskFinal (getHeight s i)
  step (Suspended AskSize s) i = end $ Suspended AskFinal (getKnownSize s i)

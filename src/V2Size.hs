{-# LANGUAGE GADTs, StandaloneDeriving #-}

module V2Size (run, Suspended(Suspended), FinalResult, Stage(AskDoYou)) where

import V2FlowCont (Cont(..))

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
  show (Suspended stage as) = show stage ++ ", " ++ show as

getKnownSize :: s -> String -> (Size, s)
getKnownSize s i = (Size $ read i, s)

getWeight :: s -> String -> (Weight, s)
getWeight s i = (Weight $ read i, s)

getHeight :: (Weight, s) -> String -> (Size, s)
getHeight (Weight w, s) i = (Size $ w * read i, s)

run :: Suspended -> String -> Cont
run (Suspended AskDoYou s) i = Cont $ if "y" == i then Suspended AskSize (True, s) else Suspended AskWeight (False, s)
run (Suspended AskWeight s) i = Cont $ Suspended AskHeight (getWeight s i)
run (Suspended AskHeight s) i = End $ Suspended AskFinal (getHeight s i)
run (Suspended AskSize s) i = End $ Suspended AskFinal (getKnownSize s i)

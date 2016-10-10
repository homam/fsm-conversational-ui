{-# LANGUAGE GADTs, StandaloneDeriving #-}

module V2Size (Suspended(Suspended), FinalResult, Stage(..)) where -- AskDoYou, AskFinal

import V2FlowCont (Cont(..), IsState(..), start, cont, end)

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

instance Read Suspended where
  readsPrec = const $ uncurry ($) . mapFst parse . fmap (drop 2) . break (==',')
    where
      parse :: String -> String -> [(Suspended, String)]
      parse stage = case stage of
        "AskDoYou"  -> parse' AskDoYou
        "AskSize"   -> parse' AskSize
        "AskWeight" -> parse' AskWeight
        "AskHeight" -> parse' AskHeight
        "AskFinal"  -> parse' AskFinal
        _ -> const []

      parse' :: (Show as, Read as) => Stage as -> String -> [(Suspended, String)]
      parse' stg st = [(Suspended stg (read st), mempty)]

      mapFst :: (a -> c) -> (a, b) -> (c, b)
      mapFst f ~(a, b) = (f a, b)

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

{-# LANGUAGE TupleSections, GADTs, StandaloneDeriving, FlexibleInstances #-}

module V1SizeFlow
  (
    sizeFlow, Suspended, SizeFlowResult
  )
where

import V1IState
import V1Flow

newtype Size = Size Int deriving (Read, Show)
newtype Height = Height Int deriving (Read, Show)
newtype Weight = Weight Int deriving (Read, Show)
newtype Colour = Colour String deriving (Read, Show)

askKnownSize :: StateMachine as (Bool, as) Bool
askKnownSize = askYN "Do you know your size?" (,)

-- askSize takes an environment of type as and adds a Size element
askSize :: StateMachine as (Size, as) Size
askSize = askNumber "What is your size?" Size (,)

-- askHeight takes an environment of type as and adds a Height element
askHeight :: StateMachine as (Height, as) Height
askHeight = askNumber "What is your height?" Height (,)

-- etc
askWeight :: StateMachine as (Weight, as) Weight
askWeight = askNumber "What is your weight?" Weight (,)

askColour :: StateMachine as (Colour, as) ()
askColour =
  -- poor man's do-notation. You could use RebindableSyntax
  ilift (putStrLn "What is your favourite colour?") >>>
  ilift readLn >>>= \answer ->
  imodify (Colour answer,)

calculateSize :: Height -> Weight -> Size
calculateSize (Height h) (Weight w) = Size (h - w)  -- or whatever the calculation is

data Stage as where
  AskKnownSize :: Stage ()
  AskSize      :: Stage (Bool, ())
  AskWeight    :: Stage (Bool, ())
  AskHeight    :: Stage (Weight, (Bool, ()))
  AskColour    :: Stage (Size, (Bool, ()))

deriving instance Show (Stage as)

data Suspended where
  Suspended :: (Show as) => Stage as -> as -> Suspended

instance Show Suspended where
  show (Suspended stage as) = show stage ++ ", " ++ show as

instance Read Suspended where
  readsPrec = const $ uncurry ($) . mapFst parse . fmap (drop 2) . break (==',')
    where
      parse :: String -> String -> [(Suspended, String)]
      parse stage = case stage of
        "AskKnownSize" -> parse' AskKnownSize
        "AskSize"      -> parse' AskSize
        "AskWeight"    -> parse' AskWeight
        "AskHeight"    -> parse' AskHeight
        "AskColour"    -> parse' AskColour
        _ -> const []

      parse' :: (Show as, Read as) => Stage as -> String -> [(Suspended, String)]
      parse' stg st = [(Suspended stg (read st), mempty)]

      mapFst :: (a -> c) -> (a, b) -> (c, b)
      mapFst f ~(a, b) = (f a, b)


type SizeFlowResult = (Colour, (Size, (Bool, ())))

resume :: Suspended -> StateMachine as SizeFlowResult ()
resume (Suspended AskKnownSize e) =
  iput e >>>
  askKnownSize >>>= \ b ->
  resume' (if b then AskSize else AskWeight) (b, e)

resume (Suspended AskSize e) =
  iput e >>>
  askSize >>>
  iget >>>= resume' AskColour

resume (Suspended AskWeight e) =
  iput e >>>
  askWeight >>>
  iget >>>= resume' AskHeight

resume (Suspended AskHeight e) =
  iput e >>>
  askHeight >>>
  imodify (\(h, (w, xs)) -> (calculateSize h w, xs)) >>>
  iget >>>= resume' AskColour

resume (Suspended AskColour e) =
  iput e >>>
  askColour

resume' :: Show as => Stage as -> as -> StateMachine as SizeFlowResult ()
resume' stage as = suspend stage >>> resume (Suspended stage as)

-- given persist :: Suspended -> IO ()
suspend :: (Show as) => Stage as -> StateMachine as as ()
suspend stage =
  iget >>>= \env ->
  ilift (persist (Suspended stage env))


sizeFlow :: Flow Suspended as SizeFlowResult ()
sizeFlow = Flow resume


-- resumeFlow () "AskWeight, (False,())" sizeFlow

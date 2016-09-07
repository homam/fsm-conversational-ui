{-# LANGUAGE TupleSections, GADTs, StandaloneDeriving, FlexibleInstances #-}

module V1TryAtHome
  (
    tryAtHomeFlow, Suspended, TryAtHomeFlowResult
  )
where

import V1IState
import V1Flow
import qualified V1SizeFlow as SF

newtype Product = Product Int deriving (Read, Show)
newtype Street = Street String deriving (Read, Show)
newtype MobileNumber = MobileNumber String deriving (Read, Show)

askProduct :: StateMachine as (Product, as) Product
askProduct = askNumber "Which product you want to try?" Product (,)

askStreet :: StateMachine as (Street, as) Street
askStreet = askString "What is your street?" Street (,)

askMobileNumber :: StateMachine as (MobileNumber, as) MobileNumber
askMobileNumber = askString "What is your street?" MobileNumber (,)

askSize :: StateMachine as (SF.SizeFlowResult, as) SF.SizeFlowResult
askSize = askFlow "AskKnownSize, ()" SF.sizeFlow >>++ (,)

data Stage as where
  AskProduct :: Stage ()
  AskStreet  :: Stage (Product, ())
  AskMobileNumber  :: Stage (Street, (Product, ()))
  AskSize :: Stage (MobileNumber, (Street, (Product, ())))

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
        "AskProduct"      -> parse' AskProduct
        "AskStreet"       -> parse' AskStreet
        "AskMobileNumber" -> parse' AskMobileNumber
        "Asksize"         -> parse' AskSize
        _ -> const []

      parse' :: (Show as, Read as) => Stage as -> String -> [(Suspended, String)]
      parse' stg st = [(Suspended stg (read st), mempty)]

      mapFst :: (a -> c) -> (a, b) -> (c, b)
      mapFst f ~(a, b) = (f a, b)


type TryAtHomeFlowResult = (SF.SizeFlowResult, (MobileNumber, (Street, (Product, ()))))

resume :: Suspended -> StateMachine as TryAtHomeFlowResult ()
resume (Suspended AskProduct e) =
  iput e >>>
  askProduct >>>
  iget >>>= resume' AskStreet

resume (Suspended AskStreet e) =
  iput e >>>
  askStreet >>>
  iget >>>= resume' AskMobileNumber

resume (Suspended AskMobileNumber e) =
  iput e >>>
  askMobileNumber >>>
  iget >>>= resume' AskSize

resume (Suspended AskSize e) =
  iput e >>>
  askSize >>> ireturn ()



resume' :: Show as => Stage as -> as -> StateMachine as TryAtHomeFlowResult ()
resume' stage as = suspend stage >>> resume (Suspended stage as)

-- given persist :: Suspended -> IO ()
suspend :: (Show as) => Stage as -> StateMachine as as ()
suspend stage =
  iget >>>= \ env ->
  ilift (persist (Suspended stage env))

tryAtHomeFlow :: Flow Suspended as TryAtHomeFlowResult ()
tryAtHomeFlow = Flow resume

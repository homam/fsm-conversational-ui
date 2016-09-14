{-# LANGUAGE TupleSections, GADTs, StandaloneDeriving, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-}

module IndexedStateMaybe2

where

newtype IStateT m i o a = IStateT { runIState :: i -> m (o, a) }

class IMonad m where
  ireturn :: a -> m i i a
  (>>>=) :: m i j a -> (a -> m j k b) -> m i k b

(>>>) :: IMonad m => m i j a -> m j k b -> m i k b
mx >>> my = mx >>>= const my

class IMonadTrans t where
  ilift :: Monad m => m a -> t m i i a

iget :: Monad m => IStateT m s s s
iget = IStateT $ \s -> return (s, s)

iput :: Monad m => o -> IStateT m i o ()
iput x = IStateT $ \_ -> return (x, ())

imodify :: Monad m => (i -> o) -> IStateT m i o ()
imodify f = IStateT $ \s -> return (f s, ())

instance Monad m => IMonad (IStateT m) where
  ireturn x = IStateT (\s -> return (s, x))
  IStateT f >>>= g = IStateT $ \s -> do
    (s', x) <- f s
    let IStateT h = g x
    h s'

instance IMonadTrans IStateT where
  ilift m = IStateT $ \s -> m >>= \x -> return (s, x)

type StateMachine = IStateT IO



newtype Size = Size Int deriving (Read, Show)
newtype Height = Height Int deriving (Read, Show)
newtype Weight = Weight Int deriving (Read, Show)
newtype Colour = Colour String deriving (Read, Show)

askKnownSize :: StateMachine as (Maybe Bool, as) Bool
askKnownSize = askYN "Do you know your size?"

-- askSize takes an environment of type as and adds a Size element
askSize :: StateMachine as (Maybe Size, as) ()
askSize = askNumber "What is your size?" Size

receiveSize :: String -> StateMachine as (Maybe Size, as) ()
receiveSize = receiveNumber Size

-- askHeight takes an environment of type as and adds a Height element
askHeight :: StateMachine as (Maybe Height, as) ()
askHeight = askNumber "What is your height?" Height

-- etc
askWeight :: StateMachine as (Maybe Weight, as) ()
askWeight = askNumber "What is your weight?" Weight

askColour :: StateMachine as (Colour, as) ()
askColour =
  ilift (putStrLn "What is your favourite colour?") >>>
  ilift readLn >>>= \answer ->
  imodify (Colour answer,)

calculateSize :: Height -> Weight -> Size
calculateSize (Height h) (Weight w) = Size (h - w)  -- or whatever the calculation is

askNumber :: String -> (Int -> a) -> StateMachine as (Maybe a, as) ()
askNumber question mk =
  ilift (putStrLn question) >>>
  ilift readLn >>>= \answer ->
  case reads answer of
    [(x, _)] -> imodify (mk <$> Just x,)
    _ -> ilift (putStrLn "Please type a number") >>> askNumber question mk

justAskColour :: StateMachine as (Maybe Colour, as) ()
justAskColour =
  ilift (putStrLn "What is your favourite colour?") >>>
  imodify (Nothing, )

receiveNumber :: (Int -> a) -> String -> StateMachine as (Maybe a, as) ()
receiveNumber mk answer =
  case reads answer of
    [(x, _)] -> imodify (mk <$> x,)
    _ -> imodify (Nothing, ) >>> ireturn ()

receiveYN :: String -> StateMachine as (Maybe Bool, as) (Maybe Bool)
receiveYN answer =  case answer of
  "y" -> imodify (Just True,) >>> ireturn (Just True)
  "n" -> imodify (Just False,) >>> ireturn (Just False)
  _ -> imodify (Nothing, ) >>> ireturn Nothing

askYN :: String -> StateMachine as (Maybe Bool, as) Bool
askYN question =
  ilift (putStrLn question) >>>
  ilift readLn >>>= \answer ->
  case answer of
    "y" -> imodify (Just True,) >>> ireturn True
    "n" -> imodify (Just False,) >>> ireturn False
    _ -> ilift (putStrLn "Please type y or n") >>> askYN question


data Stage as where
  AskKnownSize :: Stage ()
  AskSize      :: Stage (Maybe Bool, ())
  AskWeight    :: Stage (Maybe Bool, ())
  AskHeight    :: Stage (Maybe Weight, (Maybe Bool, ()))
  AskColour    :: Stage (Maybe Size, (Maybe Bool, ()))

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


type SMResult = (Colour, (Maybe Size, (Maybe Bool, ())))

-- WORKS: runIState (resume (read "AskColour, (Size 33, (True, ()))" )) ()
-- WORKS: runIState (resume (read "AskKnownSize, ()" )) ()

-- runIState (resume (Suspended AskWeight (False, ()) )) ()
-- runIState (resume (Suspended AskHeight (Weight 40, ()))) ()

resume :: Suspended -> StateMachine as SMResult ()
resume (Suspended AskKnownSize e) =
  iput e >>>
  askKnownSize >>>= \ b ->
  resume' (if b then AskSize else AskWeight) (Just b, e)

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
  imodify (\(h, (w, xs)) -> (calculateSize <$> h <*> w, xs)) >>> -- TODO: none-exhausive pattern mathching
  iget >>>= resume' AskColour

resume (Suspended AskColour e) =
  iput e >>>
  askColour


resume' :: Show as => Stage as -> as -> StateMachine as SMResult ()
resume' stage as = suspend stage >>> resume (Suspended stage as)

-- given persist :: Suspended -> IO ()
suspend :: (Show as) => Stage as -> StateMachine as as ()
suspend stage =
  iget >>>= \env ->
  ilift (persist (Suspended stage env))

persist :: Suspended -> IO ()
persist = print

-- main = runIState interaction () >>= print
-- main = runIState (resume (read "AskKnownSize, ()" )) () >>= print
main = resume'' "AskKnownSize, ()" sizeFlow ()

-- runIState (suspend AskHeight ) (Weight 4, ())
-- runIState (suspend AskWeight) ()

newtype Flow sp as o a  = Flow { unFlow :: sp -> StateMachine as o a }

sizeFlow :: Flow Suspended as SMResult ()
sizeFlow = Flow resume

resume'' :: (Read sp, Show o, Show a) => String -> Flow sp i o a -> i -> IO ()
resume'' st flow i = runIState (unFlow flow (read st)) i >>= print

--- ----

data Flows sp as o a where
  SizeFlow :: Flow Suspended as SMResult () -> Flows sp as o a
  TryAtHomeFlow :: Flow Suspended as (Colour, (Size, (Int, ()))) () -> Flows sp as o a

getFlow "SizeFlow" = SizeFlow sizeFlow
getFlow "TryAtHomeFlow" = TryAtHomeFlow undefined


---
ask :: Suspended -> StateMachine as SMResult ()
ask (Suspended AskKnownSize e) =
  iput e >>>
  suspend AskKnownSize >>>
  askKnownSize >>>= \ b ->
  resume' (if b then AskSize else AskWeight) (Just b, e)

-- -- runIState (answer "5" (Suspended AskSize (True, ()))) () AskColour, (Size 5,(True,()))
--
-- runIState (answer "y" (Suspended AskKnownSize ())) ()
-- runIState (answer "haha" (Suspended AskKnownSize ())) () -- to test error handling
-- runIState (answer "22" (Suspended AskSize (Just True,()))) ()
-- runIState (answer "red" (Suspended AskColour (Size 22, (Just True,())))) ()
answer :: String -> Suspended -> StateMachine as SMResult  ()
answer answer (Suspended AskSize e) =
  iput e >>>
  receiveSize answer >>>
  iget >>>= \ s -> resume' AskColour s

answer answer (Suspended AskKnownSize e) =
  iput e >>>
  receiveYN answer >>>
  iget >>>= \ s@(b, as) -> -- resume' AskSize s
    case b of
      Nothing -> ilift (putStrLn "invalid input, please enter either y or n") >>> resume (Suspended AskKnownSize ()) -- error "invalid input"
      Just True -> resume' AskSize s
      Just False -> resume' AskWeight s

--TODO: answer

{-# LANGUAGE TupleSections, GADTs, StandaloneDeriving, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-}

module IndexedState

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

askKnownSize :: StateMachine as (Bool, as) Bool
askKnownSize = askYN' "Do you know your size?"

-- askSize takes an environment of type as and adds a Size element
askSize :: StateMachine as (Size, as) ()
askSize = askNumber "What is your size?" Size

-- askHeight takes an environment of type as and adds a Height element
askHeight :: StateMachine as (Height, as) ()
askHeight = askNumber "What is your height?" Height

-- etc
askWeight :: StateMachine as (Weight, as) ()
askWeight = askNumber "What is your weight?" Weight

askColour :: StateMachine as (Colour, as) ()
askColour =
  -- poor man's do-notation. You could use RebindableSyntax
  ilift (putStrLn "What is your favourite colour?") >>>
  ilift readLn >>>= \answer ->
  imodify (Colour answer,)

calculateSize :: Height -> Weight -> Size
calculateSize (Height h) (Weight w) = Size (h - w)  -- or whatever the calculation is

askNumber :: String -> (Int -> a) -> StateMachine as (a, as) ()
askNumber question mk =
  ilift (putStrLn question) >>>
  ilift readLn >>>= \answer ->
  case reads answer of
    [(x, _)] -> imodify (mk x,)
    _ -> ilift (putStrLn "Please type a number") >>> askNumber question mk

askYN :: String -> StateMachine as as Bool
askYN question =
  ilift (putStrLn question) >>>
  ilift readLn >>>= \answer ->
  case answer of
    "y" -> ireturn True
    "n" -> ireturn False
    _ -> ilift (putStrLn "Please type y or n") >>> askYN question

askYN' :: String -> StateMachine as (Bool, as) Bool
askYN' question =
  ilift (putStrLn question) >>>
  ilift readLn >>>= \answer ->
  case answer of
    "y" -> imodify (True,) >>> ireturn True
    "n" -> imodify (False,) >>> ireturn False
    _ -> ilift (putStrLn "Please type y or n") >>> askYN' question


-- interaction :: StateMachine xs (Colour, (Size, xs)) ()
-- interaction :: interaction :: StateMachine () (Colour, (Size, ())) ()
interaction =
  (suspend AskKnownSize >>> askKnownSize) >>>= \ answer ->
  askOrCalculateSize answer >>>
  askColour

  where
    askOrCalculateSize True = suspend AskSize >>> askSize
    askOrCalculateSize False =
      (suspend AskWeight >>> askWeight) >>>
      (suspend AskHeight >>> askHeight) >>>
      imodify (\ (h, (w, xs)) -> (calculateSize h w, xs)) >>> suspend AskColour


-- interaction' :: (Functor m, Show as) => i -> IStateT m i as t -> Stage as -> m Suspended
-- interaction' as machine stage =  (\(r, _) -> Suspended stage r) <$> runIState machine as
--
-- interaction'' =
--   askYN "Do you know your size?" >>>= \ ans -> suspend AskWeight

-- interaction' () askWeight AskHeight
-- interaction' () askWeight AskHeight


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


-- WORKS: runIState (resume (read "AskColour, (Size 33, (True, ()))" )) ()
-- WORKS: runIState (resume (read "AskKnownSize, ()" )) ()

-- runIState (resume (Suspended AskWeight (False, ()) )) ()
-- runIState (resume (Suspended AskHeight (Weight 40, ()))) ()

resume :: Suspended -> StateMachine as (Colour, (Size, (Bool, ()))) ()
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

resume' :: Show as => Stage as -> as -> StateMachine as (Colour, (Size, (Bool, ()))) ()
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

newtype Flow sp as o a  = Flow {unFlow :: sp -> StateMachine as o a}

sizeFlow :: Flow Suspended as (Colour, (Size, (Bool, ()))) ()
sizeFlow = Flow resume

resume'' :: (Read sp, Show o, Show a) => String -> Flow sp i o a -> i -> IO ()
resume'' st flow i = runIState (unFlow flow (read st)) i >>= print



--- ----

data Flows sp as o a where
  SizeFlow :: Flow Suspended as (Colour, (Size, (Bool, ()))) () -> Flows sp as o a
  TryAtHomeFlow :: Flow Suspended as (Colour, (Size, (Int, ()))) () -> Flows sp as o a

getFlow "SizeFlow" = SizeFlow sizeFlow
getFlow "TryAtHomeFlow" = TryAtHomeFlow undefined

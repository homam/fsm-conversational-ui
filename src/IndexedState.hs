{-# LANGUAGE TupleSections, GADTs #-}

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
  ilift readLn                                      >>>= \answer ->
  imodify (Colour answer,)

calculateSize :: Height -> Weight -> Size
calculateSize (Height h) (Weight w) = Size (h - w)  -- or whatever the calculation is

askNumber :: String -> (Int -> a) -> StateMachine as (a, as) ()
askNumber question mk =
  ilift (putStrLn question) >>>
  ilift readLn              >>>= \answer ->
  case reads answer of
    [(x, _)] -> imodify (mk x,)
    _ -> ilift (putStrLn "Please type a number") >>> askNumber question mk

askYN :: String -> StateMachine as as Bool
askYN question =
  ilift (putStrLn question) >>>
  ilift readLn              >>>= \answer ->
  case answer of
    "y" -> ireturn True
    "n" -> ireturn False
    _ -> ilift (putStrLn "Please type y or n") >>> askYN question

interaction :: StateMachine xs (Colour, (Size, xs)) ()
interaction =
  askYN "Do you know your size?" >>>= \answer ->
  askOrCalculateSize answer >>>
  askColour

  where
    askOrCalculateSize True = askSize
    askOrCalculateSize False =
      askWeight >>>
      askHeight >>>
      imodify (\ (h, (w, xs)) -> (calculateSize h w, xs))



interaction' :: StateMachine xs (Stage as) ()
interaction' = undefined

data Stage as where
  AskSize   :: Stage ()
  AskWeight :: Stage ()
  AskHeight :: Stage (Weight, ())
  AskColour :: Stage (Size, ())

instance Show (Stage as) where
  show AskSize = "AskSize"

data Suspended where
  Suspended :: (Show as) => Stage as -> as -> Suspended

instance Show Suspended where
  show (Suspended stage as) = show stage ++ ", " ++ show as

-- runIState (resume (Suspended AskHeight (Weight 40, ()))) ()

resume :: Suspended -> StateMachine () (Colour, (Size, ())) ()
resume (Suspended AskSize e) =
  iput e                                               >>>
  askSize                                              >>>
  askColour
resume (Suspended AskWeight e) =
  iput e                                               >>>
  askWeight                                            >>>
  askHeight                                            >>>
  imodify (\(h, (w, xs)) -> (calculateSize h w, xs))   >>>
  askColour
resume (Suspended AskHeight e) =
  iput e                                               >>>
  askHeight                                            >>>
  imodify (\(h, (w, xs)) -> (calculateSize h w, xs))   >>>
  askColour
resume (Suspended AskColour e) =
  iput e                                               >>>
  askColour

-- given persist :: Suspended -> IO ()
suspend :: (Show as) => Stage as -> StateMachine as as ()
suspend stage =
    iget                                  >>>= \env ->
    ilift (persist (Suspended stage env))

persist :: Suspended -> IO ()
persist = print

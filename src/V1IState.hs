module V1IState
  (
    IStateT(..), IMonad(..), IMonadTrans(..), StateMachine,
    (>>>), iget, iput, imodify,
    askNumber, askYN, askString
  )
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


-- utility Ask functions:

askNumber :: String -> (Int -> a) -> (a -> as -> as') -> StateMachine as as' a
askNumber question mk mks =
  ilift (putStrLn question) >>>
  ilift readLn >>>= \ answer ->
  case reads answer of
    [(x, _)] -> let y = mk x in imodify (mks y) >>> ireturn y
    _ -> ilift (putStrLn "Please type a number") >>> askNumber question mk mks

askString :: String -> (String -> a) -> (a -> as -> as') -> StateMachine as as' a
askString question mk mks =
  ilift (putStrLn question) >>>
  ilift readLn >>>= \ answer -> let y = mk answer in imodify (mks y) >>> ireturn y

askYN :: String -> (Bool -> as -> as') -> StateMachine as as' Bool
askYN question mks =
  ilift (putStrLn question) >>>
  ilift readLn >>>= \ answer ->
  case answer of
    "y" -> imodify (mks True) >>> ireturn True
    "n" -> imodify (mks False) >>> ireturn False
    _ -> ilift (putStrLn "Please type y or n") >>> askYN question mks

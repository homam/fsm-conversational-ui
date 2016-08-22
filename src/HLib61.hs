{-# LANGUAGE RankNTypes, GADTs,
  MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, AllowAmbiguousTypes #-}

module HLib61
where
import Control.Category ((>>>))
import Control.Monad ((>=>))
import Data.Dynamic

type UserInput = String
class (Read n, Show n) => Serializable n
instance forall n. (Read n, Show n) => Serializable [n]
instance (Serializable a, Serializable b) => Serializable (a, b)
instance Serializable Int
instance Serializable ()

data Node s o = QNode {
  execQNode :: (Serializable s, Serializable o) => s -> UserInput -> Target o
} | ANode {
  execANode :: (Serializable s, Serializable o) => s -> Target o
}


data Target o where
    Continuation :: forall o o'. (Serializable o, Serializable o') => Target (Node o o')
    Recursion :: forall o o'. (Serializable o, Serializable o') => Target (Node o o')
    EOD :: forall o. (Serializable o) => o -> Target o
    Start :: forall o o'. {- (Serializable o, Serializable o') => -} (o' -> o) -> Node () o' -> Target o
    Fork :: forall o o' o''. (Serializable o, Serializable o', Serializable o'') => o' -> Node o' o'' -> Node o'' o -> Target o

-- data Target o =
--     forall o'. Continuation (Node o o')
--   | forall o'. Recursion (Node o o')
--   | EOD o
--   | forall o'. Start (o' -> o) (Node () o')
--   | forall o' o''. Fork o' (Node o' o'') (Node o'' o)

askForANumber :: Node () Int
askForANumber = QNode {
  execQNode = const $ EOD . read
}

getTwoNumbers :: Node [Int] [Int]
getTwoNumbers = ANode {
  -- execQNode = \ s -> if length s < 2 then (3:s, Recursion getTwoNumbers) else (s, EOD)
  execANode = \ s -> if length s < 2 then Start (:s) askForANumber else EOD s
}



getOperation :: Node () String
getOperation = QNode {
  -- what do you want to do with these numbers?
  execQNode = const EOD
}

getTwoNumbersAndOperation :: Node () ([Int], String)
getTwoNumbersAndOperation = ANode {
  -- give us two numbers and an operation
  execANode = const $ Fork [] getTwoNumbers makeOperation
}

makeOperation :: Node [Int] ([Int], String)
makeOperation = ANode {
  -- mempty
  execANode = \ s -> Start (\ b -> (s, b)) getOperation
}

applyOperation :: Node ([Int], String) Int
applyOperation = ANode {
  -- and the result is
  execANode = \ (num, op) -> EOD $ sum num
}

someOperation :: Node () Int
someOperation = ANode {
  execANode = const $ Fork () getTwoNumbersAndOperation applyOperation
}



execNode :: (Serializable s, Serializable o) => s -> Node s o -> IO (Target o)
execNode s (QNode f) = do
  i <- getLine
  return $ f s i
execNode s (ANode f) = return $ f s

data ConversatioNodes n where
  SomeOperation :: ConversatioNodes (Node () Int)
  GetTwoNumbersAndOperation :: ConversatioNodes (Node () ([Int], String))

getNode :: ConversatioNodes n -> n
getNode SomeOperation = someOperation
getNode GetTwoNumbersAndOperation = getTwoNumbersAndOperation

getNode' :: String -> Dynamic
getNode' "A" = toDyn askForANumber

class (Serializable s, Serializable o) => Executable n s o  where
  exec :: n -> s -> (UserInput -> Target o)

instance (Serializable i, Serializable o) => Executable (Node i o) i o where
  exec (ANode f) = const . f
  exec (QNode f) = f

-- woo ::  (Executable (Node [Int] [Int]) s o, Executable (Node () Int) s o) => String -> s -> UserInput -> Target o
woo "A" = exec askForANumber  -- someOperation
woo "B" = exec getTwoNumbers
woo "C" = exec someOperation

-- woo = maybe (return $ EOD 0) id (execNode () <$> Just askForANumber)
-- woo' :: IO (Target Int)
-- woo' :: (Executable (Node [Int] [Int]) () o, Executable (Node () Int) () o) => Target o
woo' key s = do
  r <- getLine
  return $ woo key s r

hoop :: (Executable (Node [Int] [Int]) () o, Executable (Node [Int] [Int]) s o, Executable (Node () Int) s o) => s -> UserInput -> Target o
hoop = woo "A"
--
-- boop :: IO ()
-- boop = getLine >>= \r -> let a = hoop () r in return ()

-- tee ::  (Serializable i, Serializable o, Executable n i o) => n -> IO ()
-- tee n = getLine >>= \r -> let a =  exec askForANumber () r in return ()

data Func where
  Func :: (A -> B) -> Func

-- data B b where
--   BInt :: Int -> B Int
--   BString :: String -> B String

data A = AInt Int | AString String deriving (Show)
data B = BInt Int | BString String deriving (Show)

class Funcable f where
  fexec :: (Show b) => f -> A -> B

instance Funcable Func where
  fexec (Func f) = f


f1 :: Func
f1 = Func $ \ (AInt i) -> BInt $ i * 2
--
f2 :: Func
f2 = Func $ \ (AString i) -> BString $ i ++ "!"

-- getF :: (Funcable f, Show a, Show b) => String -> a -> B
getF "f1" = f1
getF "f2" = f2
-- getF "f2" = f2

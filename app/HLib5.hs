{-# LANGUAGE RankNTypes, GADTs #-}

module HLib5
where
import Control.Category ((>>>))
import Control.Monad ((>=>))

type UserInput = String

class (Read n, Show n) => Serializable n

data DuckTarget b = forall c. DuckTarget (Duck b c) | EOD | forall a c. SOD (Duck a c)

data DuckResult b = DuckResult {
  duckResult :: b,
  duckTarget :: DuckTarget b
}

data Duck a b = QDuck {
  message :: String,
  duckF  :: (Serializable a, Serializable b) => UserInput -> a -> DuckResult b
} | ADuck {
  message :: String,
  autoF :: (Serializable a, Serializable b) => a -> DuckResult b
}



askForANumber :: Duck a Int
askForANumber = QDuck {
  message = "Give me a number",
  duckF = \ i _ -> DuckResult (read i :: Int) EOD
}

getTwoNumbers :: Duck [Int] [Int]
getTwoNumbers = ADuck {
  message = "Getting two numbers",
  autoF = \ s -> DuckResult s (if length s < 2 then DuckTarget askForANumber else EOD)
}

sumTwoNumbers :: Duck [Int] Int
sumTwoNumbers = ADuck {
  message = "Summing two numbers",
  autoF = \ s -> DuckResult (sum s) EOD
}

getAndSumTwoNumbers :: Duck [Int] ()
getAndSumTwoNumbers = ADuck {
  message = mempty,
  autoF = \ s -> DuckResult () $ if null s then SOD getTwoNumbers else SOD sumTwoNumbers
}

-- someFunc :: [Int] -> IO a

-- someFunc :: (Show r, Read r, Num r) => [r] -> IO ()
someFunc nums = if length nums == 2 then return nums else do
  ns <- getLine
  someFunc $ read ns : nums

sumThem ns = do
  print $ sum ns
  return $ sum ns

{-# LANGUAGE GADTs, RankNTypes, TypeOperators, TupleSections #-} -- , MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, UndecidableInstances, ScopedTypeVariables #-}

module SurveyMinimal2
where

import Data.Typeable
import Control.Monad (void)

type SurveyId = String
type Prompt = String

data Survey a where
  Respond ::  (Typeable a, Show a, Read a) => SurveyId -> Prompt -> (String -> a) -> Survey a
  Cross   :: (Typeable b, Typeable c, Read b, Read c, Show b, Show c) => SurveyId -> Survey b -> Survey c -> Survey (b, c)
  Cond    :: Typeable b => SurveyId -> Survey b -> (b -> Survey a) -> Survey (b, a)
  Always  :: (Typeable a, Show a, Read a) => SurveyId -> a -> Survey a

data Link a b where
  Continue :: Survey a -> Survey b -> (a -> b -> Survey (a, b)) -> Link a b

l1 = Continue yourNameS yourAgeS (curry $ Always "nameAndAge")

yourNameS = Respond "name" "What is your name?" id
yourAgeS = Respond "age" "What is your age?" (read :: String -> Int)

yourFavDrinkS = Respond "drink" "Your fav drink?" Just -- (Just :: String -> Maybe String)
notOldEnoughS = Always "nothing" Nothing

yourAgeDrinkS = Cond "age->drink" yourAgeS (\ i -> if i > 21 then yourFavDrinkS else notOldEnoughS )
yourNameAgeDrinkS = Cross "name,age->drink" yourNameS yourAgeDrinkS


askRespondSurvey :: [(SurveyId, Maybe a)] -> Survey a -> IO ()
askRespondSurvey us (Respond n prompt _) = do
  print $ (n, Nothing):us
  putStrLn prompt




readInt :: String -> Int
readInt = read

readTuple :: String -> (Int, Bool)
readTuple = read

readBool :: String -> Bool
readBool = read


program' first second maker = do
  i <- fmap first getLine
  b <- fmap second getLine
  return $ maker i b

-- program = program'' readInt readBool (,) (Nothing, Nothing)


program'' first second (a, b) =
  maybe
    (fmap ((, Nothing) . Just . first) getLine)
    (\a' -> maybe (fmap ((Just a',) . Just . second) getLine) (return . (Just a',) . Just) b)
    a

program1 = program'' readInt readBool -- (Nothing, Nothing)
program2 = program'' readInt (read :: String -> (Int, Int)) -- program2 $ read "(Just 5, Nothing)"

{-# LANGUAGE GADTs, RankNTypes, TupleSections #-}

module SurveyMinimal
where

import Data.Typeable

type Prompt = String
type SurveyId = String
data UserState where
  UserState :: (Typeable a, Show a, Read a) => [(SurveyId, a)] -> UserState

data Survey a where
  Respond :: Typeable a => Prompt -> (String -> a) -> Survey a
  Always  :: Typeable a => a -> Survey a
  (:~?>:)  :: (Typeable b, Typeable a) => Survey b -> (b -> Maybe (Survey a)) -> Survey (b, Maybe a)
  (:~:>:)  :: (Typeable b, Typeable a) => Survey b -> (b -> Either (Survey a) (Survey c)) -> Survey (b, Either a c)
  (:<+>:) :: (Typeable b, Typeable a, Show a, Show b) => Survey a ->  Survey b -> Survey (a, b)

(~:>) :: (Typeable b, Typeable a, Typeable c) => Survey b -> (b -> Either (Survey a) (Survey c)) -> IO (Survey (b, Either a c))
s ~:> f = do
  b <- runSurvey s
  case f b of
    (Left sa)  -> fmap (Always . (b,) . Left) (runSurvey sa)
    (Right sc) -> fmap (Always . (b,) . Right) (runSurvey sc)

(~?>) :: (Typeable b, Typeable a) => Survey b -> (b -> Maybe (Survey a)) -> IO (Survey (b, Maybe a))
s ~?> f = do
  b <- runSurvey s
  case f b of
    (Just sa)  -> fmap (Always . (b,) . Just) (runSurvey sa)
    Nothing -> return $ Always (b, Nothing)

(<+:>) ::  (Typeable b, Typeable a) => Survey a -> IO (Survey b) -> IO (Survey (a, b))
s1 <+:> ms2 = do
  a <- runSurvey s1
  s2 <- ms2
  b <- runSurvey s2
  return $ Always (a, b)


yourNameS = Respond "What is your name?" id
yourAgeS = Respond "What is your age?" (read :: String -> Int)

yourFavDrinkS = Respond "Your fav drink?" id
participateS = Respond "Participate in next surveys?" (read :: String -> Bool)

whatIsYourNameS = yourNameS <+:> (yourAgeS ~?> \ i -> if i >= 21 then Just yourFavDrinkS else Nothing)
whatIsYourNameS' = yourNameS :<+>: (yourAgeS :~?>: \ i -> if i >= 21 then Just yourFavDrinkS else Nothing) :<+>: participateS

whatIsYourNameS'' st =
  maybe
    (yourNameS :<+>: (yourAgeS :~?>: \ i -> if i >= 21 then Just yourFavDrinkS else Nothing))
    (Always . read)
    st
  :<+>: participateS

-- runSurvey $ mkReadable Nothing yourNameS participateS
mkReadable :: (Read a, Typeable a, Typeable b, Show a, Show b) => Maybe String -> Survey a -> Survey b -> Survey (a, b)
mkReadable st sa sb = maybe sa (Always . read) st :<+>: sb

test = yourNameS :<+>: participateS

runSurvey' :: (Show a) => Maybe String -> Maybe String -> Survey a -> IO ()
runSurvey' _ input (Respond prompt parser) = case input of
  Nothing -> putStrLn prompt
  (Just ans) -> print $ parser ans

-- more runSurvey' needed

runSurvey :: Survey a -> IO a

runSurvey (Respond prompt parser) = do
  putStr ""
  putStr $ prompt ++ " "
  ans <- getLine
  return $ parser ans

runSurvey (s :~?>: f) = do
  b <- runSurvey s
  case f b of
    (Just sa) -> fmap ((b,) . Just) (runSurvey sa)
    Nothing   -> return (b, Nothing)

runSurvey (s :~:>: f) = do
  b <- runSurvey s
  case f b of
    (Left sa)  -> fmap ((b,) . Left) (runSurvey sa)
    (Right sc) -> fmap ((b,) . Right) (runSurvey sc)

runSurvey (s1 :<+>: s2) = do
  a <- runSurvey s1
  b <- runSurvey s2
  return (a, b)

runSurvey (Always a) = return a

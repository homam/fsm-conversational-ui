{-# LANGUAGE GADTs, ScopedTypeVariables, TypeOperators, PolyKinds, DataKinds, PartialTypeSignatures, TypeSynonymInstances, FlexibleInstances#-}

module Survey
where

import Data.Typeable

type Prompt = String

data Survey a where
  Respond :: Typeable a => Prompt -> (String -> a) -> Survey a
  Choose  :: Typeable a => Prompt -> Choice a -> Survey a
  (:+:)   :: Survey b -> Survey c -> Survey (b, c)
  Group   :: String -> Survey a -> Survey a

data Choice a where
  Item   :: Typeable a => Prompt -> a -> Choice a
  (:|:)  :: Choice a -> Choice a -> Choice a
  (:||:) :: Choice b -> Choice c -> Choice (Either b c)
  (:->:) :: Typeable b => Choice b -> Survey c -> Choice (b, c)

type DepSurvey a b = Survey (Either a (a, b))
type CondSurvey b = DepSurvey Bool b

text :: Prompt -> Survey String
text p = Respond p id

prompt :: Prompt -> Choice Prompt
prompt p = Item p p

prompted :: Prompt -> Survey a -> Choice (Prompt, a)
prompted p = (prompt p :->:)


type Name = String
type Age = Int

name :: Survey Name
name = Respond "Your name" id

age :: Survey Age
age = Respond "Your age" (read :: String -> Int)

person :: Survey (Name, Age)
person = name :+: age

personal :: Survey (Name, Age)
personal = Group "Your personal infromation" person

rating :: Survey Int
rating = Choose "Rating" $ Item "Excellent" 3 :|: Item "Good" 1 :|:  Item "Bad" (-1)

voteQuestion :: Survey (Either Char Bool)
voteQuestion = Choose
  "Did you vote/who did you vote for?" $
    (Item "Candidate A" 'a' :|:
     Item "Candidate B" 'b' :|:
     Item "Candidate C" 'c')
  :||: (Item "Didn’t vote" False :|:
        Item "Rather not say" True)

existingCustomer :: CondSurvey String
existingCustomer = Choose "Have you bought anything from us before?" $
  Item "No" False :||: (Item "Yes" True :->: Respond "Which?" id)

numSurvey :: CondSurvey Int
numSurvey = Choose "Have you bought anything from us before?" $
  Item "No" False :||: (Item "Yes" True :->: Respond "How many?" (read :: String -> Int))

-- getSurvey :: Typeable a => String -> Survey a
--getSurvey "existingCustomer" = existingCustomer
--getSurvey "existingCustomer1" = existingCustomer1
-- getSurvey "age" = age

data Surveys = CondString (CondSurvey String) | CondNum (CondSurvey Int)

getSurvey "existingCustomer" = CondString existingCustomer
getSurvey "numSurvey" = CondNum numSurvey

run = do
  l <- getLine
  case getSurvey l of
    (CondString s) -> do
      runSurvey s
      return ()
    (CondNum s) -> do
      runSurvey s
      return ()


main = runSurvey existingCustomer >>= print


-- runSurvey :: forall a. Typeable a => Survey a -> IO a
runSurvey :: Survey a -> IO a

runSurvey (Group name sub) = do
    putStrLn $ "\n" ++ name
    putStrLn $ replicate (length name) '='
    runSurvey sub

runSurvey (Respond prompt parser) = do
    putStr ""
    putStr $ prompt ++ " "
    ans <- getLine
    return $ parser ans

runSurvey (Choose prompt choiceExp) = do
    putStr $ "\n" ++ prompt ++ ": \n"
    dispChoices 1 choiceExp
    ans <- readLn :: IO Int
    selected (ans - 1) choiceExp
        where
            dispChoices :: Int -> Choice a -> IO Int
            dispChoices num (Item text _) = do
                putStrLn $ "[" ++ show num ++ "] " ++ text
                return $ succ num
            dispChoices num (choice :->: _) = dispChoices num choice
            dispChoices num (l :|: r) = disp2 num l r
            dispChoices num (l :||: r) = disp2 num l r

            disp2 :: Int -> Choice a -> Choice b -> IO Int
            disp2 num l r = do
                next <- dispChoices num l
                dispChoices next r

runSurvey (left :+: right) = do
    la <- runSurvey left
    ra <- runSurvey right
    return (la, ra)

selected :: Int -> Choice a -> IO a
selected 0 (Item _ val) = return val
selected n (choice :->: sub) = do
    choiceval <- selected n choice
    subval <- runSurvey sub
    return (choiceval, subval)
selected n (l :|: r)
    | n < llen  = selected n l
    | otherwise = selected (n - llen) r
    where llen = choiceLength l
selected n (l :||: r)
    | n < llen = Left <$> selected n l
    | otherwise = Right <$> selected (n-llen) r
    where llen = choiceLength l

choiceLength :: Choice a -> Int
choiceLength (Item _ _) = 1
choiceLength (c :->: s) = choiceLength c
choiceLength (l :|: r) = choiceLength l + choiceLength r
choiceLength (l :||: r) = choiceLength l + choiceLength r

-- runAS :: forall a. Typeable a => AllSurveys -> IO a
-- runAS (AS s) = runSurvey s
--
-- data AllSurveys where
--   AS :: Typeable a => Survey a -> AllSurveys
--
-- getSurvey :: String -> AllSurveys
-- getSurvey "age" = AS age
-- getSurvey "personal" = AS personal

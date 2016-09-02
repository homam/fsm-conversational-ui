{-# LANGUAGE GADTs, RankNTypes #-}

module TestExist
where

import Data.Typeable

type Prompt = String
type SurveyId = String
data UserState where
  UserState :: (Typeable a, Show a, Read a) => [(SurveyId, a)] -> UserState

data Survey a where
  Respond :: Typeable a => Prompt -> (String -> a) -> Survey a
  Choose  :: Typeable a => Prompt -> Choice a -> Survey a
  (:++:)  :: Typeable b => Survey b -> Survey c -> Survey (b, c)
  (:~>:)  :: Typeable b => Survey b -> (b -> Survey a) -> Survey a
  Always  :: a -> Survey a

(~:>) :: Survey b -> (b -> Survey a) -> IO (Survey (b, a))
s ~:> f = do
  b <- runSurvey s
  a <- runSurvey $ f b
  return $ Always (b, a)

(<++>) ::  (Typeable b, Typeable a) => Survey a -> Survey b -> IO (Survey (a, b))
s1 <++> s2 = do
  a <- runSurvey s1
  b <- runSurvey s2
  return $ Always (a, b)

(<++:>) ::  (Typeable b, Typeable a) => Survey a -> IO (Survey b) -> IO (Survey (a, b))
s1 <++:> ms2 = do
  a <- runSurvey s1
  s2 <- ms2
  b <- runSurvey s2
  return $ Always (a, b)

data Choice a where
  Item   :: Typeable a => Prompt -> a -> Choice a
  (:||:) :: Choice b -> Choice c -> Choice (Either b c)
  (:->:) :: Typeable b => Choice b -> Survey c -> Choice (b, c)

type DepSurvey a b = Survey (Either a (a, b))
type CondSurvey b = DepSurvey Bool b

existingCustomerSurvey :: CondSurvey String
existingCustomerSurvey = Choose "Have you used any of our products before?" $
  Item "No" False :||: (Item "Yes" True :->: whichItemSurvey)

whichItemSurvey :: Survey String
whichItemSurvey = Respond "Which one?" id

whatIsYourName = Respond "What is your name?" id :++: (Respond "What is your age?" (read :: String -> Int) :~>: (\ i -> Respond "not old enough" (\ x -> (x, i))))

yourNameS = Respond "What is your name?" id
yourAgeS = Respond "What is your age?" (read :: String -> Int)

yourFavDrinkS = Respond "Your fav drink?" Just
notOldEnoughS = Always Nothing

whatIsYourName' = yourNameS <++:> (yourAgeS ~:> \ i -> if i >= 21 then yourFavDrinkS else notOldEnoughS)

runSurvey :: Survey a -> IO a

runSurvey (Respond prompt parser) = do
    putStr ""
    putStr $ prompt ++ " "
    ans <- getLine
    return $ parser ans

runSurvey (s1 :++: s2) = do
  a1 <- runSurvey s1
  a2 <- runSurvey s2
  return (a1, a2)

runSurvey (Always a) = return a

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
            dispChoices num (l :||: r) = disp2 num l r

            disp2 :: Int -> Choice a -> Choice b -> IO Int
            disp2 num l r = do
                next <- dispChoices num l
                dispChoices next r

selected :: Int -> Choice a -> IO a
selected 0 (Item _ val) = return val
selected n (choice :->: sub) = do
    choiceval <- selected n choice
    subval <- runSurvey sub
    return (choiceval, subval)
selected n (l :||: r)
    | n < llen = Left <$> selected n l
    | otherwise = Right <$> selected (n-llen) r
    where llen = choiceLength l

choiceLength :: Choice a -> Int
choiceLength (Item _ _) = 1
choiceLength (c :->: s) = choiceLength c
choiceLength (l :||: r) = choiceLength l + choiceLength r

-- main = getLine >>= (runQuestion . getQuestion) >>= print

-- type Input = String
-- type QuestionName = String
--
--
-- data Node s o = QNode {
--   name :: String,
--
--   -- s is the result of the last Node
--   -- Input is user input
--   run :: s -> Input -> (o, Target)
-- }
--
-- instance C.Category Node where
--   id = QNode {
--     name = "",
--     run = \ s i -> (s, End)
--   }
--   a . b = QNode {
--     name = (name b) ++ " - " ++ (name a),
--     run = \ s i ->
--   }
--
--
-- data Target where
--     ContinueWith :: forall o' o. (Show o, Show o', Read o, Read o', Typeable o, Typeable o') => Node o o' -> Target
--     End :: Target
--
--
-- yourName :: Node () String
-- yourName = QNode {
--   name = "yourName",
--   run = \_ input -> (input, ContinueWith yourAge )
-- }
--
-- yourAge :: Node String (String, Int)
-- yourAge = QNode {
--   name = "yourAge",
--   run = \ name input -> let res = (name, read input :: Int) in (res, End)
-- }
--
-- -- getNode :: (Show a, Show b, Read a, Read b) => String -> Node a b
-- getNode "yourName" = yourName
-- -- getNode "yourAge" = yourAge
--
-- main = do
--   nodeName <- getLine --
--   s <- getLine -- or from a DB
--   input <- getLine
--   case cast s of
--     Just s' -> do
--       let (result, target) = run (getNode nodeName) s'  input
--       putStrLn result -- save in DB
--       case target of
--         End -> putStrLn "End"
--         (ContinueWith node) -> putStrLn $ "Next node is : " ++ name node -- and save name node in a DB
--     Nothing -> print "error"
--   --  print result

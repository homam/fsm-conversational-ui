{-# LANGUAGE ScopedTypeVariables, BangPatterns #-}

module TestFSM5 where
import System.IO
import Control.Monad.State
import Control.Exception

type AOFlow = String
data UserInput = UIActionSumTwoNumbers | UIActionQuit | UIInt Int | UIAny
data AppOut = AOEnd String | AOLink App | AOMessage String App
data UserState = USInts [Int] | USInt Int deriving (Read, Show)
type NodeId = String

data App = Await {
  name :: NodeId,
  message :: String,
  parser :: String -> Maybe UserInput,
  handler :: UserInput -> StateT UserState IO AppOut
} | Yield {
  name :: NodeId,
  next :: StateT UserState IO AppOut
} | Flower {
  name :: NodeId,
  withFlow :: String,
  continueWith :: UserState {- result of the flow -} -> StateT UserState IO AppOut
}

getANumber = Await {
  name = "start",
  message = "Enter a number",
  parser = fmap UIInt . readMaybe,
  handler = \e -> case e of
    UIInt i -> do
      put (USInt i)
      return $ AOEnd ""
}


start = Await {
  name = "start",
  message = "What do you want to do?",
  parser = \ i -> case i of
    "quit" -> Just UIActionQuit
    "sum" -> Just UIActionSumTwoNumbers
    _ -> Nothing
  ,
  handler = \e -> case e of
    UIActionSumTwoNumbers -> return $ AOLink getTwoNumbers
    UIActionQuit -> return $ AOEnd "Goodbye!"
}

getTwoNumbers = Flower {
  name = "getTwoNumbers",
  withFlow = "getANumberFlow",
  continueWith = \e -> case e of
    USInt i -> do
      (USInts is) <- get
      let is' = i:is
      put $ USInts is'
      return $ if length is' == 2 then AOLink getTwoNumbers else AOEnd "Thanks, we got two numbers"
}

sumTwoNumbers = Yield {
  name = "sumTwoNumbers",
  next = do
    (USInts is) <- get
    return $ AOEnd ("And the sum is " ++ show (sum is))
}

getApp :: NodeId -> App
getApp "start" = start
getApp "getTwoNumbers" = getTwoNumbers
getApp "sumTwoNumbers" = sumTwoNumbers
getApp n = error $ "No state found for " ++ n




run :: App -> UserInput -> UserState -> IO ()
run oApp e userState = do
  (mApp, userState') <- run' oApp e userState
  case mApp of
    AOEnd message -> writeFile "./temp" "" >> print message
    AOLink app -> writeFile "./temp" (show (name app, userState')) >> go userState' app
    AOMessage message app -> writeFile "./temp" (show (name app, userState')) >> print message >> go userState' app
  where
    go :: UserState -> App -> IO ()
    go _ a@Await{} = print (message a)
    go u a@(Yield _ handler) = run a UIAny u

    run' :: App -> UserInput -> UserState -> IO (AppOut, UserState)
    run' (Await _ _ _ handler) = runStateT . handler
    run' (Yield _ next) = const $ runStateT next


runApp :: App -> String -> UserState -> IO ()
runApp oApp input userState = case parser oApp input of
    Nothing -> print "Invalid Input"
    Just e -> run oApp e userState


main = do
  stFile <- readFileMaybe "./temp"
  case (stFile >>= readMaybe) :: Maybe (NodeId, UserState) of
    Nothing -> do
      let app = start
      print $ message app
      input <- getLine
      runApp start input (USInts [])
    Just (nodeId, userState) -> do
      input <- getLine
      runApp (getApp nodeId) input userState


-- utils

readMaybe :: (Read a) => String -> Maybe a
readMaybe s = case reads s of
  [] -> Nothing
  [(a, _)] -> Just a

readFileMaybe :: String -> IO (Maybe String)
readFileMaybe path = do
  stFile <- try $ readAFile path
  case stFile of
    Left (e :: IOException) -> return Nothing
    Right !f -> return $ Just f

readAFile :: String -> IO String
readAFile path = do
  inFile <- openFile path ReadMode
  contents <- hGetContents inFile
  contents `seq` hClose inFile
  return contents

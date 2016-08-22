{-# LANGUAGE ScopedTypeVariables #-}

module TestFSM3 where
import System.IO
import Control.Monad.State
import Control.Exception

data UserInput = UIActionSumTwoNumbers | UIActionQuit | UIInt Int | UIAny
data AppOut = AOEnd String | AOLink App | AOMessage String App
type UserState = [Int]
type NodeId = String

data App = App {
  name :: NodeId,
  message :: String,
  parser :: String -> Maybe UserInput,
  handler :: UserInput -> StateT UserState IO AppOut
  -- can put state-specific stuff like ring tone, display props, etc here
}

idle = App {
  name = "idle",
  message = "What do you want to do?",
  parser = \ i -> case i of
    "quit" -> Just UIActionQuit
    "sum" -> Just UIActionSumTwoNumbers
    _ -> Nothing
  ,
  handler = \e -> case e of
    UIActionSumTwoNumbers -> return $ AOLink getANumber
    UIActionQuit -> return $ AOEnd "Goodbye!"
}

getANumber = App {
  name = "getANumber",
  message = "Enter a number",
  parser = fmap UIInt . readMaybe,
  handler = \e -> case e of
  UIInt i -> do
    is <- get
    let is' = i:is
    put is'
    return $ if length is' == 2 then AOLink sumTwoNumbers else AOMessage "Thanks ..." getANumber
}

sumTwoNumbers = App {
  name = "sumTwoNumbers",
  message = "Press enter to get the sum",
  parser = const $ Just UIAny,
  handler = const $ do
    is <- get
    return $ AOEnd ("And the sum is " ++ show (sum is))
}

getApp :: NodeId -> App
getApp "idle" = idle
getApp "getANumber" = getANumber
getApp "sumTwoNumbers" = sumTwoNumbers
getApp n = error $ "No state found for " ++ n




run :: App -> UserInput -> UserState -> IO (AppOut, UserState)
run app = runStateT . handler app


getUserInput :: App -> IO UserInput
getUserInput app = getLine >>= \c -> case parser app c of
  Nothing -> print "Invalid Input" >> getUserInput app
  Just i -> return i


runApp :: NodeId -> UserState -> IO ()
runApp nodeId userState = do
  let oApp = getApp nodeId
  print $ message oApp
  e <- getUserInput oApp
  (mApp, userState') <- run oApp e userState
  case mApp of
    AOEnd message -> writeFile "./temp" "" >> print message
    AOLink app -> writeFile "./temp" (show (name app, userState'))
    AOMessage message app -> writeFile "./temp" (show (name app, userState')) >> print message

main = do
  stFile <- readFileMaybe "./temp"
  case (stFile >>= readMaybe) :: Maybe (NodeId, UserState) of
    Nothing -> runApp "idle" []
    Just (nodeId, userState) -> runApp nodeId userState



-- utils

readMaybe :: (Read a) => String -> Maybe a
readMaybe s = case reads s of
  [] -> Nothing
  [(a, _)] -> Just a

readFileMaybe :: String -> IO (Maybe String)
readFileMaybe path = do
  stFile <- try $ readFile path
  case stFile of
    Left (e :: IOException) -> return Nothing
    Right f -> return $ Just f

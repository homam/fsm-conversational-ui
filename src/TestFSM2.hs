{-# LANGUAGE ScopedTypeVariables #-}

module TestFSM2 where
import System.IO
import Control.Monad.State
import Control.Exception

data UserInput = UIActionSumTwoNumbers | UIActionQuit | UIInt Int | UIAny

getUserInput :: App -> IO UserInput
getUserInput app = getLine >>= \c -> case parser app c of
  Nothing -> print "Invalid Input" >> getUserInput app
  Just i -> return i

type UserState = [Int]
type NodeId = String

data App = App {
  name :: NodeId,
  message :: String,
  parser :: String -> Maybe UserInput,
  handler :: UserInput -> StateT UserState IO (Maybe App)
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
    UIActionSumTwoNumbers  -> return $ Just getANumber
    UIActionQuit  -> lift $ putStrLn "\tQuitting" >> return Nothing
    _ -> lift $ putStrLn "Unexpected Input" >> return ( Just idle )
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
    return $ Just $ if length is' == 2 then sumTwoNumbers else getANumber
}

sumTwoNumbers = App {
  name = "sumTwoNumbers",
  message = "Press enter to get the sum",
  parser = const $ Just UIAny,
  handler = const $ do
    is <- get
    lift $ print $ sum is
    return $ Just idle
}

getApp :: NodeId -> App
getApp "idle" = idle
getApp "getANumber" = getANumber
getApp "sumTwoNumbers" = sumTwoNumbers
getApp n = error $ "No state found for " ++ n




run :: App -> UserInput -> UserState -> IO (Maybe App, UserState)
run app = runStateT . handler app

runApp :: NodeId -> UserState -> IO ()
runApp nodeId userState = do
  let oApp = getApp nodeId
  print $ message oApp
  e <- getUserInput oApp
  (mApp, userState') <- run oApp e userState
  case mApp of
    Nothing -> writeFile "./temp" "" >> print "Done"
    Just app -> writeFile "./temp" (show (name app, userState'))

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

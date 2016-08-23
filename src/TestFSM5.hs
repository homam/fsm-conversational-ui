{-# LANGUAGE ScopedTypeVariables, BangPatterns #-}

module TestFSM5 where
import System.IO
import Control.Monad.State
import Control.Exception

type AOFlow = String
data UserInput = UIActionSumTwoNumbers | UIActionQuit | UIInt Int | UIAny
data AppOut = AOEnd String FlowResult | AOLink App | AOMessage String App
data FlowResult = FRNoting | FRInt Int | FRInts [Int]
data UserState = USNothing | USInts [Int] | USInt Int | USArithOperation ([Int], String) deriving (Read, Show)
type NodeId = String
type FlowId = String

data App = Await {
  name :: NodeId,
  message :: String,
  parser :: String -> Maybe UserInput,
  handler :: UserInput -> StateT UserState IO AppOut
} | Yield {
  next :: StateT UserState IO AppOut
} | Flower {
  name :: NodeId,
  withFlow :: (FlowId, UserState),
  continueWith :: FlowResult -> StateT UserState IO AppOut
} | YieldFlower {
  withFlow :: (FlowId, UserState)
}

-- globalFlow

globalApp = Await {
  name = "start",
  message = "What do you want to do?",
  parser = \ i -> case i of
    "quit" -> Just UIActionQuit
    "sum" -> Just UIActionSumTwoNumbers
    _ -> Nothing
  ,
  handler = \e -> case e of
    UIActionSumTwoNumbers -> return $ AOLink YieldFlower { withFlow = ("arithFlow", USArithOperation ([], mempty)) }
    UIActionQuit -> return $ AOEnd "Goodbye!" FRNoting
}

globalFlow :: NodeId -> App
globalFlow "start" = globalApp


-- getANumberFlow

getANumberApp = Await {
  name = "start",
  message = "Enter a number",
  parser = fmap UIInt . readMaybe,
  handler = \e -> case e of
    UIInt i ->
      return $ AOEnd "" (FRInt i)
}

getANumberFlow :: NodeId -> App
getANumberFlow "start" = getANumberApp




-- arithOperationApp

arithFlow :: NodeId -> App
arithFlow "start" = arithApp
arithFlow "nextNumber" = arithAppGetNextNumber
arithFlow "sum" = sumTwoNumbersSubApp

arithApp = Flower {
  name = "start",
  withFlow = ("getANumberFlow", USNothing),
  continueWith = \ (FRInt i) -> do
    USArithOperation (nums, op) <- get
    put $ USArithOperation (i:nums, op)
    return $ AOLink arithAppGetNextNumber
}

arithAppGetNextNumber = Flower {
  name = "nextNumber",
  withFlow = ("getANumberFlow", USNothing),
  continueWith = \ (FRInt i) -> do
    USArithOperation (nums, op) <- get
    put $ USArithOperation (i:nums, op)
    return $ AOLink sumTwoNumbersSubApp
}

sumTwoNumbersSubApp = Yield {
  next = do
    USArithOperation (nums, op) <- get
    let s = sum nums
    return $ AOEnd ("And the sum is " ++ show s) (FRInt s)
}







getFlow :: FlowId -> (NodeId -> App)
getFlow "globalFlow" = globalFlow
getFlow "getANumberFlow" = getANumberFlow
getFlow "arithFlow" = arithFlow



type SerializableState = [(FlowId, (NodeId, UserState))]

initialSerState :: SerializableState
initialSerState = [("globalFlow", ("start", USNothing))]

handleAppOut :: FlowId -> AppOut -> UserState -> SerializableState -> IO ()
handleAppOut flowId (AOEnd message result) _ suState = do
  print message
  let (flowId', (nodeId', userState')) = head suState -- TODO: handle SerializableState with 0
  case getFlow flowId' nodeId' of
    app@Flower{} -> do
      (appOut', userState'') <- runStateT (continueWith app result) userState'
      handleAppOut flowId' appOut' userState'' (drop 1 suState)
    app@Await{} -> do
      let (flowId'', (nodeId'', userState'')):rest =  suState
      runAppBeforeInput flowId'' app userState'' rest
    _ -> error "Expected a Flower node"
handleAppOut flowId (AOLink app) userState usState = runAppBeforeInput flowId app userState usState
handleAppOut flowId (AOMessage message app) userState suState = do
  print message
  runAppBeforeInput flowId app userState suState

saveSerializableState :: SerializableState -> IO ()
saveSerializableState = writeFile "./temp" . show

runAppBeforeInput :: FlowId -> App -> UserState -> SerializableState -> IO ()
runAppBeforeInput flowId app@Await{} userState rest = do
  saveSerializableState ((flowId, (name app, userState)):rest)
  print $ message app
runAppBeforeInput flowId app@Yield{} userState rest = runStateT (next app) userState >>= flip (uncurry (handleAppOut flowId)) rest
runAppBeforeInput flowId app@Flower{} userState rest = do
  let (flowId', userState') = withFlow app
  let rest' = (flowId, (name app, userState)):rest
  saveSerializableState $ (flowId', ("start", userState')):rest'
  runAppBeforeInput flowId' (getFlow flowId' "start") userState' rest'
runAppBeforeInput flowId app@YieldFlower{} userState rest = do
  let (flowId', userState') = withFlow app
  let rest' = (flowId, ("start", userState)):rest
  saveSerializableState $ (flowId', ("start", userState')):rest'
  runAppBeforeInput flowId' (getFlow flowId' "start") userState' rest'

runAppAfterInput :: App -> UserState -> SerializableState -> UserInput -> IO ()
runAppAfterInput = undefined

run ::  FlowId -> App -> UserInput -> UserState -> SerializableState -> IO ()
run flowId oApp e userState rest = do
  (mApp, userState') <- run' oApp e userState
  handleAppOut flowId mApp userState' rest
  where

    run' :: App -> UserInput -> UserState -> IO (AppOut, UserState)
    run' (Await _ _ _ handler) = runStateT . handler
    run' (Yield next) = const $ runStateT next
    run' Flower{} = error "Flower cannot handle user input"


runApp :: FlowId -> App -> String -> UserState -> SerializableState -> IO ()
runApp flowId oApp input userState rest = case parser oApp input of
    Nothing -> print "Invalid Input"
    Just e -> run flowId oApp e userState rest


main = do
  stFile <- readFileMaybe "./temp"
  case (stFile >>= readMaybe) :: Maybe SerializableState of
    Nothing -> do
      let (flowId, (nodeId, userState)):rest = initialSerState
      let app = getFlow flowId nodeId
      runAppBeforeInput flowId app userState rest
      input <- getLine
      runApp flowId app input userState rest
    Just suState@((flowId, (nodeId, userState)):rest) -> do
      input <- getLine
      runApp flowId (getFlow flowId nodeId) input userState rest


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

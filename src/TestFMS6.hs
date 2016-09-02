{-# LANGUAGE ScopedTypeVariables, BangPatterns #-}
-- {-# LANGUAGE RankNTypes, GADTs, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}

module TestFMS6 where
import System.IO
import Control.Monad.State
import Control.Exception
import qualified Data.Map as M

type Message = String

data UserInput = UIActionSumTwoNumbers | UIActionQuit | UIInt Int | UIString String | UIAny
data AppOut = AOEnd (Maybe Message) FlowResult | AOLink (Maybe Message) App | AOFlower (FlowId, UserState)
data FlowResult = FRNoting | FRInt Int | FRInts [Int]
data UserState = USNothing | USInts [Int] | USInt Int | USArithOperation ([Int], String) deriving (Read, Show)
type NodeId = String
type FlowId = String

data App = Await {
  name :: NodeId,
  message :: UserState -> Message,
  parser :: String -> Maybe UserInput,
  handler :: UserInput -> StateT UserState IO AppOut -- UserInput
} | Flower {
  name :: NodeId,
  withFlow :: (FlowId, UserState),
  continueWith :: FlowResult -> StateT UserState IO AppOut
}

-- globalFlow

globalApp = Await {
  name = "start",
  message = const "What do you want to do?",
  parser = \ i -> case i of
    "quit" -> Just UIActionQuit
    "sum" -> Just UIActionSumTwoNumbers
    _ -> Nothing
  ,
  handler = \e -> case e of
    UIActionSumTwoNumbers -> return $ AOFlower ("arithFlow", USArithOperation ([], mempty))
    UIActionQuit -> return $ AOEnd (Just "Goodbye!") FRNoting -- TODO: End is meaningless for globalApp
}

globalFlow :: NodeId -> App
globalFlow "start" = globalApp


-- getANumberFlow

getANumberApp = Await {
  name = "start",
  message = const "Enter a number",
  parser = fmap UIInt . readMaybe,
  handler = \ (UIInt i) -> return $ AOEnd Nothing (FRInt i)
}

getANumberFlow :: NodeId -> App
getANumberFlow "start" = getANumberApp




-- arithOperationApp

arithFlow :: NodeId -> App
arithFlow "start" = arithApp
arithFlow "nextNumber" = arithAppGetNextNumber
arithFlow "getOperation" = getOperation

arithApp = Flower {
  name = "start",
  withFlow = ("getANumberFlow", USNothing),
  continueWith = \ (FRInt i) -> do
    USArithOperation (nums, op) <- get
    put $ USArithOperation (i:nums, op)
    return $ AOLink Nothing arithAppGetNextNumber
}

arithAppGetNextNumber = Flower {
  name = "nextNumber",
  withFlow = ("getANumberFlow", USNothing),
  continueWith = \ (FRInt i) -> do
    USArithOperation (nums, op) <- get
    put $ USArithOperation (i:nums, op)
    return $ AOLink Nothing getOperation
}

getOperation = Await {
  name = "getOperation",
  message = \ (USArithOperation (b:a:_, _)) -> "What operation do you want to do? with " ++ show a ++ " and " ++ show b,
  parser = \ s -> if s `elem` ["*", "+", "-"] then Just (UIString s) else Nothing,
  handler = \ (UIString op) -> do
    (USArithOperation (b:a:_, _)) <- get
    let operation = M.lookup op $ M.fromList [("*", (*)), ("+", (+)), ("-", (-))]
    return $ maybe
      (AOLink (Just $ "Sorry we didn't recognize the given operation: " ++ op ++ ". It's weird") getOperation)
      (\o ->
        let s = a `o` b
        in  AOEnd (Just $ "And the sum is " ++ show s) (FRInt s)
      )
      operation
}



getFlow :: FlowId -> (NodeId -> App)
getFlow "globalFlow" = globalFlow
getFlow "getANumberFlow" = getANumberFlow
getFlow "arithFlow" = arithFlow


type SerializableState = [(FlowId, (NodeId, UserState))]

initialSerState :: SerializableState
initialSerState = [("globalFlow", ("start", USNothing))]

saveSerializableState :: SerializableState -> IO ()
saveSerializableState = writeFile "./temp" . show


handleAppOut :: FlowId -> AppOut -> UserState -> SerializableState -> IO ()
handleAppOut flowId (AOEnd message result) _ suState = do
  print message
  let (flowId', (nodeId', userState')) = head suState -- TODO: handle SerializableState with 0
  case getFlow flowId' nodeId' of
    app@Flower{} -> do
      (appOut', userState'') <- runStateT (continueWith app result) userState'
      handleAppOut flowId' appOut' userState'' (drop 1 suState)
    app@Await{} -> do
      -- app here must be the globalApp (at the end of YieldFlower)
      let (flowId'', (nodeId'', userState'')):rest =  suState
      runAppBeforeInput flowId'' app userState'' rest
handleAppOut flowId (AOLink message app) userState suState = do
  maybe (return ()) print message
  runAppBeforeInput flowId app userState suState
handleAppOut flowId (AOFlower (flowId', userState')) userState rest = do
  let rest' = (flowId, ("start", userState)):rest
  saveSerializableState $ (flowId', ("start", userState')):rest'
  runAppBeforeInput flowId' (getFlow flowId' "start") userState' rest'

runAppBeforeInput :: FlowId -> App -> UserState -> SerializableState -> IO ()
runAppBeforeInput flowId app@Await{} userState rest = do
  saveSerializableState ((flowId, (name app, userState)):rest)
  print $ message app userState
runAppBeforeInput flowId app@Flower{} userState rest = do
  let (flowId', userState') = withFlow app
  let rest' = (flowId, (name app, userState)):rest
  saveSerializableState $ (flowId', ("start", userState')):rest'
  runAppBeforeInput flowId' (getFlow flowId' "start") userState' rest'

run ::  UserInput -> FlowId -> App -> UserState -> SerializableState -> IO ()
run e flowId oApp userState rest = do
  (mApp, userState') <- run' oApp e userState
  handleAppOut flowId mApp userState' rest
  where

    run' :: App -> UserInput -> UserState -> IO (AppOut, UserState)
    run' (Await _ _ _ handler) = runStateT . handler
    run' Flower{} = error "Flower cannot handle user input"


runApp :: FlowId -> App -> String -> UserState -> SerializableState -> IO ()
runApp flowId oApp input userState rest = case parser oApp input of
    Nothing -> do
      print "Invalid Input"
      case oApp of
        Await{} -> print $ message oApp userState
        _       -> error "Input received for a non-Await app"
    Just e -> run e flowId oApp userState rest


main = do
  stFile <- readFileMaybe "./temp"
  case (stFile >>= readMaybe) :: Maybe SerializableState of
    Nothing -> do
      -- if it's the first run then just run the globalApp
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

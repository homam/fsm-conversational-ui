{-# LANGUAGE ScopedTypeVariables #-}

module TestFSM where
import System.IO
import Control.Monad.State
import Control.Exception

data Event = LoYes | LoNo | LoNum -- buttons on your phone
           | ReYes | ReNo | ReNum -- buttons on his phone

getEvent :: IO Event
getEvent =
  getChar >>= \c -> case c of
  'y' -> return LoYes
  'n' -> return LoNo
  '0' -> return LoNum
  'Y' -> return ReYes
  'N' -> return ReNo
  '1' -> return ReNum
  _   -> getEvent

type UserState = String
type NodeId = String

data App = App {
  name :: NodeId,
  handler :: Event -> StateT UserState IO (Maybe App)
  -- can put state-specific stuff like ring tone, display props, etc here
  }

idle, ringing, waiting, talking :: App

idle = App "idle" $ \e -> case e of
  LoNum -> lift $ putStrLn "\tCalling somebody" >> return ( Just waiting )
  ReNum -> lift $ putStrLn "\tIt's for you-hoo" >> return ( Just ringing )
  LoNo  -> lift $ putStrLn "\tQuitting"         >> return Nothing
  ReNo  -> lift $ putStrLn "\tQuitting"         >> return Nothing
  _     ->                                  return ( Just idle )

waiting = App "waiting" $ \e -> case e of
  ReYes -> lift $ putStrLn "\tHe accepted"      >> return ( Just talking )
  ReNo  -> lift $ putStrLn "\tHe rejected"      >> return ( Just idle )
  LoNo  -> lift $ putStrLn "\tI got bored"      >> return ( Just idle )
  _     ->                                  return ( Just waiting )

ringing = App "ringing" $ \e -> case e of
  LoYes -> lift $ putStrLn "\tI accepted"       >> return ( Just talking )
  LoNo  -> lift $ putStrLn "\tI rejected"       >> return ( Just idle )
  ReNo  -> lift $ putStrLn "\tHe got bored"     >> return ( Just idle )
  _     ->                                  return ( Just ringing )

talking = App "talking" $ \e -> case e of
  LoNo  -> lift $ putStrLn "\tI'm hanging up"   >> return ( Just idle )
  ReNo  -> lift $ putStrLn "\tHe hung up"       >> return ( Just idle )
  _     ->                                  return ( Just talking )

getApp :: NodeId -> App
getApp "idle" = idle
getApp "waiting" = waiting
getApp "ringing" = ringing
getApp "talking" = talking
getApp n = error $ "No state found for " ++ n

run :: App -> IO ()
run st = do
  e <- getEvent
  let machine = handler st e
  (mApp, nState) <- runStateT machine ""
  maybe (return ()) run mApp
--   let (eff, app) = runState handler st
--   undefined
--   -- >>= maybe (return ()) run
--
main =
  hSetBuffering stdin NoBuffering >> -- so you don't have to hit return
  run idle


run2 :: App -> Event -> UserState -> IO (Maybe App, UserState)
run2 app = runStateT . handler app

runApp :: NodeId -> UserState -> IO ()
runApp nodeId userState = do
  e <- getEvent
  (mApp, userState') <- run2 (getApp nodeId) e userState
  case mApp of
    Nothing -> writeFile "./temp" "" >> print "Done"
    Just app -> writeFile "./temp" (show (name app, userState'))

main2 = do
  stFile <- readFileMaybe "./temp"
  case (stFile >>= readMaybe) :: Maybe (NodeId, UserState) of
    Nothing -> runApp "idle" ""
    Just (nodeId, userState) -> runApp nodeId userState



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

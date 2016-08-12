{-# LANGUAGE MultiParamTypeClasses, OverloadedStrings, FlexibleInstances, ScopedTypeVariables, RankNTypes #-}

module Lib
    ( someFunc
    )
where

import Control.Arrow ((>>>), (&&&), (<<<), (<<^), (<+>), arr)
import Control.Monad ((>=>), liftM)
import qualified Data.Map as M
import Data.List (isSubsequenceOf)
import qualified Data.Set as S
import RedisHelper (getRedis, saveRedis)
import Data.Maybe (fromMaybe)
import Node
import Serializable
import UserState

type UserId = String

saveUserState :: UserId -> UserState -> IO ()
saveUserState = undefined -- save to redis


getUser :: UserId -> IO (Maybe (UFSM UserState))
getUser userId = getRedis userId >>= maybe
  (return Nothing)
  (\ user ->
     do let _ufsm = deserialize user :: UFSM UserState
        return $ Just _ufsm)

toEither :: a -> Maybe b -> Either a b
toEither _ (Just b) = Right b
toEither a _ = Left a

messageFromUser :: UserId -> String -> IO String
messageFromUser userId message = do -- getUser >>= reconstructUFSM >>= currentNode.receive >>= if Right then fine the edge, edge.exec and update UFSM and send the question to user
  user <- getUser userId -- user :: UFSM
  maybe
    (error "No user was found")
    (\user ->
      either
        error -- print error (user input is not valid)
        (\ us -> do
            let nuser = user {userState = us}
            saveUser userId nuser
            case findEdgeUFSM user of
                Nothing -> error "Edge not found"
                (Just e) -> case exec e us of
                  Nothing -> do
                    print $ "exec edge lead to Nothing, edge.node: " ++ show (nodeId $ node e)
                    return ""
                  (Just n) -> do
                    saveUser userId (nuser { currentNode = n })
                    return $ ask n
        )
        (receive (currentNode user) message (userState user)) -- receive
    )
    user
  -- return ()

newUser :: UserId -> IO ()
newUser = flip saveUser $ UFSM sizeSelection whatIsYourHeight False (UserState Nothing Nothing)

saveUser :: UserId -> UFSM UserState -> IO ()
saveUser userId = saveRedis userId . serialize


someFunc :: IO ()
someFunc = print "hello"

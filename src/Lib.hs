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


toEither :: a -> Maybe b -> Either a b
toEither _ (Just b) = Right b
toEither a _ = Left a

messageFromUser :: UserId -> String -> IO String
messageFromUser userId message = do -- getUser >>= reconstructUConversation >>= currentNode.receive >>= if Right then fine the edge, edge.exec and update UConversation and send the question to user
  user <- getUser userId -- user :: UConversation
  maybe
    (error "No user was found")
    (\user ->
      either
        error -- print error (user input is not valid)
        (\ us -> do
            let nuser = user {userState = us}
            saveUser userId nuser
            case findEdgeUConversation user of
                Nothing -> error "Edge not found"
                (Just e) -> case exec e us of
                  Nothing -> do
                    print $ "exec edge lead to Nothing, edge.node: " ++ show (nodeId $ node e)
                    return ""
                  (Just n) -> do
                    saveUser userId (nuser { currentNode = n })
                    return $ question n
        )
        (receive (currentNode user) message (userState user)) -- receive
    )
    user
  -- return ()

newUser :: UserId -> IO ()
newUser = flip saveUser $ UConversation sizeSelection whatIsYourHeight False (UserState Nothing Nothing)

saveUser :: UserId -> MyUConversation -> IO ()
saveUser userId = saveRedis userId . serialize

getUser :: UserId -> IO (Maybe MyUConversation)
getUser = liftM (fmap deserialize) . getRedis

someFunc :: IO ()
someFunc = print "hello"

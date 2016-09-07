{-# LANGUAGE CPP, OverloadedStrings #-}

module RedisHelper (getRedis, saveRedis) where

import Text.Read hiding (get)
import Control.Monad
import Database.Redis
import Control.Monad.IO.Class
import Data.ByteString.Char8 (pack, unpack)

saveRedis :: String -> String -> IO ()
saveRedis key value = do
  conn <- connect defaultConnectInfo
  runRedis conn $ do
    _ <- set (pack key) (pack value)
    return ()


getRedis :: String -> IO (Maybe String)
getRedis key = do
  conn <- connect defaultConnectInfo
  runRedis conn $ do
    a <- get (pack key)
    liftIO $ return $ go a
      where
        go (Left _) = Nothing
        go (Right m) = fmap unpack m

readRedisValue :: Read a => String -> IO (Maybe a)
readRedisValue key = do
  s <- getRedis key
  return $ join $ readMaybe <$> s

{-# LANGUAGE CPP, OverloadedStrings #-}

module RedisHelper (getRedis, saveRedis) where

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
    liftIO $ putStrLn $ "get " ++ key ++ show a
    liftIO $ go a
      where
        go (Left a) = return Nothing
        go (Right m) = return $ fmap unpack m

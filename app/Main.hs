{-# LANGUAGE CPP, OverloadedStrings #-}

module Main where

import Lib
import System.Environment
import Database.Redis
import Control.Monad.IO.Class

-- main :: IO (Either Reply Status)
-- main = do
--   conn <- connect defaultConnectInfo
--   runRedis conn $ set "a" "apple"
--   runRedis conn $ do
--      result <- get "a"
--      liftIO $ print result


main = undefined

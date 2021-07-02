{-# LANGUAGE OverloadedStrings #-}

module Clients where

import Control.Monad (replicateM)
import System.Random (randomRIO)
import qualified Database.Redis as R
import qualified Data.ByteString.Char8 as BC

idIsFree :: BC.ByteString -> R.Connection -> IO Bool
idIsFree id rConn = do
  response <- R.runRedis rConn (R.get id)
  pure $ case response of
    (Left _)  -> False -- maybe error here? look into what causes this more
    (Right x) ->
      case x of
        (Just _) -> True
        _        -> False

genClientId :: IO String
genClientId = replicateM 8 (randomRIO ('a', 'z'))

-- todo: config file
maxClients :: Int
maxClients = 100

getClients :: IO Int
getClients = undefined

acceptingClients :: IO Bool
acceptingClients = do
  clients <- getClients
  pure $ clients < maxClients

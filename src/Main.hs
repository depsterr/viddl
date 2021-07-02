{-# LANGUAGE OverloadedStrings #-}

module Main where

import Templates.Index
import Templates.Loading
import Templates.Error
import YTDL
import Clients
import Helpers

import Control.Monad.IO.Class
import qualified Database.Redis as R
import qualified Data.Text.Lazy as TL
import Web.Scotty

-- todo ReaderT
app :: R.Connection -> ScottyM ()
app rConn = do
  get "/" $ do
    html indexPage

  post "/" $ do
    url <- param "url"
    res <- param "resolution"
    if (isURL url) && (isRes res)
      then do
        queueOK <- liftIO acceptingClients
        if queueOK
          then do
            html loadingPage
            -- set redis stuff and id here
            -- redirect $ TL.pack $ '/':id
          else
            html $ errorPage "Too many clients right now. Try again later!"
        else
        html $ errorPage "Invalid input!"

  get "/:id" $ do
    -- grab id and process video if not already done
    id <- param "id"
    html id

main :: IO ()
main = do
  -- todo: parse connection config
  rConn <- R.connect R.defaultConnectInfo
  scotty 3000 (app rConn)

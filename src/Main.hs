{-# LANGUAGE OverloadedStrings #-}

module Main where

import Templates.Index
import Templates.Loading
import Templates.Error

import Control.Monad.IO.Class
import qualified Database.Redis as R
import qualified Data.Text.Lazy as TL
import Web.Scotty
import Network.URI (URI, parseURI)

data Resolution
  = P144
  | P240
  | P360
  | P480
  | P720
  | P1080
  | PMAX
  | PMIN
  | Audio
  deriving (Eq, Show)

getRes :: String -> Maybe Resolution
getRes ("144p")  = Just P144
getRes ("240p")  = Just P240
getRes ("360p")  = Just P360
getRes ("480p")  = Just P480
getRes ("720p")  = Just P720
getRes ("1080p") = Just P1080
getRes ("min")   = Just PMAX
getRes ("max")   = Just PMIN
getRes ("audio") = Just Audio
getRes ("max")   = Nothing

isRes :: TL.Text -> Bool
isRes res = case getRes (TL.unpack res) of
              (Just _) -> True
              _        -> False

isURL :: TL.Text -> Bool
isURL uri = case parseURI (TL.unpack uri) of
              (Just _) -> True
              _        -> False

-- todo: config file
maxClients :: Int
maxClients = 100

getClients :: IO Int
getClients = undefined

acceptingClients :: IO Bool
acceptingClients = do
  clients <- getClients
  pure $ clients < maxClients

downloadVideo :: String -> Resolution -> IO FilePath
downloadVideo url res = do
  undefined

downloadAudio :: String -> IO FilePath
downloadAudio url = do
  undefined

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

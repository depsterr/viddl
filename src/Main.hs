{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Database.Redis as R
import qualified Data.Text.Lazy as TL
import Web.Scotty
import System.Posix.Process (executeFile)

data Resolution
  = P144
  | P240
  | P360
  | P480
  | P720
  | P1080
  | PMAX
  | PMIN
  deriving (Eq, Show)

getRes :: String -> Maybe Resolution
getRes ("144p")  = P144
getRes ("240p")  = P240
getRes ("360p")  = P360
getRes ("480p")  = P480
getRes ("720p")  = P720
getRes ("1080p") = P1080
getRes ("min")   = PMAX
getRes ("max")   = PMIN

downloadVideo :: String -> Resolution -> IO Filepath
downloadVideo url res = do
  undefined

downloadAudio :: String -> IO Filepath
downloadAudio url = do
  undefined

getIndex :: IO String
getIndex = readFile "views/index.html"

app :: R.Connection -> ScottyM ()
app rConn = do
  get "/" $ do
    setHeader "Content-Type" "text/html;charset=utf-8"
    file "views/index.html"

  post "/" $ do
    setHeader "Content-Type" "text/html;charset=utf-8"
    file "views/loading.html"
    url <- param "url"
    res <- param "resolution"
    -- set redis stuff and id here
    redirect '/':id

  get "/:id" $ do
    -- grab id and process video if not already done
    id <- param "id"

main :: IO ()
main = do
  -- todo: parse connection config
  rConn <- R.connect R.defaultConnectInfo
  scotty 3000 (app rConn)

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Templates.Index
import Templates.Error
import YTDL
import Helpers

import Control.Monad.IO.Class
import qualified Data.Text.Lazy as TL
import Web.Scotty

safeDownloadAction :: ActionM ()
safeDownloadAction = downloadAction `rescue` (html . errorPage)

downloadAction :: ActionM ()
downloadAction = do
    url <- param "url"
    res <- param "resolution"
    if (isURL url) && (isRes res)
      then do
        let (Just res') = getRes res -- safe cause we checked with isRes
        ytdlRes <- liftIO $ ytdl (TL.unpack url) res'
        case ytdlRes of
          (Right filePath) -> do
            setHeader "content-type" "video/mp4"
            file filePath
          (Left err) -> html $ errorPage (TL.pack err)
      else
        html $ errorPage "Invalid input!"

-- todo ReaderT
app :: ScottyM ()
app = do
  get "/" $ html indexPage
  get "/video.mp4" safeDownloadAction
  get "/audio.mp3" safeDownloadAction

main :: IO ()
main = scotty 3000 app

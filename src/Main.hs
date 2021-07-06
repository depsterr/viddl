{-# LANGUAGE OverloadedStrings #-}

module Main where

import Templates.Index
import Templates.Error
import YTDL
import Helpers
import Config

import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class
import qualified Data.Text.Lazy as TL
import System.Environment
import Web.Scotty.Trans

type Scotty = ScottyT TL.Text (ReaderT ViddlConfig IO)
type Action = ActionT TL.Text (ReaderT ViddlConfig IO)

safeDownloadAction :: Action ()
safeDownloadAction = downloadAction `rescue` (html . errorPage)

downloadAction :: Action ()
downloadAction = do
    url <- param "url"
    res <- param "resolution"
    if (isURL url) && (isRes res)
      then do
        let (Just res') = getRes res -- safe cause we checked with isRes
        ytdlRes <- lift $ ytdl (TL.unpack url) res'
        case ytdlRes of
          (Right filePath) -> do
            setHeader "content-type" "video/mp4"
            file filePath
          (Left err) -> html $ errorPage (TL.pack err)
      else
        html $ errorPage "Invalid input!"

-- todo ReaderT
app :: Scotty ()
app = do
  get "/" $ html indexPage
  get "/video.mp4" safeDownloadAction
  get "/audio.mp3" safeDownloadAction

main :: IO ()
main = do
  args <- getArgs
  defCfg <- defConfig
  cfg <- parseArgs defCfg args
  scottyT (webPort cfg) (\(ReaderT ma) -> ma cfg) app

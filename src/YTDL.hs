module YTDL (Resolution(..), ytdl) where

import Config

import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class
import Control.Concurrent (forkIO, threadDelay)
import Data.Digest.Pure.MD5
import qualified Data.ByteString.Lazy.UTF8 as BCU
import System.Directory
import System.Exit
import System.Process

data Resolution
  = P144
  | P240
  | P360
  | P480
  | P720
  | P1080
  | PMAX
  | Audio
  deriving (Eq, Show)

wrapResString :: String -> [String]
wrapResString str = ["-f", concat ["bestvideo[ext=mp4,height<=", str,
  "]+bestaudio[ext=m4a]/mp4[height<=", str, "]"], "--merge-output-format", "mp4"]

resToArgs :: Resolution -> [String]
resToArgs (P144)  = wrapResString "144"
resToArgs (P240)  = wrapResString "240"
resToArgs (P360)  = wrapResString "360"
resToArgs (P480)  = wrapResString "480"
resToArgs (P720)  = wrapResString "720"
resToArgs (P1080) = wrapResString "1080"
resToArgs (PMAX)  = ["-f", "bestvideo[ext=mp4]+bestaudio[ext=m4a]/mp4", "--merge-output-format", "mp4"]
resToArgs (Audio) = ["-x", "--audio-format", "mp3"]

ytdl :: String -> Resolution -> ReaderT ViddlConfig IO (Either String FilePath)
ytdl url res = do
  cfg <- ask

  let ext = case res of { Audio -> ".mp3"; _ -> ".mp4" }

  let ident = show . md5 . BCU.fromString $ url <> show res

  let dir = concat [dlDir cfg, "/", ident]
  let fileName = concat [dir, "/", ident, ext]

  processed <- lift $ doesFileExist fileName
  if processed
    then do
      lift $ putStrLn $ "Returning existing ident " <> ident
      pure (Right fileName)
    else do
      lift $ putStrLn $ "Processing new ident " <> ident

      lift $ createDirectoryIfMissing True dir

      lift $ print (resToArgs res <> ["-o", fileName, url])

      ytdlProc <- lift $ createProcess (proc "youtube-dl" (resToArgs res <> ["-o", fileName, url]))

      case ytdlProc of
        (_, _, _, ph) -> do
          exitCode <- lift $ waitForProcess ph
          case exitCode of
            ExitSuccess -> do
              exists <- lift $ doesFileExist fileName
              if exists
                then do
                  -- wait 5 minutes and then delete the directory
                  _ <- lift $ forkIO $ threadDelay 300000000 >> removeDirectoryRecursive dir
                  pure (Right fileName)
                else do 
                  -- removeDirectoryRecursive dir
                  pure (Left "An unknown error prevented the output file from being created")

            (ExitFailure status) -> pure (Left ("execution failed with status " <> show status))

        _ -> pure (Left "Unable to create ytdlProcess for downloading video")

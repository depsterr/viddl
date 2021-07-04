module YTDL (Resolution(..), ytdl) where

import Control.Concurrent (forkIO, threadDelay)
import Data.Digest.Pure.MD5
import qualified Data.ByteString.Lazy.UTF8 as BCU
import System.Directory
import System.Exit
import System.Process
import System.IO

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
  "]+bestaudio[ext=m4a]/mp4[height<=", str, "]"]]

resToArgs :: Resolution -> [String]
resToArgs (P144)  = wrapResString "144"
resToArgs (P240)  = wrapResString "240"
resToArgs (P360)  = wrapResString "360"
resToArgs (P480)  = wrapResString "480"
resToArgs (P720)  = wrapResString "720"
resToArgs (P1080) = wrapResString "1080"
resToArgs (PMAX)  = ["-f", "bestvideo[ext=mp4]+bestaudio[ext=m4a]/mp4"]
resToArgs (Audio) = ["-x", "--audio-format", "mp3"]

ytdl :: String -> Resolution -> IO (Either String FilePath)
ytdl url res = do
  let ext = case res of { Audio -> ".mp3"; _ -> ".mp4" }

  let ident = show . md5 . BCU.fromString $ url <> show res

  -- todo: config for path
  tmpdir <- getTemporaryDirectory
  let dir = concat [tmpdir, "/viddl/", ident]
  let fileName = concat [dir, "/", ident, ext]

  -- todo: implement file locking for deleting and uploading file
  processed <- doesFileExist fileName
  if processed
    then do
      putStrLn $ "Returning existing ident " <> ident
      pure (Right fileName)
    else do
      putStrLn $ "Processing new ident " <> ident

      createDirectoryIfMissing True dir

      ytdlProc <- createProcess (proc "youtube-dl" (resToArgs res <> ["-o", fileName, url]))
        { std_out = CreatePipe
        , std_err = CreatePipe }

      case ytdlProc of
        (_, _, Just herr, ph) -> do
          err <- hGetContents herr
          exitCode <- waitForProcess ph
          case exitCode of
            ExitSuccess -> do
              exists <- doesFileExist fileName
              if exists
                then do
                  -- wait 5 minutes and then delete the directory
                  -- todo: implement file locking for deleting and uploading file
                  _ <- forkIO $ threadDelay 300000000 >> removeDirectoryRecursive dir
                  pure (Right fileName)
                else do 
                  removeDirectoryRecursive dir
                  pure (Left "An unknown error prevented the output file from being created")

            (ExitFailure status) -> pure (Left (concat ["execution failed with status ", show status, ": ", err]))

        _ -> pure (Left "Unable to create ytdlProcess for downloading video")

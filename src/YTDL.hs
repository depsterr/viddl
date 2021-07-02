module YTDL (downloadVideo, downloadAudio, Resolution(..), ytdl) where

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

downloadVideo :: String -> String -> Resolution -> IO (Either String FilePath)
downloadVideo client url res = ytdl client url res

downloadAudio :: String -> String -> IO (Either String FilePath)
downloadAudio client url = ytdl client url Audio

ytdl :: String -> String -> Resolution -> IO (Either String FilePath)
ytdl client url res = do
  let ext = case res of { Audio -> ".mp3"; _ -> ".mp4" }
  -- todo: config for path
  tmpdir <- getTemporaryDirectory
  let dir = concat [tmpdir, "/viddl/", client]
  let fileName = concat [dir, "/", client, ext]
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
            then pure (Right fileName)
            else do 
              removeDirectory dir
              pure (Left "An unknown error prevented the output file from being created")

        (ExitFailure status) -> pure (Left (concat ["execution failed with status ", show status, ": ", err]))

    _ -> pure (Left "Unable to create ytdlProcess for downloading video")

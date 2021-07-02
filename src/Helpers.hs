module Helpers where

import YTDL
import qualified Data.Text.Lazy as TL
import Network.URI (parseURI)

getRes :: String -> Maybe Resolution
getRes ("144p")  = Just P144
getRes ("240p")  = Just P240
getRes ("360p")  = Just P360
getRes ("480p")  = Just P480
getRes ("720p")  = Just P720
getRes ("1080p") = Just P1080
getRes ("max")   = Just PMAX
getRes ("audio") = Just Audio
getRes _         = Nothing

isRes :: TL.Text -> Bool
isRes res = case getRes (TL.unpack res) of
              (Just _) -> True
              _        -> False

isURL :: TL.Text -> Bool
isURL uri = case parseURI (TL.unpack uri) of
              (Just _) -> True
              _        -> False

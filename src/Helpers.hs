{-# LANGUAGE OverloadedStrings #-}

module Helpers where

import YTDL
import Config

import Text.Read (readMaybe)
import qualified Data.Text.Lazy as TL
import Network.URI
import System.Exit

parseArgs :: ViddlConfig -> [String] -> IO ViddlConfig
parseArgs _ ("-h":_) = exitHelp
parseArgs _ ("--help":_) = exitHelp
parseArgs cfg@(ViddlConfig _ d) ("-p":p':xs) = case (readMaybe p' :: Maybe Int) of
  (Just p) -> parseArgs (ViddlConfig p d) xs
  Nothing  -> parseArgs cfg (p':xs)
parseArgs (ViddlConfig p _) ("-d":d:xs) = parseArgs (ViddlConfig p d) xs
parseArgs cfg (_:xs) = parseArgs cfg xs
parseArgs cfg [] = pure cfg


exitHelp :: IO a
exitHelp = putStrLn "viddl [-p port] [-d dir]" >> exitSuccess

getRes :: TL.Text -> Maybe Resolution
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
isRes res = case getRes res of
              (Just _) -> True
              _        -> False

-- ssrf paranoia
isOkPath :: String -> Bool
isOkPath p = not $ isIPv4address p || isIPv6address p || p == "localhost"

isURL :: TL.Text -> Bool
isURL uri = case parseURI (TL.unpack uri) of
              (Just u) -> case uriAuthority u of
                  (Just (URIAuth _ p _)) -> isOkPath p
                  _                        -> False
              _          -> False

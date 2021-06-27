{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Templates.Error (errorPage) where

import qualified Data.Text.Lazy as TL
import Text.RawString.QQ

errorPage :: TL.Text -> TL.Text
errorPage msg = [r|
<!DOCTYPE html>
<head>
	<meta cherset="UTF-8">
	<meta name="viewport" content="width=device-width, initial-scale=1.0">
	<title>viddl</title>
</head>
<body>
	<center>
		<h1>viddl</h1>
		<p>|] <> msg <> [r|</p>
		<hr>
		<p>viddl is free <a href="https://github.com/depsterr/viddl">open source</a> software and is powered by <a href="https://yt-dl.org/">youtube-dl</a>.</p>
	</center>
</body>
|]

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Templates.Loading (loadingPage) where

import qualified Data.Text.Lazy as TL
import Text.RawString.QQ

loadingPage :: TL.Text
loadingPage = [r|
<!DOCTYPE html>
<head>
	<meta cherset="UTF-8">
	<meta name="viewport" content="width=device-width, initial-scale=1.0">
	<title>viddl</title>
</head>
<body>
	<center>
		<h1>viddl</h1>
		<p>Your request is being processed... Please wait...</p>
		<hr>
		<p>viddl is free <a href="https://github.com/depsterr/viddl">open source</a> software and is powered by <a href="https://yt-dl.org/">youtube-dl</a>.</p>
	</center>
</body>
|]

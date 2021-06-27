{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Templates.Index (indexPage) where

import qualified Data.Text.Lazy as TL
import Text.RawString.QQ

indexPage :: TL.Text
indexPage = [r|
<!DOCTYPE html>
<head>
	<meta cherset="UTF-8">
	<meta name="viewport" content="width=device-width, initial-scale=1.0">
	<title>viddl</title>
</head>
<body>
	<center>
		<h1>viddl</h1>
		<p>Download videos from sources such as youtube, twitter, vimeo <a href="https://github.com/ytdl-org/youtube-dl/blob/master/docs/supportedsites.md">and more!</a></p>
		<table>
			<tr>
				<td valign="top">
					<h2>Video download</h2>
					<form method="post" action="/">
						<input required name="url" type="text" placeholder="Enter url here"><br>
						<label for="resolution">res</label>
						<select required name="resolution">
							<option value="min">Smallest Possible</option>
							<option value="144p">144p</option>
							<option value="240p">240p</option>
							<option value="360p">360p</option>
							<option value="480p">480p</option>
							<option selected value="720p">720p</option>
							<option value="1080p">1080p</option>
							<option value="max">Largest Possible</option>
						</select><br>
						<input type="submit" value="Download">
					</form>
				</td>
				<td valign="top">
					<h2>Audio only download</h2>
					<form method="post" action="/">
						<input required name="url" type="text" placeholder="Enter url here"><br>
						<input type="hidden" name="resolution" value="audio">
						<input type="submit" value="Download">
					</form>
				</td>
			</tr>
		</table>
		<hr>
		<p>viddl is free <a href="https://github.com/depsterr/viddl">open source</a> software and is powered by <a href="https://yt-dl.org/">youtube-dl</a>.</p>
	</center>
</body>
|]

module Config where

import System.Directory

data ViddlConfig = ViddlConfig
  { webPort :: Int
  , dlDir   :: FilePath }

defConfig :: IO ViddlConfig
defConfig = getTemporaryDirectory >>= pure . ViddlConfig 3000 . (<> "/viddl")

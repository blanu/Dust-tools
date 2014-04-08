module Dust.Services.File.Progress
(
  progressBar
)
where

import Control.Monad
import Control.Concurrent
import System.IO
import Text.Printf

putProgress :: String -> IO ()
putProgress s = hPutStr stderr $ "\r\ESC[K" ++ s

drawProgressBar :: Int -> Float -> String
drawProgressBar width progress =
  "[" ++ replicate bars '=' ++ replicate spaces ' ' ++ "]"
  where bars = round (progress * fromIntegral width)
        spaces = width - bars

drawPercentage :: Float -> String
drawPercentage progress = printf "%3d%%" (truncate (progress * 100) :: Int)

progressBar :: Int -> Int -> IO()
progressBar maxProgress progress = do
  let fp = fromIntegral progress :: Float
  let fm = fromIntegral maxProgress :: Float
  let percent = (fp / fm) :: Float
  putProgress $ drawProgressBar 40 percent ++ " " ++ drawPercentage percent
--  hPutChar stderr '\n'

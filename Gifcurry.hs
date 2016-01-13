-- David Lettier (C) 2016. http://www.lettier.com/

module Gifcurry (gif) where

import System.Environment
import System.Process
import System.IO.Temp
import System.Directory
import System.Exit
import Data.List
import Control.Exception

gif [is, os, st, dr, wd, qa, ttx, btx] =
  withTempDirectory "." "frames" $ \tmpDir -> do
    putStrLn $ "\nInput file: " ++ is
    putStrLn $ "Start second: " ++ st
    putStrLn $ "Duration: " ++ dr ++ " seconds"
    putStrLn $ "GIF width: " ++ wd ++ "px"
    putStrLn $ "Quality: " ++ (quality qa) ++ "%"
    putStrLn $ "Top text: " ++ ttx
    putStrLn $ "Bottom text: " ++ btx
    putStrLn $ "\nWriting temporary frames to... " ++ tmpDir
    fileExists <- doesFileExist is
    if fileExists
      then do
        result <- try (readProcess "ffmpeg" [
          "-nostats",
          "-loglevel",
          "panic",
          "-an",
          "-ss",
          st,
          "-i",
          is,
          "-t",
          dr,
          "-r",
          "15",
          "-q:v",
          "2",
          "-vf",
          "scale=" ++ wd ++ ":-1",
          "-f",
          "image2",
          tmpDir ++ "/%010d.png" ] "") :: IO (Either IOError String)
        result <- case result of
          Left ex -> return 1
          Right val -> return 0
        putStrLn $ "Writing your GIF to... " ++ os
        result <- try (convert qa tmpDir os wd ttx btx) :: IO (Either IOError String)
        result <- case result of
          Left ex -> return 1
          Right val -> return 0
        putStrLn "Done."
        return result
      else do
        putStrLn "Video file does not exist."
        return 0

convert qa dr os wd ttx btx = readProcess "convert" ([
  "-quiet",
  "-delay",
  "6",
  "-colors",
  ncolors qa,
  "-coalesce",
  "-layers",
  "OptimizeTransparency",
  "-layers",
  "RemoveDups",
  dr ++ "/*.png",
  "-dither",
  "FloydSteinberg",
  "-loop",
  "0" ] ++ annotate wd ttx "north" ++ annotate wd btx "south" ++ [os]) ""

quality qa
  | n > 100   = show 100
  | n < 0     = show 2
  | otherwise = show n
  where n = read qa :: Float

ncolors qa
  | n < 0.0    = show 1
  | n >= 100.0 = show 256
  | otherwise  = show $ truncate (n / 100.0 * 256.0)
  where n = read (quality qa) :: Float

annotate wd tx dr = [
  "-gravity",
  dr,
  "-stroke",
  "#000C",
  "-strokewidth",
  "10",
  "-pointsize",
  pointsize wd tx,
  "-annotate",
  "+0+10",
  tx,
  "-stroke",
  "none",
  "-fill",
  "white",
  "-pointsize",
  pointsize wd tx,
  "-annotate",
  "+0+10",
  tx ]

pointsize wd tx
  | length tx == 0 = show 0
  | n <= 0         = show 0
  | otherwise      = show $ truncate ((n * 0.4) / l * (72.0 / 34.0))
  where n = read wd :: Float
        l = fromIntegral (length tx)

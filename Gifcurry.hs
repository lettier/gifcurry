-- David Lettier (C) 2016. http://www.lettier.com/

-- | Produces GIFs using FFmpeg and ImageMagick.
-- The main function is 'gif'.
module Gifcurry (
      gif
    , GifParams(..)
    , defaultGifParams
    , gifParamsValid
  ) where

import System.Environment
import System.Process
import System.IO.Temp
import System.Directory
import System.Exit
import Data.List
import Text.Printf
import Control.Exception
import Control.Monad

-- | The data type record required by 'gif'.
data GifParams = GifParams {
      inputFile :: [Char]
    , outputFile :: [Char]
    , startTime :: Float
    , durationTime :: Float
    , widthSize :: Int
    , qualityPercent :: Float
    , topText :: [Char]
    , bottomText :: [Char]
  } deriving (Show, Read)

-- | Specifies default parameters for 'startTime', 'durationTime', 'widthSize', and 'qualityPercent'.
defaultGifParams = GifParams {
      inputFile = ""
    , outputFile = ""
    , startTime = 0.0
    , durationTime = 1.0
    , widthSize = 500
    , qualityPercent = 100.0
    , topText = ""
    , bottomText = ""
  }

-- | Inputs 'GifParams' and outputs either an IO IOError or IO String.
--
-- @
--    import qualified Gifcurry (gif, GifParams(..), defaultGifParams, gifParamsValid)
--    main :: IO ()
--    main = do
--      let params = Gifcurry.defaultGifParams { Gifcurry.inputFile = ".\/in.mov", Gifcurry.outputFile = ".\/out.gif" }
--      valid <- Gifcurry.gifParamsValid params
--      if valid
--        then do
--          result <- Gifcurry.gif params
--          print result
--        else return ()
-- @
gif :: GifParams -> IO (Either IOError String)
gif gifParams =
  withTempDirectory "." "frames" $ \tmpdir -> do
    printGifParams gifParams tmpdir
    validParams <- gifParamsValid gifParams
    if validParams
      then do
        result <- tryFfmpeg gifParams tmpdir
        result' <- case result of
          Left  err -> return False
          Right val -> return True
        if result'
          then do
            putStrLn $ "Writing your GIF to... " ++ outputFile gifParams
            result <- tryConvert gifParams tmpdir
            result' <- case result of
               Left  err -> return False
               Right val -> return True
            if result'
              then putStrLn "Done."
              else putStrLn "[Error] Something when wrong with ImageMagick."
            return result
          else do
            putStrLn "[Error] Something went wrong with FFmpeg."
            return result
      else return $ Left (userError "[Error] Invalid params.")

-- | Outputs True or False if a GifParams record parameters are valid.
-- Looks at 'inputFile', 'outputFile', 'startTime', 'durationTime', 'widthSize', and 'qualityPercent'.
gifParamsValid :: GifParams -> IO Bool
gifParamsValid GifParams {
      inputFile = ipf
    , outputFile = opf
    , startTime = st
    , durationTime = dt
    , widthSize = ws
    , qualityPercent = qp
    , topText = tt
    , bottomText = bt
  } = do
    inputFileExists <- case length ipf of
      0 -> return False
      _ -> doesFileExist ipf
    unless inputFileExists $ putStrLn "\n[Error] Input video file does not exist."
    let outputFileValid = length opf > 5
    unless outputFileValid $ putStrLn "\n[Error] Output video file blank."
    let valid = inputFileExists && outputFileValid && (st >= 0.0) && (dt >= 0.0) && (ws > 0) && (qp > 0.0)
    unless valid $ putStrLn "\n[Error] Invalid params."
    return valid

printGifParams :: GifParams -> [Char] -> IO ()
printGifParams
  GifParams {
      inputFile = ipf
    , outputFile = opf
    , startTime = st
    , durationTime = dt
    , widthSize = ws
    , qualityPercent = qp
    , topText = tt
    , bottomText = bt
  }
  tmpdir = mapM_ putStrLn [
        "\nInput file: " ++ ipf
      , "Output file: " ++ opf
      , "Start second: " ++ printf "%.3f" st
      , "Duration: " ++ printf "%.3f" dt ++ " seconds"
      , "GIF width: " ++ show ws ++ "px"
      , "Quality: " ++ show (qualityPercentClamp qp) ++ "%"
      , "Top text: " ++ tt
      , "Bottom text: " ++ bt
      , "\nWriting temporary frames to... " ++ tmpdir
    ]

tryFfmpeg :: GifParams -> [Char] -> IO (Either IOError String)
tryFfmpeg
  GifParams {
      inputFile = ipf
    , startTime = st
    , durationTime = dt
    , widthSize = ws
  }
  tmpdir = try(
    readProcess "ffmpeg" [
        "-nostats"
      , "-loglevel"
      , "panic"
      , "-an"
      , "-ss"
      , sts
      , "-i"
      , ipf
      , "-t"
      , dts
      , "-r"
      , "15"
      , "-q:v"
      , "2"
      , "-vf"
      , "scale=" ++ wss ++ ":-1"
      , "-f"
      , "image2"
      , tmpdir ++ "/%010d.png"
    ] ""
  ) :: IO (Either IOError String)
  where sts = printf "%.3f" st
        dts = printf "%.3f" dt
        wss = show ws

tryConvert :: GifParams -> [Char] -> IO (Either IOError String)
tryConvert
  GifParams {
      outputFile = opf
    , widthSize = ws
    , qualityPercent = qp
    , topText = tt
    , bottomText = bt
  }
  tmpdir = try(
    readProcess "convert" (
      [
          "-quiet"
        , "-delay"
        , "6"
        , "-colors"
        , show $ ncolors qp
        , "-coalesce"
        , "-layers"
        , "OptimizeTransparency"
        , "-layers"
        , "RemoveDups"
        , tmpdir ++ "/*.png"
        , "-dither"
        , "FloydSteinberg"
        , "-loop"
        , "0"
      ] ++ annotate ws tt "north" ++ annotate ws bt "south" ++ [opf]
    ) ""
  )

qualityPercentClamp :: Float -> Float
qualityPercentClamp qp
  | qp > 100.0   = 100.0
  | qp < 0.0     = 2.0
  | otherwise    = qp

ncolors :: Float -> Int
ncolors qp
  | qpc < 0.0    = 1
  | qpc >= 100.0 = 256
  | otherwise  = truncate (qpc / 100.0 * 256.0)
  where qpc = qualityPercentClamp qp

annotate :: Int -> [Char] -> [Char] -> [[Char]]
annotate widthSize text topBottom = [
      "-gravity"
    , topBottom
    , "-stroke"
    , "#000C"
    , "-strokewidth"
    , "10"
    , "-pointsize"
    , ps
    , "-annotate"
    , "+0+10"
    , text
    , "-stroke"
    , "none"
    , "-fill"
    , "white"
    , "-pointsize"
    , ps
    , "-annotate"
    , "+0+10"
    , text
  ]
  where ps = show $ pointSize widthSize text

pointSize :: Int -> [Char] -> Int
pointsize _ "" = 0
pointSize widthSize text
  | widthSize <= 0  = 0
  | otherwise       = truncate ((wsf * 0.4) / l * (72.0 / 34.0))
  where wsf = fromIntegral widthSize
        l   = fromIntegral (length text)

-- David Lettier (C) 2016. http://www.lettier.com/

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NamedFieldPuns #-}

import System.Environment
import System.Process
import System.IO.Temp
import System.Exit
import System.Console.CmdArgs
import Control.Monad
import Data.List

import qualified Gifcurry (gif, GifParams(..), defaultGifParams, gifParamsValid)

data CliArgs = CliArgs {
      inputFile :: [Char]
    , outputFile :: [Char]
    , startTime :: Float
    , durationTime :: Float
    , widthSize :: Int
    , qualityPercent :: Float
    , topText :: [Char]
    , bottomText :: [Char]
  } deriving (Data, Typeable, Show, Eq)

cliargs = CliArgs {
      inputFile = "" &= help "The input file path and name."
    , outputFile = "" &= help "The output file path and name."
    , startTime = 0.0 &= help "The start time in seconds for the first frame."
    , durationTime = 1.0 &= help "How long in seconds from the start time."
    , widthSize = 500 &= help "How wide the gif needs to be. Height will scale to match."
    , qualityPercent = 100.0 &= help "Ranges from 0.0 to 100.0."
    , topText = "" &= help "Any text you want to add to the top of the GIF."
    , bottomText = "" &= help "Any text you want to add to the bottom of the GIF."
  } &= summary "Gifcurry 2.0.0.0 (C) 2016 David Lettier"

main :: IO ()
main = do
  args <- cmdArgs cliargs
  let params = makeGifParams args
  printHeader
  paramsValid <- Gifcurry.gifParamsValid params
  if paramsValid
    then do
      Gifcurry.gif params
      return ()
    else printUsage
  return ()

makeGifParams :: CliArgs -> Gifcurry.GifParams
makeGifParams CliArgs {
      inputFile
    , outputFile
    , startTime
    , durationTime
    , widthSize
    , qualityPercent
    , topText
    , bottomText
  } = Gifcurry.GifParams {
      Gifcurry.inputFile      = inputFile
    , Gifcurry.outputFile     = outputFile
    , Gifcurry.startTime      = startTime
    , Gifcurry.durationTime   = durationTime
    , Gifcurry.widthSize      = widthSize
    , Gifcurry.qualityPercent = qualityPercent
    , Gifcurry.topText        = topText
    , Gifcurry.bottomText     = bottomText
  }

printUsage :: IO ()
printUsage = mapM_ putStrLn [
      "\nUsage: \n\t$ gifcurry_cli -i inputFile -o outputFile -s startTime " ++
      "-d durationTime -w widthSize -q qualityPercent -t topText -b bottomText"
  ]

printHeader :: IO ()
printHeader = mapM_ putStrLn [
      " _____ _  __                           "
    , "|  __ (_)/ _|                          "
    , "| |  \\/_| |_ ___ _   _ _ __ _ __ _   _ "
    , "| | __| |  _/ __| | | | '__| '__| | | |"
    , "| |_\\ \\ | || (__| |_| | |  | |  | |_| |"
    , " \\____/_|_| \\___|\\__,_|_|  |_|   \\__, |"
    , "                                  __/ |"
    , "                                 |___/ "
    , "\nGifcurry (C) 2016 David Lettier. http://www.lettier.com/"
  ]

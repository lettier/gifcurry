-- David Lettier (C) 2016. http://www.lettier.com/

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NamedFieldPuns #-}

import System.Console.CmdArgs

import qualified Gifcurry

data CliArgs = CliArgs {
      inputFile :: String
    , outputFile :: String
    , startTime :: Float
    , durationTime :: Float
    , widthSize :: Int
    , qualityPercent :: Float
    , fontChoice :: String
    , topText :: String
    , bottomText :: String
  } deriving (Data, Typeable, Show, Eq)

cliArgs :: CliArgs
cliArgs = CliArgs {
      inputFile      = ""        &= typFile &= help "The input video file path and name."
    , outputFile     = ""        &= typFile &= help "The output GIF file path and name."
    , startTime      = 0.0                  &= help "The start time (in seconds) for the first frame."
    , durationTime   = 1.0                  &= help "How long the GIF lasts (in seconds) from the start time."
    , widthSize      = 500                  &= help "How wide the GIF needs to be. Height will scale to match."
    , qualityPercent = 100.0                &= help "Ranges from 0.0 to 100.0."
    , fontChoice     = "default"            &= help "Choose your desired font for the top and bottom text."
    , topText        = ""                   &= help "The text you wish to add to the top of the GIF."
    , bottomText     = ""                   &= help "The text you wish to add to the bottom of the GIF."
  -- VERSION
  } &= summary "Gifcurry 2.1.0.0 (C) 2016 David Lettier"
    &= program "gifcurry_cli"

main :: IO ()
main = do
  cliArgs' <- cmdArgs cliArgs
  let params = makeGifParams cliArgs'
  printHeader
  paramsValid <- Gifcurry.gifParamsValid params
  if paramsValid
    then do
      _ <- Gifcurry.gif params
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
    , fontChoice
    , topText
    , bottomText
  } = Gifcurry.GifParams {
      Gifcurry.inputFile      = inputFile
    , Gifcurry.outputFile     = outputFile
    , Gifcurry.startTime      = startTime
    , Gifcurry.durationTime   = durationTime
    , Gifcurry.widthSize      = widthSize
    , Gifcurry.qualityPercent = qualityPercent
    , Gifcurry.fontChoice     = fontChoice
    , Gifcurry.topText        = topText
    , Gifcurry.bottomText     = bottomText
  }

printUsage :: IO ()
printUsage = putStrLn $ unwords [
        "\n"
      , "Usage:"
      , "\n\t"
      , "$ gifcurry_cli \\"
      , "\n\t"
      , "-i inputFile \\"
      , "\n\t"
      , "-o outputFile \\"
      , "\n\t"
      , "-s startTime \\"
      , "\n\t"
      , "-d durationTime \\"
      , "\n\t"
      , "-w widthSize \\"
      , "\n\t"
      , "-q qualityPercent \\"
      , "\n\t"
      , "-f fontChoice \\"
      , "\n\t"
      , "-t topText \\"
      , "\n\t"
      , "-b bottomText"
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
    , "\nGifcurry (C) 2016 David Lettier. https://www.lettier.com/"
  ]

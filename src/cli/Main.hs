{-
  Gifcurry
  (C) 2016 David Lettier
  lettier.com
-}

{-# LANGUAGE
    DeriveDataTypeable
  , OverloadedStrings
  , NamedFieldPuns
#-}
{-# OPTIONS_GHC -fno-cse #-}

import System.Console.CmdArgs
import Control.Monad

import qualified Gifcurry

data CliArgs =
  CliArgs
    { input_file         :: String
    , output_file        :: String
    , save_as_video      :: Bool
    , start_time         :: Double
    , end_time           :: Double
    , width              :: Int
    , fps                :: Int
    , color_count        :: Int
    , dither             :: Bool
    , left_crop          :: Double
    , right_crop         :: Double
    , top_crop           :: Double
    , bottom_crop        :: Double
    , text_file          :: String
    }
  deriving (Data, Typeable, Show, Eq)

programName :: String
programName = "gifcurry_cli"

info :: String -> String
info art =
  unlines
    [ art
    , "Gifcurry" ++ " " ++ Gifcurry.versionNumber
    , "(C) 2016 David Lettier"
    ,"https://lettier.com"
    , ""
    , "Wanna help out Gifcurry? Star it on GitHub! \x263A  Thanks for helping out\x2014you rock!"
    , "https://github.com/lettier/gifcurry/stargazers"
    ]

cliArgs :: CliArgs
cliArgs =
  CliArgs
    { input_file
        = ""
        &= groupname "FILE IO"
        &= typFile
        &= help "The input video file path."
    , output_file
        = ""
        &= groupname "FILE IO"
        &= typFile
        &= help "The output GIF file path."
    , save_as_video
        = False
        &= groupname "FILE IO"
        &= name "m"
        &= help "If present, saves the GIF as a video."
    , start_time
        = 0.0
        &= groupname "TIME"
        &= name "s"
        &= help "The start time (in seconds) for the first frame."
    , end_time
        = 1.0
        &= groupname "TIME"
        &= help "The end time (in seconds) for the last frame."
    , width
        = 500
        &= groupname "OUTPUT FILE SIZE"
        &= help "How wide the output needs to be. Height will scale to match."
    , fps
        = 24
        &= groupname "OUTPUT FILE SIZE"
        &= help "How many frames per second the output should have."
    , color_count
        = 256
        &= groupname "OUTPUT FILE SIZE"
        &= help "How many colors are used in the output."
    , dither
        = False
        &= groupname "OUTPUT FILE SIZE"
        &= help "If present, uses dither."
    , left_crop
        = 0.0
        &= groupname "CROP"
        &= name "L"
        &= help "The amount you wish to crop from the left."
    , right_crop
        = 0.0
        &= groupname "CROP"
        &= name "R"
        &= help "The amount you wish to crop from the right."
    , top_crop
        = 0.0
        &= groupname "CROP"
        &= name "T"
        &= help "The amount you wish to crop from the top."
    , bottom_crop
        = 0.0
        &= groupname "CROP"
        &= name "B"
        &= help "The amount you wish to crop from the bottom."
    , text_file
        = ""
        &= groupname "TEXT"
        &= typFile
        &= name "t"
        &= help "Either a text overlays YAML or SRT subtitles file path."
    }
    &= summary ""
    &= program programName
    &= details ["Visit https://github.com/lettier/gifcurry for more information.", ""]

main :: IO ()
main = do
  putStrLn $ info logo
  cliArgs'     <- cmdArgs cliArgs
  maybePlayableMetadata <- Gifcurry.getPlayableMetadata $ input_file cliArgs'
  let maybeWidthHeight =
        case maybePlayableMetadata of
          Nothing -> Nothing
          Just Gifcurry.PlayableMetadata
            { Gifcurry.playableMetadataWidth
            , Gifcurry.playableMetadataHeight
            } -> Just (playableMetadataWidth, playableMetadataHeight)
  textOverlays <- Gifcurry.convertFileToTextOverlays (text_file cliArgs') maybeWidthHeight
  let params   = (makeGifParams cliArgs') { Gifcurry.textOverlays = textOverlays }
  paramsValid  <- Gifcurry.validateGifParams params
  if paramsValid
    then void $ Gifcurry.createGif params
    else
      putStrLn $ "[INFO] Type \"" ++ programName ++ " -?\" for help."
  return ()

makeGifParams :: CliArgs -> Gifcurry.GifParams
makeGifParams
  CliArgs
    { input_file
    , output_file
    , save_as_video
    , start_time
    , end_time
    , width
    , fps
    , color_count
    , dither
    , left_crop
    , right_crop
    , top_crop
    , bottom_crop
    }
  =
  Gifcurry.GifParams
    { Gifcurry.inputFile      = input_file
    , Gifcurry.outputFile     = output_file
    , Gifcurry.saveAsVideo    = save_as_video
    , Gifcurry.startTime      = start_time
    , Gifcurry.endTime        = end_time
    , Gifcurry.width          = width
    , Gifcurry.fps            = fps
    , Gifcurry.colorCount     = color_count
    , Gifcurry.dither         = dither
    , Gifcurry.textOverlays   = []
    , Gifcurry.leftCrop       = left_crop
    , Gifcurry.rightCrop      = right_crop
    , Gifcurry.topCrop        = top_crop
    , Gifcurry.bottomCrop     = bottom_crop
    }

logo :: String
logo =
  unlines
    [ ""
    , "         ▄▄▄▄▄▄▄▄                                                                             " 
    , "     ▄▄████    ▀▀███▄                                                                         " 
    , "      ████▀   ▄    ▀███           ▄     ▐██▌   ▄███▄                                          " 
    , "  ▄   ▐███   ████   ▀███      ▄███▀▀██        ███                                             "
    , " ▐█▌   ██   ▐███     ████    ███        ▐██  █████▌ ▄█████ ▐██▌  ██▌  ██▄██▌ ██▄██▌ ██▌   ███ " 
    , " ███   ▐▌   ███      ▐███▌   ███  ████▌ ▐██   ██▌  ███     ▐██▌  ██▌  ███▀   ███▀   ▐██  ███  " 
    , " ████      ███▀  ▐█   ███▌   ███    ██▌ ▐██   ██▌  ███     ▐██▌  ██▌  ██▌    ██▌     ██▌▐██   " 
    , " ▐███▄    ▐██▌   ██    ██     ███▄▄▄██▌ ▐██   ██▌   ███▄▄█  ███▄███▌  ██▌    ██▌      ████▌   " 
    , "  ▀███   ▀███   ▐███   ▀        ▀▀▀▀▀    ▀▀   ▀▀      ▀▀▀     ▀▀▀     ▀▀     ▀▀        ███    " 
    , "    ███▄   ▀    ████▌                                                                ███▀     " 
    , "      ▀███▄▄   █████▀                                                                         " 
    , "          ▀▀▀▀▀▀▀                                                                             " 
    , ""
    ]

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

import System.Directory
import System.Console.CmdArgs
import Control.Monad
import Data.Text (pack, unpack, strip)
import Data.Maybe
import Data.Yaml
import qualified Data.ByteString.Char8 as DBC

import qualified Gifcurry

data CliArgs =
  CliArgs
    { input_file         :: String
    , output_file        :: String
    , save_as_video      :: Bool
    , start_time         :: Float
    , duration_time      :: Float
    , width_size         :: Int
    , quality            :: String
    , left_crop          :: Float
    , right_crop         :: Float
    , top_crop           :: Float
    , bottom_crop        :: Float
    , text_overlays_file :: String
    }
  deriving (Data, Typeable, Show, Eq)

data TextOverlay =
  TextOverlay
    { text         :: String
    , fontFamily   :: String
    , fontStyle    :: String
    , fontStretch  :: String
    , fontWeight   :: Int
    , fontSize     :: Int
    , origin       :: String
    , xTranslation :: Float
    , yTranslation :: Float
    , rotation     :: Int
    , startTime    :: Float
    , durationTime :: Float
    , outlineSize  :: Int
    , outlineColor :: String
    , fillColor    :: String
    }
  deriving (Show)

instance FromJSON TextOverlay where
  parseJSON =
    withObject
      "TextOverlay"
      (\ obj ->
        TextOverlay
          <$> obj .:  "text"
          <*> obj .:? "fontFamily"   .!= "Sans"
          <*> obj .:? "fontStyle"    .!= "Normal"
          <*> obj .:? "fontStretch"  .!= "Normal"
          <*> obj .:? "fontWeight"   .!= 400
          <*> obj .:? "fontSize"     .!= 30
          <*> obj .:? "origin"       .!= "Center"
          <*> obj .:? "xTranslation" .!= 0.0
          <*> obj .:? "yTranslation" .!= 0.0
          <*> obj .:? "rotation"     .!= 0
          <*> obj .:  "startTime"
          <*> obj .:  "durationTime"
          <*> obj .:? "outlineSize"  .!= 10
          <*> obj .:? "outlineColor" .!= "rgba(0,0,0)"
          <*> obj .:? "fillColor"    .!= "rgba(255,255,255)"
      )

programName :: String
programName = "gifcurry_cli"

info :: String -> String
info art =
  unlines
    [ art
    , "Gifcurry" ++ " " ++ Gifcurry.versionNumber
    , "(C) 2016 David Lettier"
    ,"lettier.com"
    ]

cliArgs :: CliArgs
cliArgs =
  CliArgs
    { input_file
        = ""
        &= groupname "FILE IO"
        &= typFile
        &= help "The input video file path and name."
    , output_file
        = ""
        &= groupname "FILE IO"
        &= typFile
        &= help "The output GIF file path and name."
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
    , duration_time
        = 1.0
        &= groupname "TIME"
        &= help "How long the GIF lasts (in seconds) from the start time."
    , width_size
        = 500
        &= groupname "OUTPUT FILE SIZE"
        &= help "How wide the GIF needs to be. Height will scale to match."
    , quality
        = "medium"
        &= groupname "OUTPUT FILE SIZE"
        &= help
          (  "Controls how many colors are used and the frame rate. \n"
          ++ "The options are High, Medium, and Low."
          )
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
    , text_overlays_file
        = ""
        &= groupname "TEXT"
        &= typFile
        &= name "t"
        &= help
          (unlines
            [ "The text overlays YAML file path and name."
            , "\n"
            , "The format is:"
            , "\n"
            , "- text:         ..."
            , "\n"
            , "  fontFamily:   ..."
            , "\n"
            , "  fontStyle:    ..."
            , "\n"
            , "  fontStretch:  ..."
            , "\n"
            , "  fontWeight:   ..."
            , "\n"
            , "  fontSize:     ..."
            , "\n"
            , "  origin:       ..."
            , "\n"
            , "  xTranslation: ..."
            , "\n"
            , "  yTranslation: ..."
            , "\n"
            , "  rotation:     ..."
            , "\n"
            , "  startTime:    ..."
            , "\n"
            , "  durationTime: ..."
            , "\n"
            , "  outlineSize:  ..."
            , "\n"
            , "  outlineColor: ..."
            , "\n"
            , "  fillColor:    ..."
            , "\n"
            , "- text:         ..."
            , "\n"
            , "..."
            , " \n"
            , " \n"
            ]
          )
    }
    &= summary ""
    &= program programName
    &= details ["Visit https://github.com/lettier/gifcurry for more information.", ""]

main :: IO ()
main = do
  putStrLn $ info logo
  cliArgs'                  <- cmdArgs cliArgs
  let text_overlays_file'   = unpack $ strip $ pack $ text_overlays_file cliArgs'
  textOverlays              <-
    if null text_overlays_file'
      then return []
      else do
        text_overlays_file_exists <- doesFileExist text_overlays_file'
        text_overlays_data        <-
          if text_overlays_file_exists
            then DBC.readFile text_overlays_file'
            else return ""
        let maybeTextOverlays     = Data.Yaml.decode text_overlays_data :: Maybe [TextOverlay]
        makeTextOverlays text_overlays_file' maybeTextOverlays
  let params                = (makeGifParams cliArgs') { Gifcurry.textOverlays = textOverlays }
  paramsValid               <- Gifcurry.gifParamsValid params
  if paramsValid
    then void $ Gifcurry.gif params
    else
      putStrLn $
        "[INFO] Type \"" ++ programName ++ " -?\" for help."
  return ()

makeTextOverlays :: String -> Maybe [TextOverlay] -> IO [Gifcurry.TextOverlay]
makeTextOverlays text_overlays_file' maybeTextOverlays =
  case maybeTextOverlays of
    Nothing -> do
      putStrLn $
        "[WARNING] Could not parse the " ++ text_overlays_file' ++ " YAML file!"
      return []
    Just textOverlays ->
      mapM
      (\
        TextOverlay
          { text
          , fontFamily
          , fontStyle
          , fontStretch
          , fontWeight
          , fontSize
          , origin
          , xTranslation
          , yTranslation
          , rotation
          , startTime
          , durationTime
          , outlineSize
          , outlineColor
          , fillColor
          }
        -> do
        origin' <- originFromString origin
        return
          Gifcurry.TextOverlay
            { Gifcurry.textOverlayText         = text
            , Gifcurry.textOverlayFontFamily   = fontFamily
            , Gifcurry.textOverlayFontStyle    = fontStyle
            , Gifcurry.textOverlayFontStretch  = fontStretch
            , Gifcurry.textOverlayFontWeight   = fontWeight
            , Gifcurry.textOverlayFontSize     = fontSize
            , Gifcurry.textOverlayOrigin       = origin'
            , Gifcurry.textOverlayXTranslation = xTranslation
            , Gifcurry.textOverlayYTranslation = yTranslation
            , Gifcurry.textOverlayRotation     = rotation
            , Gifcurry.textOverlayStartTime    = startTime
            , Gifcurry.textOverlayDurationTime = durationTime
            , Gifcurry.textOverlayOutlineSize  = outlineSize
            , Gifcurry.textOverlayOutlineColor = outlineColor
            , Gifcurry.textOverlayFillColor    = fillColor
            }
      )
      textOverlays
  where
    originFromString :: String -> IO Gifcurry.TextOverlayOrigin
    originFromString origin' = do
      let maybeOrigin = Gifcurry.textOverlayOriginFromString origin'
      case maybeOrigin of
        Nothing -> do
          putStrLn $
            "[WARNING] Origin " ++ origin' ++ " not valid! Defaulting to Center."
          return Gifcurry.TextOverlayOriginCenter
        Just origin'' -> return origin''

makeGifParams :: CliArgs -> Gifcurry.GifParams
makeGifParams
  CliArgs
    { input_file
    , output_file
    , save_as_video
    , start_time
    , duration_time
    , width_size
    , quality
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
    , Gifcurry.durationTime   = duration_time
    , Gifcurry.widthSize      = width_size
    , Gifcurry.quality        = fromMaybe Gifcurry.QualityMedium $
                                  Gifcurry.qualityFromString quality
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

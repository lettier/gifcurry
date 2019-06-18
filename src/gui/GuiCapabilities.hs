{-
  Gifcurry
  (C) 2018 David Lettier
  lettier.com
-}

{-# LANGUAGE
    OverloadedStrings
#-}

module GuiCapabilities where

import Text.Read
import Text.ParserCombinators.ReadP
import Data.Text
import qualified GI.Gtk

import qualified Gifcurry (parseVersionNumber)
import Paths_Gifcurry
import qualified GuiRecords as GR
import GuiStyle
import GuiMisc

checkCapabilitiesAndNotify :: GR.GuiComponents -> IO ()
checkCapabilitiesAndNotify
  GR.GuiComponents
    { GR.inFileChooserButtonImage = inFileChooserButtonImage
    , GR.inFileChooserDialogLabel = inFileChooserDialogLabel
    }
  = do
    ffmpegEncoders                  <- getFfmpegEncoders
    ffmpegDecoders                  <- getFfmpegDecoders
    gstInspectOutput                <- getGtkInspectOutput
    (hasFfmpeg,   ffmpegVersion)    <- hasFfmpegWithVersion
    (hasFfprobe,  _ffprobeVersion)  <- hasFfprobeWithVersion
    (hasConvert,  _convertVersion)  <- hasConvertWithVersion
    (hasIdentify, _identifyVersion) <- hasIdentifyWithVersion
    let hasValidFfmpegVersion'      = hasValidFfmpegVersion ffmpegVersion
    let hasFfmpegVp9Encoder'        = hasFfmpegVp9Encoder   ffmpegEncoders
    let hasFfmpegDecoders'          = hasFfmpegDecoders     ffmpegDecoders
    let hasGtkSink'                 = hasGtkSink      gstInspectOutput
    let hasGstLibav'                = hasGstLibav     gstInspectOutput
    let hasGstDecoders'             = hasGstDecoders  gstInspectOutput
    GI.Gtk.widgetShow inFileChooserDialogLabel
    case [ hasFfmpeg
         , hasFfprobe
         , hasConvert
         , hasIdentify
         , hasValidFfmpegVersion'
         , hasFfmpegVp9Encoder'
         , hasFfmpegDecoders'
         , hasGtkSink'
         , hasGstLibav'
         , hasGstDecoders'
         ] of
      (False:_) -> do
        setImageToFile "data/error-icon.svg"
        setLabelText "FFmpeg not found. Cannot create GIFs."
        setLabelStyle "gifcurry-label-error"
      (_:False:_) -> do
        setImageToFile "data/error-icon.svg"
        setLabelText "FFprobe not found. Cannot create GIFs."
        setLabelStyle "gifcurry-label-error"
      (_:_:False:_) -> do
        setImageToFile "data/error-icon.svg"
        setLabelText "ImageMagick convert not found. Cannot create GIFs."
        setLabelStyle "gifcurry-label-error"
      (_:_:_:False:_) -> do
        setImageToFile "data/error-icon.svg"
        setLabelText "ImageMagick identify not found. Cannot create GIFs."
        setLabelStyle "gifcurry-label-error"
      (_:_:_:_:False:_) -> do
        setImageToFile "data/error-icon.svg"
        setLabelText "FFmpeg version too old. Upgrade to at least version 2.8.15."
        setLabelStyle "gifcurry-label-error"
      (_:_:_:_:_:False:_) -> do
        setImageToFile "data/error-icon.svg"
        setLabelText "FFmpeg does not have the VP9 encoder. Cannot save the GIF as a video."
        setLabelStyle "gifcurry-label-error"
      (_:_:_:_:_:_:False:_) -> do
        setImageToFile "data/error-icon.svg"
        setLabelText "FFmpeg is missing decoders. Cannot make GIFs for some videos."
        setLabelStyle "gifcurry-label-warning"
      (_:_:_:_:_:_:_:False:_) -> do
        setImageToFile "data/error-icon.svg"
        setLabelText "\"gtksink\" not found. No video preview. Install all GStreamer 1.0 plugins."
        setLabelStyle "gifcurry-label-warning"
      (_:_:_:_:_:_:_:_:False:_) -> do
        setImageToFile "data/warning-icon.svg"
        setLabelText "\"gst-libav\" not found. Video preview may not work."
        setLabelStyle "gifcurry-label-warning"
      (_:_:_:_:_:_:_:_:_:False:_) -> do
        setImageToFile "data/warning-icon.svg"
        setLabelText "GStreamer is missing decoders. Video preview may not work."
        setLabelStyle "gifcurry-label-warning"
      _ -> do
        GI.Gtk.widgetHide inFileChooserDialogLabel
        setImageToFile "data/open-icon.svg"
        setLabelText ""
        setLabelStyle "gifcurry-label-ok"
  where
    setImageToFile :: String -> IO ()
    setImageToFile iconFilePathName = do
      filePathName <- getDataFileName iconFilePathName
      GI.Gtk.imageSetFromFile
        inFileChooserButtonImage
        (Just filePathName)
    setLabelText :: Text -> IO ()
    setLabelText =
      GI.Gtk.labelSetText
        inFileChooserDialogLabel
    setLabelStyle :: Text -> IO ()
    setLabelStyle = widgetAddStyleClass inFileChooserDialogLabel

hasFfmpegVp9Encoder :: String -> Bool
hasFfmpegVp9Encoder = hasText "libvpx-vp9"

hasValidFfmpegVersion :: Maybe (Int, Int, Int) -> Bool
hasValidFfmpegVersion Nothing = True
hasValidFfmpegVersion
  (Just (major, minor, patch))
  | major == 2 && minor == 8 && patch == 15 = True
  | major == 2 && minor == 8 && patch >  15 = True
  | major == 2 && minor >  8                = True
  | major >  2                              = True
  | otherwise                               = False

hasFfmpegDecoders :: String -> Bool
hasFfmpegDecoders haystack =
      hasText "h264"   haystack
  &&  hasText "vp9"    haystack
  &&  hasText "vp8"    haystack
  &&  hasText "theora" haystack

hasGtkSink :: String -> Bool
hasGtkSink = hasText "gtksink"

hasGstLibav :: String -> Bool
hasGstLibav = hasText "libav"

hasGstDecoders :: String -> Bool
hasGstDecoders haystack =
      hasText  "avdec_h264" haystack
  &&  (hasText "avdec_vp9"  haystack || hasText "vp9dec" haystack)
  &&  (hasText "avdec_vp8"  haystack || hasText "vp8dec" haystack)
  &&  hasText  "theoradec"  haystack

getFfmpegDecoders :: IO String
getFfmpegDecoders = getFfmpegInfo "-decoders"

getFfmpegEncoders :: IO String
getFfmpegEncoders = getFfmpegInfo "-encoders"

getFfmpegInfo :: String -> IO String
getFfmpegInfo arg = do
  (_, out, _) <- safeRunProcessGetOutput "ffmpeg" [arg]
  return out

getGtkInspectOutput :: IO String
getGtkInspectOutput = do
  (_, out, _) <- safeRunProcessGetOutput "gst-inspect-1.0" []
  return out

hasFfmpegWithVersion :: IO (Bool, Maybe (Int, Int, Int))
hasFfmpegWithVersion =
  hasProcessWithVersionNumber "ffmpeg" ["-version"]

hasFfprobeWithVersion :: IO (Bool, Maybe (Int, Int, Int))
hasFfprobeWithVersion =
  hasProcessWithVersionNumber "ffprobe" ["-version"]

hasConvertWithVersion :: IO (Bool, Maybe (Int, Int, Int))
hasConvertWithVersion =
  hasProcessWithVersionNumber "convert" ["-version"]

hasIdentifyWithVersion :: IO (Bool, Maybe (Int, Int, Int))
hasIdentifyWithVersion =
  hasProcessWithVersionNumber "identify" ["-version"]

hasProcessWithVersionNumber
  :: String
  -> [String]
  -> IO (Bool, Maybe (Int, Int, Int))
hasProcessWithVersionNumber
  process
  args
  = do
  (_, out, _) <- safeRunProcessGetOutput process args
  if not . Prelude.null $ out
    then do
      let result = readP_to_S Gifcurry.parseVersionNumber (stringToLower out)
      case result of
        [((x, y, z), _)] -> do
          let x' = readMaybe x :: Maybe Int
          let y' = readMaybe y :: Maybe Int
          let z' = readMaybe z :: Maybe Int
          case (x', y', z') of
            (Just major, Just minor, Just patch) -> return (True, Just (major, minor, patch))
            _                                    -> return (True, Nothing)
        _ -> return (True, Nothing)
    else return (False, Nothing)

{-
  Gifcurry
  (C) 2016 David Lettier
  lettier.com
-}

{-# LANGUAGE
    OverloadedStrings
  , NamedFieldPuns
  , DuplicateRecordFields
#-}

import System.Directory
import System.FilePath
import System.Process
import Text.Printf
import Data.Maybe
import Data.Either
import Data.Text
import Data.List
import Data.IORef
import Data.Word
import Data.Int
import qualified Data.Digest.Pure.MD5 as MD5
import qualified Data.ByteString.Lazy as DBL
import Control.Monad
import Control.Concurrent
import Control.Exception
import Data.GI.Base
import qualified GI.GLib
import qualified GI.GObject
import qualified GI.Gdk
import qualified GI.Gtk
import GI.Gst

import Paths_Gifcurry
import qualified Gifcurry
  ( GifParams(..)
  , PlayableMetadata(..)
  , gif
  , defaultGifParams
  , gifParamsValid
  , getPlayableMetadata
  , getOutputFileWithExtension
  , findOrCreateTemporaryDirectory
  )
import qualified GtkMainSyncAsync (gtkMainAsync)
import qualified GuiRecords as GR
import qualified GuiCapabilities
import qualified GuiStyle
import qualified GuiTextOverlays
import qualified GuiPreview
import GuiMisc

durationTimeWarningLevel :: Double
durationTimeWarningLevel = 10.0

main :: IO ()
main = do
  _ <- GI.Gst.init Nothing
  _ <- GI.Gtk.init Nothing

  builder <- buildBuilder

  window                            <- builderGetObject GI.Gtk.Window            builder "gifcurry-window"
  startTimeSpinButton               <- builderGetObject GI.Gtk.SpinButton        builder "start-time-spin-button"
  durationTimeSpinButton            <- builderGetObject GI.Gtk.SpinButton        builder "duration-time-spin-button"
  widthSpinButton                   <- builderGetObject GI.Gtk.SpinButton        builder "width-spin-button"
  fpsSpinButton                     <- builderGetObject GI.Gtk.SpinButton        builder "fps-spin-button"
  colorCountSpinButton              <- builderGetObject GI.Gtk.SpinButton        builder "color-count-spin-button"
  leftCropSpinButton                <- builderGetObject GI.Gtk.SpinButton        builder "left-crop-spin-button"
  rightCropSpinButton               <- builderGetObject GI.Gtk.SpinButton        builder "right-crop-spin-button"
  topCropSpinButton                 <- builderGetObject GI.Gtk.SpinButton        builder "top-crop-spin-button"
  bottomCropSpinButton              <- builderGetObject GI.Gtk.SpinButton        builder "bottom-crop-spin-button"
  inFileChooserButton               <- builderGetObject GI.Gtk.Button            builder "in-file-chooser-button"
  inFileChooserDialogCancelButton   <- builderGetObject GI.Gtk.Button            builder "in-file-chooser-dialog-cancel-button"
  inFileChooserDialogOpenButton     <- builderGetObject GI.Gtk.Button            builder "in-file-chooser-dialog-open-button"
  outFileChooserButton              <- builderGetObject GI.Gtk.FileChooserButton builder "out-file-chooser-button"
  textOverlaysAddButton             <- builderGetObject GI.Gtk.Button            builder "text-overlays-add-button"
  saveButton                        <- builderGetObject GI.Gtk.Button            builder "save-button"
  openButton                        <- builderGetObject GI.Gtk.Button            builder "open-button"
  confirmMessageDialogYesButton     <- builderGetObject GI.Gtk.Button            builder "confirm-message-dialog-yes-button"
  confirmMessageDialogNoButton      <- builderGetObject GI.Gtk.Button            builder "confirm-message-dialog-no-button"
  aboutButton                       <- builderGetObject GI.Gtk.Button            builder "about-button"
  aboutDialogCloseButton            <- builderGetObject GI.Gtk.Button            builder "about-dialog-close-button"
  giphyUploadButton                 <- builderGetObject GI.Gtk.Button            builder "giphy-upload-button"
  imgurUploadButton                 <- builderGetObject GI.Gtk.Button            builder "imgur-upload-button"
  saveAsVideoRadioButton            <- builderGetObject GI.Gtk.RadioButton       builder "save-as-video-radio-button"
  fileSizeToggleButton              <- builderGetObject GI.Gtk.ToggleButton      builder "file-size-toggle-button"
  cropToggleButton                  <- builderGetObject GI.Gtk.ToggleButton      builder "crop-toggle-button"
  textOverlaysToggleButton          <- builderGetObject GI.Gtk.ToggleButton      builder "text-overlays-toggle-button"
  saveOpenToggleButton              <- builderGetObject GI.Gtk.ToggleButton      builder "save-open-toggle-button"
  uploadToggleButton                <- builderGetObject GI.Gtk.ToggleButton      builder "upload-toggle-button"
  videoPreviewPauseToggleButton     <- builderGetObject GI.Gtk.ToggleButton      builder "video-preview-pause-toggle-button"
  inFileChooserDialogLabel          <- builderGetObject GI.Gtk.Label             builder "in-file-chooser-dialog-label"
  inFileChooserButtonLabel          <- builderGetObject GI.Gtk.Label             builder "in-file-chooser-button-label"
  startTimeAdjustment               <- builderGetObject GI.Gtk.Adjustment        builder "start-time-adjustment"
  durationTimeAdjustment            <- builderGetObject GI.Gtk.Adjustment        builder "duration-time-adjustment"
  widthAdjustment                   <- builderGetObject GI.Gtk.Adjustment        builder "width-adjustment"
  fpsAdjustment                     <- builderGetObject GI.Gtk.Adjustment        builder "fps-adjustment"
  colorCountAdjustment              <- builderGetObject GI.Gtk.Adjustment        builder "color-count-adjustment"
  outFileNameEntry                  <- builderGetObject GI.Gtk.Entry             builder "out-file-name-entry"
  aboutDialogLabel                  <- builderGetObject GI.Gtk.Label             builder "about-dialog-label"
  statusLabel                       <- builderGetObject GI.Gtk.Label             builder "status-label"
  sidebarControlsPreviewbox         <- builderGetObject GI.Gtk.Box               builder "sidebar-controls-preview-box"
  mainPreviewBox                    <- builderGetObject GI.Gtk.Box               builder "main-preview-box"
  imagesPreviewBox                  <- builderGetObject GI.Gtk.Box               builder "images-preview-box"
  videoPreviewBox                   <- builderGetObject GI.Gtk.Box               builder "video-preview-box"
  videoPreviewOverlayChildBox       <- builderGetObject GI.Gtk.Box               builder "video-preview-overlay-child-box"
  cropSpinButtonsBox                <- builderGetObject GI.Gtk.Box               builder "crop-spin-buttons-box"
  textOverlaysMainBox               <- builderGetObject GI.Gtk.Box               builder "text-overlays-main-box"
  textOverlaysBox                   <- builderGetObject GI.Gtk.Box               builder "text-overlays-box"
  saveOpenBox                       <- builderGetObject GI.Gtk.Box               builder "save-open-box"
  uploadBox                         <- builderGetObject GI.Gtk.Box               builder "upload-box"
  fileSizeSpinButtonsGrid           <- builderGetObject GI.Gtk.Grid              builder "file-size-spin-buttons-grid"
  videoPreviewDrawingArea           <- builderGetObject GI.Gtk.DrawingArea       builder "video-preview-drawing-area"
  timeSlicesDrawingArea             <- builderGetObject GI.Gtk.DrawingArea       builder "time-slices-drawing-area"
  firstFramePreviewImageDrawingArea <- builderGetObject GI.Gtk.DrawingArea       builder "first-frame-preview-image-drawing-area"
  lastFramePreviewImageDrawingArea  <- builderGetObject GI.Gtk.DrawingArea       builder "last-frame-preview-image-drawing-area"
  inFileChooserButtonImage          <- builderGetObject GI.Gtk.Image             builder "in-file-chooser-button-image"
  firstFrameImage                   <- builderGetObject GI.Gtk.Image             builder "first-frame-image"
  lastFrameImage                    <- builderGetObject GI.Gtk.Image             builder "last-frame-image"
  inFileChooserDialog               <- builderGetObject GI.Gtk.Dialog            builder "in-file-chooser-dialog"
  confirmMessageDialog              <- builderGetObject GI.Gtk.MessageDialog     builder "confirm-message-dialog"
  aboutDialog                       <- builderGetObject GI.Gtk.Dialog            builder "about-dialog"
  saveSpinner                       <- builderGetObject GI.Gtk.Spinner           builder "save-spinner"
  inFileChooserWidget               <- builderGetObject GI.Gtk.FileChooserWidget builder "in-file-chooser-widget"

  -- Glade does not allow us to use the response ID nicknames so we set them here.
  GI.Gtk.dialogAddActionWidget confirmMessageDialog confirmMessageDialogYesButton   $ enumToInt32 GI.Gtk.ResponseTypeYes
  GI.Gtk.dialogAddActionWidget confirmMessageDialog confirmMessageDialogNoButton    $ enumToInt32 GI.Gtk.ResponseTypeNo
  GI.Gtk.dialogAddActionWidget inFileChooserDialog  inFileChooserDialogCancelButton $ enumToInt32 GI.Gtk.ResponseTypeCancel
  GI.Gtk.dialogAddActionWidget inFileChooserDialog  inFileChooserDialogOpenButton   $ enumToInt32 GI.Gtk.ResponseTypeOk
  GI.Gtk.dialogAddActionWidget aboutDialog          aboutDialogCloseButton          $ enumToInt32 GI.Gtk.ResponseTypeOk

  (maybeVideoPreviewWidget, maybePlaybinElement) <-
    GuiPreview.buildVideoPreviewWidgetAndPlaybinElement

  temporaryDirectory <- Gifcurry.findOrCreateTemporaryDirectory

  guiInFilePropertiesRef <- newIORef GR.defaultGuiInFileProperties
  textOverlaysRef        <- newIORef []
  guiPreviewStateRef     <- newIORef GR.defaultGuiPreviewState

  let guiComponents =
        GR.GuiComponents
          { GR.window                            = window
          , GR.startTimeSpinButton               = startTimeSpinButton
          , GR.durationTimeSpinButton            = durationTimeSpinButton
          , GR.widthSpinButton                   = widthSpinButton
          , GR.fpsSpinButton                     = fpsSpinButton
          , GR.colorCountSpinButton              = colorCountSpinButton
          , GR.leftCropSpinButton                = leftCropSpinButton
          , GR.rightCropSpinButton               = rightCropSpinButton
          , GR.topCropSpinButton                 = topCropSpinButton
          , GR.bottomCropSpinButton              = bottomCropSpinButton
          , GR.inFileChooserButton               = inFileChooserButton
          , GR.inFileChooserDialogCancelButton   = inFileChooserDialogCancelButton
          , GR.inFileChooserDialogOpenButton     = inFileChooserDialogOpenButton
          , GR.outFileChooserButton              = outFileChooserButton
          , GR.textOverlaysAddButton             = textOverlaysAddButton
          , GR.saveButton                        = saveButton
          , GR.openButton                        = openButton
          , GR.confirmMessageDialogYesButton     = confirmMessageDialogYesButton
          , GR.confirmMessageDialogNoButton      = confirmMessageDialogNoButton
          , GR.aboutButton                       = aboutButton
          , GR.aboutDialogCloseButton            = aboutDialogCloseButton
          , GR.giphyUploadButton                 = giphyUploadButton
          , GR.imgurUploadButton                 = imgurUploadButton
          , GR.saveAsVideoRadioButton            = saveAsVideoRadioButton
          , GR.fileSizeToggleButton              = fileSizeToggleButton
          , GR.cropToggleButton                  = cropToggleButton
          , GR.textOverlaysToggleButton          = textOverlaysToggleButton
          , GR.saveOpenToggleButton              = saveOpenToggleButton
          , GR.uploadToggleButton                = uploadToggleButton
          , GR.videoPreviewPauseToggleButton     = videoPreviewPauseToggleButton
          , GR.inFileChooserDialogLabel          = inFileChooserDialogLabel
          , GR.inFileChooserButtonLabel          = inFileChooserButtonLabel
          , GR.startTimeAdjustment               = startTimeAdjustment
          , GR.durationTimeAdjustment            = durationTimeAdjustment
          , GR.widthAdjustment                   = widthAdjustment
          , GR.fpsAdjustment                     = fpsAdjustment
          , GR.colorCountAdjustment              = colorCountAdjustment
          , GR.outFileNameEntry                  = outFileNameEntry
          , GR.aboutDialogLabel                  = aboutDialogLabel
          , GR.statusLabel                       = statusLabel
          , GR.sidebarControlsPreviewbox         = sidebarControlsPreviewbox
          , GR.mainPreviewBox                    = mainPreviewBox
          , GR.imagesPreviewBox                  = imagesPreviewBox
          , GR.videoPreviewBox                   = videoPreviewBox
          , GR.videoPreviewOverlayChildBox       = videoPreviewOverlayChildBox
          , GR.fileSizeSpinButtonsGrid           = fileSizeSpinButtonsGrid
          , GR.cropSpinButtonsBox                = cropSpinButtonsBox
          , GR.textOverlaysMainBox               = textOverlaysMainBox
          , GR.textOverlaysBox                   = textOverlaysBox
          , GR.saveOpenBox                       = saveOpenBox
          , GR.uploadBox                         = uploadBox
          , GR.videoPreviewDrawingArea           = videoPreviewDrawingArea
          , GR.timeSlicesDrawingArea             = timeSlicesDrawingArea
          , GR.firstFramePreviewImageDrawingArea = firstFramePreviewImageDrawingArea
          , GR.lastFramePreviewImageDrawingArea  = lastFramePreviewImageDrawingArea
          , GR.inFileChooserButtonImage          = inFileChooserButtonImage
          , GR.firstFrameImage                   = firstFrameImage
          , GR.lastFrameImage                    = lastFrameImage
          , GR.inFileChooserDialog               = inFileChooserDialog
          , GR.confirmMessageDialog              = confirmMessageDialog
          , GR.aboutDialog                       = aboutDialog
          , GR.saveSpinner                       = saveSpinner
          , GR.inFileChooserWidget               = inFileChooserWidget
          , GR.maybeVideoPreviewWidget           = maybeVideoPreviewWidget
          , GR.maybePlaybinElement               = maybePlaybinElement
          , GR.temporaryDirectory                = temporaryDirectory
          , GR.guiInFilePropertiesRef            = guiInFilePropertiesRef
          , GR.textOverlaysRef                   = textOverlaysRef
          , GR.guiPreviewStateRef                = guiPreviewStateRef
          }

  hideWidgetsOnRealize              guiComponents
  handleSpinButtons                 guiComponents
  handleSaveButtonClick             guiComponents
  handleOpenButtonClick             guiComponents
  handleDialogs                     guiComponents
  handleSidebarSectionToggleButtons guiComponents
  handleUploadButtons               guiComponents
  handleWindow                      guiComponents
  handleGuiPreview                  guiComponents
  handleStatusLabelClick            guiComponents
  handleAboutDialogLabelClick       guiComponents

  GuiTextOverlays.handleTextOverlaysAddButton guiComponents

  GuiStyle.applyCss guiComponents

  GuiCapabilities.checkCapabilitiesAndNotify guiComponents

  GI.Gtk.main

buildBuilder :: IO GI.Gtk.Builder
buildBuilder = do
  gladeFile <- getDataFileName "data/gui.glade"
  GI.Gtk.builderNewFromFile (pack gladeFile)

builderGetObject
  ::  (GI.GObject.GObject b, GI.Gtk.IsBuilder a)
  =>  (Data.GI.Base.ManagedPtr b -> b)
  -> a
  -> String
  -> IO b
builderGetObject objectTypeClass builder objectId = do
  maybeObject <- GI.Gtk.builderGetObject builder $ pack objectId
  when (isNothing maybeObject) $
    putStrLn $ "[ERROR] could not build " ++ objectId ++ "."
  GI.Gtk.unsafeCastTo objectTypeClass $ fromJust maybeObject

handleFileChooserDialogReponse :: GR.GuiComponents -> Int32 -> IO ()
handleFileChooserDialogReponse
  guiComponents@GR.GuiComponents
    { GR.sidebarControlsPreviewbox
    , GR.startTimeSpinButton
    , GR.durationTimeSpinButton
    , GR.leftCropSpinButton
    , GR.rightCropSpinButton
    , GR.topCropSpinButton
    , GR.bottomCropSpinButton
    , GR.widthSpinButton
    , GR.fpsSpinButton
    , GR.colorCountSpinButton
    , GR.outFileNameEntry
    , GR.inFileChooserDialog
    , GR.statusLabel
    , GR.inFileChooserButtonLabel
    , GR.inFileChooserWidget
    , GR.guiInFilePropertiesRef
    , GR.temporaryDirectory
    }
  responseId
  = do
  GI.Gtk.widgetHide inFileChooserDialog
  when (enumToInt32 GI.Gtk.ResponseTypeOk == responseId) $ do
    resetSideBarControls
    maybeLoadFile
    syncStartAndDurationTimeSpinButtons guiComponents
  where
    maybeLoadFile :: IO ()
    maybeLoadFile = do
      maybeInFilePath <-
        fileChooserGetFilePath inFileChooserWidget
      case maybeInFilePath of
        Just inFilePath -> do
          maybePlayableMetadata <-
            Gifcurry.getPlayableMetadata
              Gifcurry.defaultGifParams
                { Gifcurry.inputFile = inFilePath }
          case maybePlayableMetadata of
            Just
              Gifcurry.PlayableMetadata
                { Gifcurry.playableMetadataWidth
                , Gifcurry.playableMetadataHeight
                , Gifcurry.playableMetadataDuration
                , Gifcurry.playableMetadataFps
                }
              -> do
                let isGif = ".gif" == Data.Text.toLower (Data.Text.pack $ takeExtension inFilePath)
                inFilePath' <-
                  if isGif
                    then do
                      bytes <- DBL.readFile inFilePath
                      return (temporaryDirectory ++ [pathSeparator] ++ show (MD5.md5 bytes) ++ ".webm")
                    else return inFilePath
                fileExists <-
                  if isGif
                    then doesFileExist inFilePath'
                    else return True
                when (isGif && not fileExists) $
                  adaptGif
                    inFilePath
                    inFilePath'
                    playableMetadataFps
                atomicModifyIORef' guiInFilePropertiesRef $
                  \ guiInFileProperties' ->
                    ( guiInFileProperties'
                        { GR.inFileUri      = if fileExists then inFilePath' else inFilePath
                        , GR.inFileDuration = playableMetadataDuration
                        , GR.inFileWidth    = playableMetadataWidth
                        , GR.inFileHeight   = playableMetadataHeight
                        }
                    , ()
                    )
                let startTimeFraction = 0.25
                let startTime         = playableMetadataDuration * startTimeFraction
                let endTime           = startTime * 3
                let durationTime      = endTime - startTime
                let durationText      = Data.Text.pack $ printf "%.3f" playableMetadataDuration
                GI.Gtk.spinButtonSetValue fpsSpinButton playableMetadataFps
                updateStartAndDurationTimeSpinButtons
                  guiComponents
                  startTime
                  durationTime
                updateStatusLabelAsync statusLabel 1 $
                  Data.Text.concat
                    [ "That "
                    , if isGif then "GIF" else "video"
                    , " is about "
                    , durationText
                    , " seconds long."
                    ]
                GI.Gtk.labelSetText inFileChooserButtonLabel $
                  Data.Text.pack $
                    takeFileName inFilePath
                GI.Gtk.widgetShow sidebarControlsPreviewbox
            _ -> resetOnFailure
        _ -> resetOnFailure
    resetOnFailure :: IO ()
    resetOnFailure = do
      atomicWriteIORef guiInFilePropertiesRef GR.defaultGuiInFileProperties
      GI.Gtk.widgetHide sidebarControlsPreviewbox
      updateStartAndDurationTimeSpinButtonRanges guiComponents
      GI.Gtk.spinButtonSetValue startTimeSpinButton    0.0
      GI.Gtk.spinButtonSetValue durationTimeSpinButton 0.0
      GI.Gtk.labelSetText inFileChooserButtonLabel "Open"
      resetSideBarControls
      updateStatusLabelAsync statusLabel 1 "Couldn't open that file."
    resetSideBarControls :: IO ()
    resetSideBarControls = do
      resetFileSizeSpinButtons
      resetCropSpinButtons
      resetTextEntries
      resetGuiTextOverlays
    resetTextEntries :: IO ()
    resetTextEntries = do
      let textEntries =
            [ outFileNameEntry
            ]
      mapM_
        (`GI.Gtk.entrySetText` "")
        textEntries
    resetGuiTextOverlays :: IO ()
    resetGuiTextOverlays =
      GuiTextOverlays.removeTextOverlays guiComponents
    resetFileSizeSpinButtons :: IO ()
    resetFileSizeSpinButtons = do
      GI.Gtk.spinButtonSetValue widthSpinButton      500.0
      GI.Gtk.spinButtonSetValue fpsSpinButton        24.0
      GI.Gtk.spinButtonSetValue colorCountSpinButton 256.0
    resetCropSpinButtons :: IO ()
    resetCropSpinButtons = do
      let spinButtons =
            [ leftCropSpinButton
            , rightCropSpinButton
            , topCropSpinButton
            , bottomCropSpinButton
            ]
      mapM_
        (`GI.Gtk.spinButtonSetValue` 0.0)
        spinButtons
    adaptGif :: String -> String -> Double -> IO ()
    adaptGif inFilePath outFilePath fps =
      void $
        forkIO $ do
          putStrLn "[INFO] Adapting the input GIF."
          result <-
            try
              ( readProcess
                  "ffmpeg"
                  [ "-nostats"
                  , "-loglevel"
                  , "error"
                  , "-y"
                  , "-i"
                  , inFilePath
                  , "-framerate"
                  , show fps
                  , "-c:v"
                  , "libvpx-vp9"
                  , "-crf"
                  , "37"
                  , "-b:v"
                  , "0"
                  , "-pix_fmt"
                  , "yuv420p"
                  , "-vf"
                  , "scale=trunc(iw/2)*2:trunc(ih/2)*2"
                  , "-an"
                  , outFilePath
                  ]
                  []
              ) :: IO (Either IOError String)
          when (isLeft result) $
            putStrLn "[ERROR] Something went wrong with FFmpeg."
          maybePlayableMetadata <-
            Gifcurry.getPlayableMetadata
              Gifcurry.defaultGifParams
                { Gifcurry.inputFile = outFilePath }
          case maybePlayableMetadata of
            Just
              Gifcurry.PlayableMetadata
                { Gifcurry.playableMetadataWidth
                , Gifcurry.playableMetadataHeight
                , Gifcurry.playableMetadataDuration
                }
              -> do
              atomicModifyIORef' guiInFilePropertiesRef $
                \ guiInFileProperties' ->
                  ( guiInFileProperties'
                      { GR.inFileUri      = outFilePath
                      , GR.inFileDuration = playableMetadataDuration
                      , GR.inFileWidth    = playableMetadataWidth
                      , GR.inFileHeight   = playableMetadataHeight
                      }
                  , ()
                  )
              putStrLn "[INFO] Adapted the input GIF."
            Nothing -> do
              putStrLn "[ERROR] Could not adapt the input GIF."
              GtkMainSyncAsync.gtkMainAsync resetOnFailure
              return ()

handleSpinButtons :: GR.GuiComponents -> IO ()
handleSpinButtons
  guiComponents@GR.GuiComponents
    { GR.startTimeSpinButton
    , GR.durationTimeSpinButton
    , GR.widthSpinButton
    , GR.fpsSpinButton
    , GR.colorCountSpinButton
    , GR.leftCropSpinButton
    , GR.rightCropSpinButton
    , GR.topCropSpinButton
    , GR.bottomCropSpinButton
    , GR.statusLabel
    , GR.guiInFilePropertiesRef
    }
  = do
  _ <- GI.Gtk.onSpinButtonValueChanged
    startTimeSpinButton
    handleStartTimeSpinButton
  _ <- GI.Gtk.onSpinButtonValueChanged
    durationTimeSpinButton
    handleDurationTimeSpinButton
  _ <- GI.Gtk.onSpinButtonValueChanged
    widthSpinButton
    handleWidthSpinButton
  _ <- GI.Gtk.onSpinButtonValueChanged
    fpsSpinButton
    handleFpsSpinButton
  _ <- GI.Gtk.onSpinButtonValueChanged
    colorCountSpinButton
    handleColorCountSpinButton
  _ <- GI.Gtk.onSpinButtonValueChanged
    leftCropSpinButton
    (handleCropSpinButton leftCropSpinButton rightCropSpinButton "left")
  _ <- GI.Gtk.onSpinButtonValueChanged
    rightCropSpinButton
    (handleCropSpinButton rightCropSpinButton leftCropSpinButton "right")
  _ <- GI.Gtk.onSpinButtonValueChanged
    topCropSpinButton
    (handleCropSpinButton topCropSpinButton bottomCropSpinButton "top")
  _ <- GI.Gtk.onSpinButtonValueChanged
    bottomCropSpinButton
    (handleCropSpinButton bottomCropSpinButton topCropSpinButton "bottom")
  return ()
  where
    handleStartTimeSpinButton :: IO ()
    handleStartTimeSpinButton = do
      startTime <- GI.Gtk.spinButtonGetValue startTimeSpinButton
      _         <- setSpinButtonFraction startTimeSpinButton
      if startTime < 0.0
        then do
          GI.Gtk.labelSetText statusLabel "The start time is wrong."
          highlightSpinButton startTimeSpinButton
        else do
          GI.Gtk.labelSetText statusLabel "Ready."
          unhighlightSpinButton startTimeSpinButton
      void $ syncStartAndDurationTimeSpinButtons guiComponents
    handleDurationTimeSpinButton :: IO ()
    handleDurationTimeSpinButton = do
      startTime     <- GI.Gtk.spinButtonGetValue startTimeSpinButton
      durationTime  <- GI.Gtk.spinButtonGetValue durationTimeSpinButton
      fileDuration  <- inFileDuration
      _ <- GI.Gtk.setEntryProgressFraction durationTimeSpinButton 0.5
      _ <- setSpinButtonFraction durationTimeSpinButton
      if
           durationTime < 0.0
        -- 2.1 > 10.2 - 8.1
        || (10.0 * durationTime) > ((10.0 * fileDuration) - (10.0 * startTime))
        || durationTime > fileDuration
        then do
          GI.Gtk.labelSetText statusLabel "The duration time is wrong."
          highlightSpinButton durationTimeSpinButton
        else do
          GI.Gtk.labelSetText statusLabel "Ready."
          unhighlightSpinButton durationTimeSpinButton
      void $ syncStartAndDurationTimeSpinButtons guiComponents
    handleWidthSpinButton :: IO ()
    handleWidthSpinButton = do
      width <- GI.Gtk.spinButtonGetValue widthSpinButton
      _     <- setSpinButtonFraction widthSpinButton
      if width <= 0.0
        then do
          GI.Gtk.labelSetText statusLabel "The width is wrong."
          highlightSpinButton widthSpinButton
        else do
          GI.Gtk.labelSetText statusLabel "Ready."
          unhighlightSpinButton widthSpinButton
    handleFpsSpinButton :: IO ()
    handleFpsSpinButton = do
      fps <- GI.Gtk.spinButtonGetValue fpsSpinButton
      _   <- setSpinButtonFraction fpsSpinButton
      if fps < 15.0 || fps > 60.0
        then do
          GI.Gtk.labelSetText statusLabel "The FPS is wrong."
          highlightSpinButton fpsSpinButton
        else do
          GI.Gtk.labelSetText statusLabel "Ready."
          unhighlightSpinButton fpsSpinButton
    handleColorCountSpinButton :: IO ()
    handleColorCountSpinButton = do
      colorCount <- GI.Gtk.spinButtonGetValue colorCountSpinButton
      _          <- setSpinButtonFraction colorCountSpinButton
      if colorCount < 1.0 || colorCount > 256.0
        then do
          GI.Gtk.labelSetText statusLabel "The color count is wrong."
          highlightSpinButton colorCountSpinButton
        else do
          GI.Gtk.labelSetText statusLabel "Ready."
          unhighlightSpinButton colorCountSpinButton
    handleCropSpinButton :: GI.Gtk.SpinButton -> GI.Gtk.SpinButton -> Text -> IO ()
    handleCropSpinButton a b t = do
      cropValue <- GI.Gtk.spinButtonGetValue a
      _         <- setSpinButtonFraction a
      if cropValue < 0.0 || cropValue >= 1.0
        then do
          GI.Gtk.labelSetText statusLabel $ Data.Text.concat ["The ", t, " crop is wrong."]
          highlightSpinButton a
        else do
          GI.Gtk.labelSetText statusLabel "Ready."
          unhighlightSpinButton a
      syncCropSpinButtons a b
    syncCropSpinButtons :: GI.Gtk.SpinButton -> GI.Gtk.SpinButton -> IO ()
    syncCropSpinButtons a b = do
      aValue <- GI.Gtk.spinButtonGetValue a
      bValue <- GI.Gtk.spinButtonGetValue b
      when (aValue + bValue >= 1) $ do
        let newValue  = 1.0 - aValue - 0.01
        let newValue' = if newValue < 0.0 then 0.0 else newValue
        void $ GI.Gtk.spinButtonSetValue b newValue'
      return ()
    inFileDuration :: IO Double
    inFileDuration =
      GR.inFileDuration <$> readIORef guiInFilePropertiesRef

handleSaveButtonClick :: GR.GuiComponents -> IO ()
handleSaveButtonClick
  guiComponents@GR.GuiComponents
    { GR.outFileChooserButton
    , GR.outFileNameEntry
    , GR.saveButton
    , GR.openButton
    , GR.saveAsVideoRadioButton
    , GR.startTimeSpinButton
    , GR.durationTimeSpinButton
    , GR.widthSpinButton
    , GR.fpsSpinButton
    , GR.colorCountSpinButton
    , GR.leftCropSpinButton
    , GR.rightCropSpinButton
    , GR.topCropSpinButton
    , GR.bottomCropSpinButton
    , GR.statusLabel
    , GR.confirmMessageDialog
    , GR.saveSpinner
    , GR.guiInFilePropertiesRef
    }
  =
  void $
    GI.Gtk.onWidgetButtonReleaseEvent
      saveButton $
        \ _ -> do
        GR.GuiInFileProperties
          { GR.inFileUri = inFilePath
          }            <- readIORef guiInFilePropertiesRef
        startTime      <- GI.Gtk.spinButtonGetValue startTimeSpinButton
        durationTime   <- GI.Gtk.spinButtonGetValue durationTimeSpinButton
        width          <- GI.Gtk.spinButtonGetValue widthSpinButton
        fps            <- GI.Gtk.spinButtonGetValue fpsSpinButton
        colorCount     <- GI.Gtk.spinButtonGetValue colorCountSpinButton
        leftCrop       <- GI.Gtk.spinButtonGetValue leftCropSpinButton
        rightCrop      <- GI.Gtk.spinButtonGetValue rightCropSpinButton
        topCrop        <- GI.Gtk.spinButtonGetValue topCropSpinButton
        bottomCrop     <- GI.Gtk.spinButtonGetValue bottomCropSpinButton
        saveAsVideo    <- GI.Gtk.toggleButtonGetActive saveAsVideoRadioButton
        outFilePath    <- outFileChooserButtonGetFilePath outFileChooserButton outFileNameEntry
        textOverlays   <- GuiTextOverlays.getGifcurryTextOverlays guiComponents
        let params =
              Gifcurry.defaultGifParams
                { Gifcurry.inputFile      = inFilePath
                , Gifcurry.outputFile     = outFilePath
                , Gifcurry.saveAsVideo    = saveAsVideo
                , Gifcurry.startTime      = startTime
                , Gifcurry.durationTime   = durationTime
                , Gifcurry.width          = truncate width
                , Gifcurry.fps            = truncate fps
                , Gifcurry.colorCount     = truncate colorCount
                , Gifcurry.leftCrop       = leftCrop
                , Gifcurry.rightCrop      = rightCrop
                , Gifcurry.topCrop        = topCrop
                , Gifcurry.bottomCrop     = bottomCrop
                , Gifcurry.textOverlays   = textOverlays
                }
        paramsValid <- Gifcurry.gifParamsValid params
        GI.Gtk.labelSetText statusLabel "Ready."
        if paramsValid
          then do
            GI.Gtk.setMessageDialogText
              confirmMessageDialog $
                Data.Text.concat
                  [ "Create a "
                  , if saveAsVideo then "video" else "GIF"
                  , " with that long of a duration?"
                  ]
            confirmMessageDialogResponse <-
              if durationTime >= durationTimeWarningLevel
                then GI.Gtk.dialogRun confirmMessageDialog
                else return (enumToInt32 GI.Gtk.ResponseTypeYes)
            when (confirmMessageDialogResponse == enumToInt32 GI.Gtk.ResponseTypeYes) $ do
              GI.Gtk.widgetSetSensitive saveButton False
              GI.Gtk.widgetSetSensitive openButton False
              GI.Gtk.widgetHide saveButton
              GI.Gtk.widgetShow saveSpinner
              GI.Gtk.setSpinnerActive saveSpinner True
              void $ forkOS $ do
                GtkMainSyncAsync.gtkMainAsync $
                  GI.Gtk.labelSetText
                    statusLabel $
                      Data.Text.concat
                        [ "One "
                        , if saveAsVideo then "video" else "GIF"
                        , " coming up!"
                        ]
                result <- Gifcurry.gif params
                case result of
                  Left _ ->
                    GtkMainSyncAsync.gtkMainAsync $
                      GI.Gtk.labelSetMarkup
                        statusLabel $
                          Data.Text.concat
                            [ "Didn't work. Check your settings. "
                            , "If you think it's a bug please open an "
                            , "<a href=\"https://github.com/lettier/gifcurry/issues\">issue</a>"
                            , "."
                            ]
                  Right filePath -> do
                    _ <-
                      forkOS $
                        GtkMainSyncAsync.gtkMainAsync $
                          openLocalFileWithDefaultProgram filePath
                    GtkMainSyncAsync.gtkMainAsync $
                      GI.Gtk.labelSetText statusLabel "Ready."
                GtkMainSyncAsync.gtkMainAsync $ do
                  GI.Gtk.setSpinnerActive saveSpinner False
                  GI.Gtk.widgetHide saveSpinner
                  GI.Gtk.widgetShow saveButton
                  GI.Gtk.widgetSetSensitive saveButton True
                  GI.Gtk.widgetSetSensitive openButton True
          else GI.Gtk.labelSetText statusLabel "The settings are wrong."
        return True

handleOpenButtonClick :: GR.GuiComponents -> IO ()
handleOpenButtonClick
  GR.GuiComponents
    { GR.openButton
    , GR.saveAsVideoRadioButton
    , GR.outFileChooserButton
    , GR.outFileNameEntry
    , GR.statusLabel
    }
  =
  void $ GI.Gtk.onWidgetButtonReleaseEvent openButton $ \ _ -> do
    saveAsVideo <- GI.Gtk.toggleButtonGetActive saveAsVideoRadioButton
    outFilePath <- outFileChooserButtonGetFilePath outFileChooserButton outFileNameEntry
    let outFilePath' =
          Gifcurry.getOutputFileWithExtension $
            Gifcurry.defaultGifParams
              { Gifcurry.outputFile = outFilePath
              , Gifcurry.saveAsVideo = saveAsVideo
              }
    fileExists <- doesFileExist outFilePath'
    if fileExists
      then do
        GI.Gtk.labelSetText statusLabel "Ready."
        void $
          forkIO $
            GtkMainSyncAsync.gtkMainAsync $
              openLocalFileWithDefaultProgram outFilePath'
      else
        GI.Gtk.labelSetText
          statusLabel
          "Couldn't find the file. Check your settings."
    return True

handleDialogs :: GR.GuiComponents -> IO ()
handleDialogs
  guiComponents@GR.GuiComponents
    { GR.inFileChooserDialog
    , GR.aboutDialog
    , GR.confirmMessageDialog
    , GR.aboutButton
    , GR.inFileChooserButton
    }
  = do
  _ <- GI.Gtk.onWidgetButtonReleaseEvent
    aboutButton
    (\ _ -> GI.Gtk.dialogRun aboutDialog >> return True)
  _ <- GI.Gtk.onDialogResponse
    confirmMessageDialog
    (\ _ -> GI.Gtk.widgetHide confirmMessageDialog)
  _ <- GI.Gtk.onDialogResponse
    aboutDialog
    (\ _ -> GI.Gtk.widgetHide aboutDialog)
  _ <- GI.Gtk.onWidgetButtonReleaseEvent inFileChooserButton
    (\ _ -> GI.Gtk.dialogRun inFileChooserDialog >> return True)
  _ <- GI.Gtk.onDialogResponse inFileChooserDialog
    (handleFileChooserDialogReponse guiComponents)
  return ()

-- GI.Gtk.Expander does not work.
-- While it visually hides its children, they are still present
-- in terms of activation. This prevents the children of
-- other expanded sections from working.
-- This workaround emulates the expander functionality.
handleSidebarSectionToggleButtons :: GR.GuiComponents -> IO ()
handleSidebarSectionToggleButtons
  GR.GuiComponents
    { GR.fileSizeToggleButton
    , GR.cropToggleButton
    , GR.textOverlaysToggleButton
    , GR.saveOpenToggleButton
    , GR.uploadToggleButton
    , GR.fileSizeSpinButtonsGrid
    , GR.cropSpinButtonsBox
    , GR.textOverlaysMainBox
    , GR.saveOpenBox
    , GR.uploadBox
    }
  =
  handleToggles
    [ fileSizeToggleButton
    , cropToggleButton
    , textOverlaysToggleButton
    , saveOpenToggleButton
    , uploadToggleButton
    ]
    [ GI.Gtk.unsafeCastTo GI.Gtk.Widget fileSizeSpinButtonsGrid
    , GI.Gtk.unsafeCastTo GI.Gtk.Widget cropSpinButtonsBox
    , GI.Gtk.unsafeCastTo GI.Gtk.Widget textOverlaysMainBox
    , GI.Gtk.unsafeCastTo GI.Gtk.Widget saveOpenBox
    , GI.Gtk.unsafeCastTo GI.Gtk.Widget uploadBox
    ]
  where
    handleToggles
      ::  GI.Gtk.IsWidget a
      =>  [GI.Gtk.ToggleButton]
      ->  [IO a]
      ->  IO ()
    handleToggles
      toggleButtons
      ioWidgets
      =
      mapM_
        (\ (toggleButton, ioWidget) ->
          void $
            GI.Gtk.onToggleButtonToggled toggleButton $
              handleOnToggle
                toggleButton
                ioWidget
                toggleButtons
        ) $
        Data.List.zip toggleButtons ioWidgets
    handleOnToggle
      ::  GI.Gtk.IsWidget a
      =>  GI.Gtk.ToggleButton
      ->  IO a
      ->  [GI.Gtk.ToggleButton]
      ->  IO ()
    handleOnToggle toggleButton ioWidget toggleButtons = do
      widget <- ioWidget
      active <- GI.Gtk.toggleButtonGetActive toggleButton
      if active
        then do
          GI.Gtk.widgetShow widget
          mapM_
            (\ x ->
              isNotToggleButton x >>=
                \ b ->
                  when b $
                    GI.Gtk.setToggleButtonActive x False
            )
            toggleButtons
        else
          GI.Gtk.widgetHide widget
      where
        isNotToggleButton :: GI.Gtk.ToggleButton -> IO Bool
        isNotToggleButton x = do
          a <- GI.Gtk.widgetGetName toggleButton
          b <- GI.Gtk.widgetGetName x
          return $ a /= b

handleUploadButtons :: GR.GuiComponents -> IO ()
handleUploadButtons
  GR.GuiComponents
    { GR.giphyUploadButton
    , GR.imgurUploadButton
    }
  =
  mapM_
    (\ (button, link) ->
      GI.Gtk.onButtonClicked button $ openUriWithDefaultProgram link
    )
    [ (giphyUploadButton, "https://giphy.com/upload/")
    , (imgurUploadButton, "https://imgur.com/upload")
    ]

handleGuiPreview :: GR.GuiComponents -> IO ()
handleGuiPreview = GuiPreview.runGuiPreview

handleWindow :: GR.GuiComponents -> IO ()
handleWindow GR.GuiComponents { GR.window } = do
  -- Setting the window to resizable false causes the video
  -- to load at its natural size.
  -- This workaround locks the window size which is required
  -- to get accurate crop measurements.
  geometry <- GI.Gdk.newZeroGeometry
  GI.Gdk.setGeometryMinWidth  geometry 1
  GI.Gdk.setGeometryMaxWidth  geometry 1
  GI.Gdk.setGeometryMinHeight geometry 1
  GI.Gdk.setGeometryMaxHeight geometry 1
  GI.Gtk.windowSetGeometryHints
    window
    (Just window)
    (Just geometry)
    [GI.Gdk.WindowHintsMinSize, GI.Gdk.WindowHintsMaxSize]
  _ <- GI.Gtk.onWidgetDestroy window GI.Gtk.mainQuit
  GI.Gtk.widgetShowAll window

openLocalFileWithDefaultProgram :: String -> IO ()
openLocalFileWithDefaultProgram filePath = do
  fileExists <- doesFileExist filePath
  when fileExists $ openUriWithDefaultProgram filePath

openUriWithDefaultProgram :: String -> IO ()
openUriWithDefaultProgram uri = do
  maybeFileOpenCommand <- determineFileOpenCommand
  case maybeFileOpenCommand of
    Just fileOpenCommand ->
      void $ spawnCommand $ fileOpenCommand ++ " \"" ++ uri ++ "\""
    _ -> return ()
  where
    determineFileOpenCommand :: IO (Maybe String)
    determineFileOpenCommand = do
      uname  <- tryReadProcess "uname"   ["-a"]
      swVers <- tryReadProcess "sw_vers" []
      ver    <- tryReadProcess "ver"     []
      let linuxCommand   = "xdg-open"
      let macCommand     = "open"
      let windowsCommand = "explorer"
      case (uname, swVers, ver) of
        (Right s, _, _) ->
          case (containsText "darwin" s, containsText "linux" s) of
            (True, _)   -> return $ Just macCommand
            (_, True)   -> return $ Just linuxCommand
            _           -> return Nothing
        (_, Right s, _) -> returnIf "mac"     s macCommand
        (_, _, Right s) -> returnIf "windows" s windowsCommand
        _               -> return Nothing
      where
        tryReadProcess :: String -> [String] -> IO (Either IOError String)
        tryReadProcess process params = try $ readProcess process params []
        returnIf :: Text -> String -> String -> IO (Maybe String)
        returnIf needle haystack command =
          if containsText needle haystack
            then return $ Just command
            else return Nothing
        containsText :: Text -> String -> Bool
        containsText needle haystack =
          Data.Text.isInfixOf
            needle
            (Data.Text.toLower $ Data.Text.pack haystack)

outFileChooserButtonGetFilePath :: GI.Gtk.FileChooserButton -> GI.Gtk.Entry -> IO String
outFileChooserButtonGetFilePath outFileChooserButton outFileNameEntry = do
  filePath <- fileChooserGetString outFileChooserButton
  fileName <- Data.Text.unpack . Data.Text.strip <$> GI.Gtk.entryGetText outFileNameEntry
  if Data.List.null filePath || Data.List.null fileName
    then return fileName
    else do
      isDirectory <- System.Directory.doesDirectoryExist filePath
      if isDirectory
        then return $ System.FilePath.combine filePath fileName
        else return fileName

highlightSpinButton :: GI.Gtk.SpinButton -> IO ()
highlightSpinButton =
  styleSpinButtonAndEntry
    "{ background-color: #eb3b5a; color: white; }"

unhighlightSpinButton :: GI.Gtk.SpinButton -> IO ()
unhighlightSpinButton = styleSpinButtonAndEntry "{}"

styleSpinButtonAndEntry :: String -> GI.Gtk.SpinButton -> IO ()
styleSpinButtonAndEntry style spinButton = do
  name      <- Data.Text.unpack . Data.Text.strip <$> GI.Gtk.widgetGetName spinButton
  let name' = if Data.List.null name then "" else "#" ++ name
  GuiStyle.styleWidget
    (  "spinbutton"
    ++ name'
    ++ " entry "
    ++ style
    ++ " GtkSpinButton"
    ++ name'
    ++ " GtkEntry "
    ++ style
    )
    spinButton

updateStatusLabelAsync :: GI.Gtk.Label -> Word32 -> Text -> IO ()
updateStatusLabelAsync statusLabel seconds message =
  void $ GI.GLib.timeoutAdd
    GI.GLib.PRIORITY_DEFAULT
    seconds $ do
      GtkMainSyncAsync.gtkMainAsync $
        GI.Gtk.labelSetText statusLabel message
      return False

setSpinButtonFraction :: GI.Gtk.SpinButton -> IO ()
setSpinButtonFraction spinButton = do
  (_, maxValue) <- GI.Gtk.spinButtonGetRange spinButton
  value         <- GI.Gtk.spinButtonGetValue spinButton
  let fraction  = if maxValue <= 0.0 then 0.0 else abs $ value / maxValue
  void $ GI.Gtk.setEntryProgressFraction spinButton $ truncatePastDigit fraction 2

updateStartAndDurationTimeSpinButtonRanges :: GR.GuiComponents -> IO ()
updateStartAndDurationTimeSpinButtonRanges
  GR.GuiComponents
    { GR.startTimeSpinButton
    , GR.durationTimeSpinButton
    , GR.guiInFilePropertiesRef
    }
  = do
  startTime           <- GI.Gtk.spinButtonGetValue startTimeSpinButton
  fileDuration        <- GR.inFileDuration <$> readIORef guiInFilePropertiesRef
  let startTime'      = if startTime >= fileDuration        then fileDuration else startTime
  let maxDurationTime = if fileDuration - startTime' <= 0.0 then 0.0           else fileDuration - startTime'
  let buffer          = if fileDuration * 0.01 > 0.1        then 0.1           else fileDuration * 0.01
  GI.Gtk.spinButtonSetRange startTimeSpinButton 0.0 (fileDuration - buffer)
  GI.Gtk.spinButtonSetRange durationTimeSpinButton buffer maxDurationTime

syncStartAndDurationTimeSpinButtons :: GR.GuiComponents -> IO ()
syncStartAndDurationTimeSpinButtons
  guiComponents@GR.GuiComponents
    { GR.startTimeSpinButton
    , GR.durationTimeSpinButton
    , GR.guiInFilePropertiesRef
    }
  = do
  startTime           <- GI.Gtk.spinButtonGetValue startTimeSpinButton
  durationTime        <- GI.Gtk.spinButtonGetValue durationTimeSpinButton
  fileDuration        <- GR.inFileDuration <$> readIORef guiInFilePropertiesRef
  let startTime'      = if startTime >= fileDuration        then fileDuration    else startTime
  let maxDurationTime = if fileDuration - startTime' <= 0.0 then 0.0             else fileDuration - startTime'
  let durationTime'   = if durationTime >= maxDurationTime  then maxDurationTime else durationTime
  updateStartAndDurationTimeSpinButtons
    guiComponents
    startTime'
    durationTime'

updateStartAndDurationTimeSpinButtons
  ::  GR.GuiComponents
  ->  Double
  ->  Double
  ->  IO ()
updateStartAndDurationTimeSpinButtons
  guiComponents@GR.GuiComponents
    { GR.startTimeSpinButton
    , GR.durationTimeSpinButton
    }
  startTime
  durationTime
  = do
  updateStartAndDurationTimeSpinButtonRanges guiComponents
  GI.Gtk.spinButtonSetValue startTimeSpinButton    $ truncatePastDigit startTime    3
  GI.Gtk.spinButtonSetValue durationTimeSpinButton $ truncatePastDigit durationTime 3
  updateStartAndDurationTimeAdjustments         guiComponents
  updateStartAndDurationTimeSpinButtonFractions guiComponents

updateStartAndDurationTimeAdjustments
  ::  GR.GuiComponents
  -> IO ()
updateStartAndDurationTimeAdjustments
  GR.GuiComponents
    { GR.startTimeAdjustment
    , GR.durationTimeAdjustment
    , GR.guiInFilePropertiesRef
    }
  = do
  duration <- GR.inFileDuration <$> readIORef guiInFilePropertiesRef
  let fraction = duration / 100.0
  let step     = if fraction < 0.1 then 0.1 else fraction
  GI.Gtk.adjustmentSetStepIncrement startTimeAdjustment    step
  GI.Gtk.adjustmentSetStepIncrement durationTimeAdjustment step

updateStartAndDurationTimeSpinButtonFractions :: GR.GuiComponents -> IO ()
updateStartAndDurationTimeSpinButtonFractions
  GR.GuiComponents
    { GR.startTimeSpinButton
    , GR.durationTimeSpinButton
    }
  = do
  setSpinButtonFraction startTimeSpinButton
  setSpinButtonFraction durationTimeSpinButton

hideWidgetsOnRealize :: GR.GuiComponents -> IO ()
hideWidgetsOnRealize
  GR.GuiComponents
    { GR.saveSpinner
    , GR.sidebarControlsPreviewbox
    , GR.fileSizeSpinButtonsGrid
    , GR.cropSpinButtonsBox
    , GR.textOverlaysMainBox
    , GR.saveOpenBox
    , GR.uploadBox
    }
  = do
  let widgets =
        [ GI.Gtk.unsafeCastTo GI.Gtk.Widget saveSpinner
        , GI.Gtk.unsafeCastTo GI.Gtk.Widget sidebarControlsPreviewbox
        , GI.Gtk.unsafeCastTo GI.Gtk.Widget fileSizeSpinButtonsGrid
        , GI.Gtk.unsafeCastTo GI.Gtk.Widget cropSpinButtonsBox
        , GI.Gtk.unsafeCastTo GI.Gtk.Widget textOverlaysMainBox
        , GI.Gtk.unsafeCastTo GI.Gtk.Widget saveOpenBox
        , GI.Gtk.unsafeCastTo GI.Gtk.Widget uploadBox
        ]
  mapM_ hideOnRealize widgets
  where
    hideOnRealize :: GI.Gtk.IsWidget a => IO a -> IO ()
    hideOnRealize ioWidget = do
      widget <- ioWidget
      void $
        GI.Gtk.onWidgetRealize widget $
          GI.Gtk.widgetHide widget

handleStatusLabelClick
  ::  GR.GuiComponents
  ->  IO ()
handleStatusLabelClick
  GR.GuiComponents
    { GR.statusLabel
    }
  =
  handleLabelClick statusLabel

handleAboutDialogLabelClick
  ::  GR.GuiComponents
  ->  IO ()
handleAboutDialogLabelClick
  GR.GuiComponents
    { GR.aboutDialogLabel
    }
  =
  handleLabelClick aboutDialogLabel

handleLabelClick
  ::  GI.Gtk.Label
  -> IO ()
handleLabelClick
  label
  =
  void $
    GI.Gtk.onLabelActivateLink
      label $
        \ uri -> do
          openUriWithDefaultProgram
            (Data.Text.unpack uri)
          return True

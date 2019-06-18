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
import Data.Int
import Data.GI.Base.Overloading
import qualified Data.Digest.Pure.MD5 as MD5
import qualified Data.ByteString.Lazy as DBL
import Control.Monad
import Control.Concurrent
import Control.Exception
import Data.GI.Base
import qualified GI.GObject
import qualified GI.Gdk
import qualified GI.Gtk
import qualified GI.GLib
import GI.Gst

import Paths_Gifcurry
import qualified Gifcurry
  ( GifParams(..)
  , PlayableMetadata(..)
  , createGif
  , defaultGifParams
  , validateGifParams
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
import qualified GuiKeyboard
import GuiMisc

durationTimeWarningLevel
  ::  Double
durationTimeWarningLevel = 10.0

main
  ::  IO ()
main = do
  _ <- GI.Gst.init Nothing
  _ <- GI.Gtk.init Nothing

  builder <- buildBuilder

  window                              <- builderGetObject GI.Gtk.Window            builder "gifcurry-window"
  startTimeSpinButton                 <- builderGetObject GI.Gtk.SpinButton        builder "start-time-spin-button"
  endTimeSpinButton                   <- builderGetObject GI.Gtk.SpinButton        builder "end-time-spin-button"
  widthSpinButton                     <- builderGetObject GI.Gtk.SpinButton        builder "width-spin-button"
  fpsSpinButton                       <- builderGetObject GI.Gtk.SpinButton        builder "fps-spin-button"
  colorCountSpinButton                <- builderGetObject GI.Gtk.SpinButton        builder "color-count-spin-button"
  leftCropSpinButton                  <- builderGetObject GI.Gtk.SpinButton        builder "left-crop-spin-button"
  rightCropSpinButton                 <- builderGetObject GI.Gtk.SpinButton        builder "right-crop-spin-button"
  topCropSpinButton                   <- builderGetObject GI.Gtk.SpinButton        builder "top-crop-spin-button"
  bottomCropSpinButton                <- builderGetObject GI.Gtk.SpinButton        builder "bottom-crop-spin-button"
  inFileChooserButton                 <- builderGetObject GI.Gtk.Button            builder "in-file-chooser-button"
  inFileChooserDialogCancelButton     <- builderGetObject GI.Gtk.Button            builder "in-file-chooser-dialog-cancel-button"
  inFileChooserDialogOpenButton       <- builderGetObject GI.Gtk.Button            builder "in-file-chooser-dialog-open-button"
  outFileChooserDialogCancelButton    <- builderGetObject GI.Gtk.Button            builder "out-file-chooser-dialog-cancel-button"
  outFileChooserDialogViewButton      <- builderGetObject GI.Gtk.Button            builder "out-file-chooser-dialog-view-button"
  outFileChooserDialogSaveButton      <- builderGetObject GI.Gtk.Button            builder "out-file-chooser-dialog-save-button"
  outFileButton                       <- builderGetObject GI.Gtk.Button            builder "out-file-button"
  textOverlaysAddButton               <- builderGetObject GI.Gtk.Button            builder "text-overlays-add-button"
  textOverlaysOpenButton              <- builderGetObject GI.Gtk.Button            builder "text-overlays-open-button"
  textOverlaysSaveButton              <- builderGetObject GI.Gtk.Button            builder "text-overlays-save-button"
  textOverlaysRemoveAllButton         <- builderGetObject GI.Gtk.Button            builder "text-overlays-remove-all-button"
  confirmMessageDialogYesButton       <- builderGetObject GI.Gtk.Button            builder "confirm-message-dialog-yes-button"
  confirmMessageDialogNoButton        <- builderGetObject GI.Gtk.Button            builder "confirm-message-dialog-no-button"
  aboutButton                         <- builderGetObject GI.Gtk.Button            builder "about-button"
  aboutDialogCloseButton              <- builderGetObject GI.Gtk.Button            builder "about-dialog-close-button"
  giphyUploadButton                   <- builderGetObject GI.Gtk.Button            builder "giphy-upload-button"
  imgurUploadButton                   <- builderGetObject GI.Gtk.Button            builder "imgur-upload-button"
  outFileChooserDialogGifRadioButton  <- builderGetObject GI.Gtk.RadioButton       builder "out-file-chooser-dialog-gif-radio-button"
  fileSizeToggleButton                <- builderGetObject GI.Gtk.ToggleButton      builder "file-size-toggle-button"
  cropToggleButton                    <- builderGetObject GI.Gtk.ToggleButton      builder "crop-toggle-button"
  textOverlaysToggleButton            <- builderGetObject GI.Gtk.ToggleButton      builder "text-overlays-toggle-button"
  uploadToggleButton                  <- builderGetObject GI.Gtk.ToggleButton      builder "upload-toggle-button"
  videoPreviewPauseToggleButton       <- builderGetObject GI.Gtk.ToggleButton      builder "video-preview-pause-toggle-button"
  ditherToggleButton                  <- builderGetObject GI.Gtk.ToggleButton      builder "dither-toggle-button"
  inFileChooserDialogLabel            <- builderGetObject GI.Gtk.Label             builder "in-file-chooser-dialog-label"
  inFileChooserButtonLabel            <- builderGetObject GI.Gtk.Label             builder "in-file-chooser-button-label"
  startTimeAdjustment                 <- builderGetObject GI.Gtk.Adjustment        builder "start-time-adjustment"
  endTimeAdjustment                   <- builderGetObject GI.Gtk.Adjustment        builder "end-time-adjustment"
  widthAdjustment                     <- builderGetObject GI.Gtk.Adjustment        builder "width-adjustment"
  fpsAdjustment                       <- builderGetObject GI.Gtk.Adjustment        builder "fps-adjustment"
  colorCountAdjustment                <- builderGetObject GI.Gtk.Adjustment        builder "color-count-adjustment"
  aboutDialogLabel                    <- builderGetObject GI.Gtk.Label             builder "about-dialog-label"
  statusLabel                         <- builderGetObject GI.Gtk.Label             builder "status-label"
  sidebarControlsPreviewbox           <- builderGetObject GI.Gtk.Box               builder "sidebar-controls-preview-box"
  mainPreviewBox                      <- builderGetObject GI.Gtk.Box               builder "main-preview-box"
  imagesPreviewBox                    <- builderGetObject GI.Gtk.Box               builder "images-preview-box"
  videoPreviewBox                     <- builderGetObject GI.Gtk.Box               builder "video-preview-box"
  videoPreviewOverlayChildBox         <- builderGetObject GI.Gtk.Box               builder "video-preview-overlay-child-box"
  cropSpinButtonsBox                  <- builderGetObject GI.Gtk.Box               builder "crop-spin-buttons-box"
  textOverlaysMainBox                 <- builderGetObject GI.Gtk.Box               builder "text-overlays-main-box"
  textOverlaysBox                     <- builderGetObject GI.Gtk.Box               builder "text-overlays-box"
  uploadBox                           <- builderGetObject GI.Gtk.Box               builder "upload-box"
  fileSizeSpinButtonsGrid             <- builderGetObject GI.Gtk.Grid              builder "file-size-spin-buttons-grid"
  videoPreviewDrawingArea             <- builderGetObject GI.Gtk.DrawingArea       builder "video-preview-drawing-area"
  timeSlicesDrawingArea               <- builderGetObject GI.Gtk.DrawingArea       builder "time-slices-drawing-area"
  firstFramePreviewImageDrawingArea   <- builderGetObject GI.Gtk.DrawingArea       builder "first-frame-preview-image-drawing-area"
  lastFramePreviewImageDrawingArea    <- builderGetObject GI.Gtk.DrawingArea       builder "last-frame-preview-image-drawing-area"
  inFileChooserButtonImage            <- builderGetObject GI.Gtk.Image             builder "in-file-chooser-button-image"
  firstFrameImage                     <- builderGetObject GI.Gtk.Image             builder "first-frame-image"
  lastFrameImage                      <- builderGetObject GI.Gtk.Image             builder "last-frame-image"
  inFileChooserDialog                 <- builderGetObject GI.Gtk.Dialog            builder "in-file-chooser-dialog"
  outFileChooserDialog                <- builderGetObject GI.Gtk.FileChooserDialog builder "out-file-chooser-dialog"
  textOverlaysOpenDialog              <- builderGetObject GI.Gtk.FileChooserDialog builder "text-overlays-open-file-chooser-dialog"
  textOverlaysSaveDialog              <- builderGetObject GI.Gtk.FileChooserDialog builder "text-overlays-save-file-chooser-dialog"
  confirmMessageDialog                <- builderGetObject GI.Gtk.MessageDialog     builder "confirm-message-dialog"
  aboutDialog                         <- builderGetObject GI.Gtk.Dialog            builder "about-dialog"
  inFileChooserWidget                 <- builderGetObject GI.Gtk.FileChooserWidget builder "in-file-chooser-widget"
  outFileChooserDialogGifFileFilter   <- builderGetObject GI.Gtk.FileFilter        builder "out-file-chooser-dialog-gif-file-filter"
  outFileChooserDialogVideoFileFilter <- builderGetObject GI.Gtk.FileFilter        builder "out-file-chooser-dialog-video-file-filter"

  -- Glade does not allow us to use the response ID nicknames so we set them here.
  GI.Gtk.dialogAddActionWidget confirmMessageDialog confirmMessageDialogYesButton    $ enumToInt32 GI.Gtk.ResponseTypeYes
  GI.Gtk.dialogAddActionWidget confirmMessageDialog confirmMessageDialogNoButton     $ enumToInt32 GI.Gtk.ResponseTypeNo
  GI.Gtk.dialogAddActionWidget inFileChooserDialog  inFileChooserDialogCancelButton  $ enumToInt32 GI.Gtk.ResponseTypeCancel
  GI.Gtk.dialogAddActionWidget inFileChooserDialog  inFileChooserDialogOpenButton    $ enumToInt32 GI.Gtk.ResponseTypeOk

  (maybeVideoPreviewWidget, maybePlaybinElement) <-
    GuiPreview.buildVideoPreviewWidgetAndPlaybinElement

  temporaryDirectory <- Gifcurry.findOrCreateTemporaryDirectory

  guiInFilePropertiesRef <- newIORef GR.defaultGuiInFileProperties
  textOverlaysRef        <- newIORef []
  guiPreviewStateRef     <- newIORef GR.defaultGuiPreviewState

  let guiComponents =
        GR.GuiComponents
          { GR.window                              = window
          , GR.startTimeSpinButton                 = startTimeSpinButton
          , GR.endTimeSpinButton                   = endTimeSpinButton
          , GR.widthSpinButton                     = widthSpinButton
          , GR.fpsSpinButton                       = fpsSpinButton
          , GR.colorCountSpinButton                = colorCountSpinButton
          , GR.leftCropSpinButton                  = leftCropSpinButton
          , GR.rightCropSpinButton                 = rightCropSpinButton
          , GR.topCropSpinButton                   = topCropSpinButton
          , GR.bottomCropSpinButton                = bottomCropSpinButton
          , GR.inFileChooserButton                 = inFileChooserButton
          , GR.inFileChooserDialogCancelButton     = inFileChooserDialogCancelButton
          , GR.inFileChooserDialogOpenButton       = inFileChooserDialogOpenButton
          , GR.outFileChooserDialogCancelButton    = outFileChooserDialogCancelButton
          , GR.outFileChooserDialogViewButton      = outFileChooserDialogViewButton
          , GR.outFileChooserDialogSaveButton      = outFileChooserDialogSaveButton
          , GR.outFileButton                       = outFileButton
          , GR.textOverlaysAddButton               = textOverlaysAddButton
          , GR.textOverlaysOpenButton              = textOverlaysOpenButton
          , GR.textOverlaysSaveButton              = textOverlaysSaveButton
          , GR.textOverlaysRemoveAllButton         = textOverlaysRemoveAllButton
          , GR.confirmMessageDialogYesButton       = confirmMessageDialogYesButton
          , GR.confirmMessageDialogNoButton        = confirmMessageDialogNoButton
          , GR.aboutButton                         = aboutButton
          , GR.aboutDialogCloseButton              = aboutDialogCloseButton
          , GR.giphyUploadButton                   = giphyUploadButton
          , GR.imgurUploadButton                   = imgurUploadButton
          , GR.outFileChooserDialogGifRadioButton  = outFileChooserDialogGifRadioButton
          , GR.fileSizeToggleButton                = fileSizeToggleButton
          , GR.cropToggleButton                    = cropToggleButton
          , GR.textOverlaysToggleButton            = textOverlaysToggleButton
          , GR.uploadToggleButton                  = uploadToggleButton
          , GR.videoPreviewPauseToggleButton       = videoPreviewPauseToggleButton
          , GR.ditherToggleButton                  = ditherToggleButton
          , GR.inFileChooserDialogLabel            = inFileChooserDialogLabel
          , GR.inFileChooserButtonLabel            = inFileChooserButtonLabel
          , GR.startTimeAdjustment                 = startTimeAdjustment
          , GR.endTimeAdjustment                   = endTimeAdjustment
          , GR.widthAdjustment                     = widthAdjustment
          , GR.fpsAdjustment                       = fpsAdjustment
          , GR.colorCountAdjustment                = colorCountAdjustment
          , GR.aboutDialogLabel                    = aboutDialogLabel
          , GR.statusLabel                         = statusLabel
          , GR.sidebarControlsPreviewbox           = sidebarControlsPreviewbox
          , GR.mainPreviewBox                      = mainPreviewBox
          , GR.imagesPreviewBox                    = imagesPreviewBox
          , GR.videoPreviewBox                     = videoPreviewBox
          , GR.videoPreviewOverlayChildBox         = videoPreviewOverlayChildBox
          , GR.fileSizeSpinButtonsGrid             = fileSizeSpinButtonsGrid
          , GR.cropSpinButtonsBox                  = cropSpinButtonsBox
          , GR.textOverlaysMainBox                 = textOverlaysMainBox
          , GR.textOverlaysBox                     = textOverlaysBox
          , GR.uploadBox                           = uploadBox
          , GR.videoPreviewDrawingArea             = videoPreviewDrawingArea
          , GR.timeSlicesDrawingArea               = timeSlicesDrawingArea
          , GR.firstFramePreviewImageDrawingArea   = firstFramePreviewImageDrawingArea
          , GR.lastFramePreviewImageDrawingArea    = lastFramePreviewImageDrawingArea
          , GR.inFileChooserButtonImage            = inFileChooserButtonImage
          , GR.firstFrameImage                     = firstFrameImage
          , GR.lastFrameImage                      = lastFrameImage
          , GR.inFileChooserDialog                 = inFileChooserDialog
          , GR.outFileChooserDialog                = outFileChooserDialog
          , GR.textOverlaysOpenDialog              = textOverlaysOpenDialog
          , GR.textOverlaysSaveDialog              = textOverlaysSaveDialog
          , GR.confirmMessageDialog                = confirmMessageDialog
          , GR.aboutDialog                         = aboutDialog
          , GR.inFileChooserWidget                 = inFileChooserWidget
          , GR.outFileChooserDialogGifFileFilter   = outFileChooserDialogGifFileFilter
          , GR.outFileChooserDialogVideoFileFilter = outFileChooserDialogVideoFileFilter
          , GR.maybeVideoPreviewWidget             = maybeVideoPreviewWidget
          , GR.maybePlaybinElement                 = maybePlaybinElement
          , GR.temporaryDirectory                  = temporaryDirectory
          , GR.guiInFilePropertiesRef              = guiInFilePropertiesRef
          , GR.textOverlaysRef                     = textOverlaysRef
          , GR.guiPreviewStateRef                  = guiPreviewStateRef
          }

  hideWidgetsOnRealize                      guiComponents
  handleSpinButtons                         guiComponents
  handleOutFileChooserDialogViewButtonClick guiComponents
  handleDialogs                             guiComponents
  handleSidebarSectionToggleButtons         guiComponents
  handleUploadButtons                       guiComponents
  handleWindow                              guiComponents
  handleGuiPreview                          guiComponents
  handleStatusLabelClick                    guiComponents
  handleAboutDialogLabelClick               guiComponents
  handleDitherToggleButtonClick             guiComponents
  handleOutFileChooserDialogGifRadioButton  guiComponents

  GuiTextOverlays.handleTextOverlaysControls guiComponents
  GuiStyle.applyCss                          guiComponents
  GuiCapabilities.checkCapabilitiesAndNotify guiComponents
  GuiKeyboard.addKeyboardEventHandler        guiComponents

  GI.Gtk.main

buildBuilder
  ::  IO GI.Gtk.Builder
buildBuilder = do
  gladeFile <- getDataFileName "data/gui.glade"
  GI.Gtk.builderNewFromFile (pack gladeFile)

builderGetObject
  ::  ( GI.GObject.GObject b
      , GI.GObject.GObject a
      , Data.GI.Base.Overloading.IsDescendantOf GI.Gtk.Builder a
      )
  =>  (Data.GI.Base.ManagedPtr b -> b)
  ->  a
  ->  String
  ->  IO b
builderGetObject objectTypeClass builder objectId = do
  maybeObject <- GI.Gtk.builderGetObject builder $ pack objectId
  when (isNothing maybeObject) $
    putStrLn $ "[ERROR] could not build " ++ objectId ++ "."
  GI.Gtk.unsafeCastTo objectTypeClass $ fromJust maybeObject

handleInFileChooserDialogReponse
  ::  GR.GuiComponents
  ->  Int32
  ->  IO ()
handleInFileChooserDialogReponse
  guiComponents@GR.GuiComponents
    { GR.sidebarControlsPreviewbox
    , GR.startTimeSpinButton
    , GR.endTimeSpinButton
    , GR.leftCropSpinButton
    , GR.rightCropSpinButton
    , GR.topCropSpinButton
    , GR.bottomCropSpinButton
    , GR.widthSpinButton
    , GR.fpsSpinButton
    , GR.colorCountSpinButton
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
    syncStartAndEndTimeSpinButtons guiComponents
  where
    maybeLoadFile :: IO ()
    maybeLoadFile = do
      maybeInFilePath <-
        fileChooserGetExistingFilePath inFileChooserWidget
      case maybeInFilePath of
        Just inFilePath -> do
          maybePlayableMetadata <-
            Gifcurry.getPlayableMetadata
              inFilePath
          case maybePlayableMetadata of
            Just
              Gifcurry.PlayableMetadata
                { Gifcurry.playableMetadataWidth
                , Gifcurry.playableMetadataHeight
                , Gifcurry.playableMetadataDuration
                , Gifcurry.playableMetadataFps
                }
              -> do
                let isGif = hasFileExtension inFilePath ".gif"
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
                atomicModifyIORef' guiInFilePropertiesRef
                  $ \ guiInFileProperties' ->
                    ( guiInFileProperties'
                        { GR.inFileUri      = if fileExists then inFilePath' else inFilePath
                        , GR.inFileFps      = playableMetadataFps
                        , GR.inFileDuration = playableMetadataDuration
                        , GR.inFileWidth    = playableMetadataWidth
                        , GR.inFileHeight   = playableMetadataHeight
                        }
                    , ()
                    )
                let startTimeFraction = 0.25
                let startTime         = playableMetadataDuration * startTimeFraction
                let endTime           = playableMetadataDuration * (1.0 - startTimeFraction)
                let durationText      = Data.Text.pack $ printf "%.3f" playableMetadataDuration
                GI.Gtk.spinButtonSetValue fpsSpinButton playableMetadataFps
                updateStartAndEndTimeSpinButtons
                  guiComponents
                  startTime
                  endTime
                updateStatusLabelAsync statusLabel 1
                  $ Data.Text.concat
                    [ "That "
                    , if isGif then "GIF" else "video"
                    , " is about "
                    , durationText
                    , " seconds long."
                    ]
                GI.Gtk.labelSetText inFileChooserButtonLabel
                  $ Data.Text.pack
                    $ takeFileName inFilePath
                GI.Gtk.widgetShow sidebarControlsPreviewbox
            _ -> resetOnFailure
        _ -> resetOnFailure
    resetOnFailure :: IO ()
    resetOnFailure = do
      atomicWriteIORef guiInFilePropertiesRef GR.defaultGuiInFileProperties
      GI.Gtk.widgetHide sidebarControlsPreviewbox
      updateStartAndEndTimeSpinButtonRanges guiComponents
      GI.Gtk.spinButtonSetValue startTimeSpinButton  0.0
      GI.Gtk.spinButtonSetValue endTimeSpinButton    0.0
      GI.Gtk.labelSetText inFileChooserButtonLabel "Open"
      resetSideBarControls
      updateStatusLabelAsync statusLabel 1 "Couldn't open that file."
    resetSideBarControls :: IO ()
    resetSideBarControls = do
      resetFileSizeSpinButtons
      resetCropSpinButtons
      resetGuiTextOverlays
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
    adaptGif
      ::  String
      ->  String
      ->  Double
      ->  IO ()
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
              outFilePath
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

handleSpinButtons
  ::  GR.GuiComponents
  ->  IO ()
handleSpinButtons
  guiComponents@GR.GuiComponents
    { GR.startTimeSpinButton
    , GR.endTimeSpinButton
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
    endTimeSpinButton
    handleEndTimeSpinButton
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
      void $ syncStartAndEndTimeSpinButtons guiComponents
    handleEndTimeSpinButton :: IO ()
    handleEndTimeSpinButton = do
      startTime    <- GI.Gtk.spinButtonGetValue startTimeSpinButton
      endTime      <- GI.Gtk.spinButtonGetValue endTimeSpinButton
      fileDuration <- inFileDuration
      _ <- GI.Gtk.setEntryProgressFraction endTimeSpinButton 0.5
      _ <- setSpinButtonFraction endTimeSpinButton
      if    endTime <= startTime
        ||  endTime > fileDuration
        then do
          GI.Gtk.labelSetText statusLabel "The end time is wrong."
          highlightSpinButton endTimeSpinButton
        else do
          GI.Gtk.labelSetText statusLabel "Ready."
          unhighlightSpinButton endTimeSpinButton
      void $ syncStartAndEndTimeSpinButtons guiComponents
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
    inFileDuration :: IO Double
    inFileDuration =
      GR.inFileDuration <$> readIORef guiInFilePropertiesRef

handleOutFileChooserDialogResponse
  ::  GR.GuiComponents
  ->  Int32
  ->  IO ()
handleOutFileChooserDialogResponse
  guiComponents@GR.GuiComponents
    { GR.outFileChooserDialog
    , GR.outFileChooserDialogGifRadioButton
    , GR.outFileButton
    , GR.ditherToggleButton
    , GR.startTimeSpinButton
    , GR.endTimeSpinButton
    , GR.widthSpinButton
    , GR.fpsSpinButton
    , GR.colorCountSpinButton
    , GR.leftCropSpinButton
    , GR.rightCropSpinButton
    , GR.topCropSpinButton
    , GR.bottomCropSpinButton
    , GR.statusLabel
    , GR.confirmMessageDialog
    , GR.guiInFilePropertiesRef
    }
  responseId
  = do
  GI.Gtk.widgetHide outFileChooserDialog
  when (enumToInt32 GI.Gtk.ResponseTypeOk == responseId)
    handleOutFileChooserDialogResponse'
  where
    handleOutFileChooserDialogResponse'
      ::  IO ()
    handleOutFileChooserDialogResponse'
      = do
      GR.GuiInFileProperties
        { GR.inFileUri = inFilePath
        }          <- readIORef guiInFilePropertiesRef
      startTime    <- GI.Gtk.spinButtonGetValue startTimeSpinButton
      endTime      <- GI.Gtk.spinButtonGetValue endTimeSpinButton
      width        <- GI.Gtk.spinButtonGetValue widthSpinButton
      fps          <- GI.Gtk.spinButtonGetValue fpsSpinButton
      colorCount   <- GI.Gtk.spinButtonGetValue colorCountSpinButton
      leftCrop     <- GI.Gtk.spinButtonGetValue leftCropSpinButton
      rightCrop    <- GI.Gtk.spinButtonGetValue rightCropSpinButton
      topCrop      <- GI.Gtk.spinButtonGetValue topCropSpinButton
      bottomCrop   <- GI.Gtk.spinButtonGetValue bottomCropSpinButton
      saveAsVideo  <- not <$> GI.Gtk.toggleButtonGetActive outFileChooserDialogGifRadioButton
      dither       <- GI.Gtk.toggleButtonGetActive ditherToggleButton
      outFilePath  <- fileChooserGetFilePath outFileChooserDialog
      textOverlays <- GuiTextOverlays.getGifcurryTextOverlays guiComponents True
      let params =
            Gifcurry.defaultGifParams
              { Gifcurry.inputFile    = inFilePath
              , Gifcurry.outputFile   = outFilePath
              , Gifcurry.saveAsVideo  = saveAsVideo
              , Gifcurry.startTime    = startTime
              , Gifcurry.endTime      = endTime
              , Gifcurry.width        = truncate width
              , Gifcurry.fps          = truncate fps
              , Gifcurry.colorCount   = truncate colorCount
              , Gifcurry.dither       = dither
              , Gifcurry.leftCrop     = leftCrop
              , Gifcurry.rightCrop    = rightCrop
              , Gifcurry.topCrop      = topCrop
              , Gifcurry.bottomCrop   = bottomCrop
              , Gifcurry.textOverlays = textOverlays
              }
      paramsValid <- Gifcurry.validateGifParams params
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
            if endTime - startTime >= durationTimeWarningLevel
              then GI.Gtk.dialogRun confirmMessageDialog
              else return (enumToInt32 GI.Gtk.ResponseTypeYes)
          when (confirmMessageDialogResponse == enumToInt32 GI.Gtk.ResponseTypeYes) $ do
            GI.Gtk.widgetSetSensitive outFileButton False
            void
              $ GI.Gtk.fileChooserSetCurrentFolder
                outFileChooserDialog
                (takeDirectory outFilePath)
            void $ forkOS $ do
              GtkMainSyncAsync.gtkMainAsync $
                GI.Gtk.labelSetText
                  statusLabel $
                    Data.Text.concat
                      [ "One "
                      , if saveAsVideo then "video" else "GIF"
                      , " coming up!"
                      ]
              result <- Gifcurry.createGif params
              case result of
                Left _ ->
                  GtkMainSyncAsync.gtkMainAsync $
                    GI.Gtk.labelSetMarkup
                      statusLabel $
                        Data.Text.concat
                          [ "Didn't work. Check your settings. "
                          , "If you think it's a bug, please open an "
                          , "<a href=\"https://github.com/lettier/gifcurry/issues\">issue</a>"
                          , "."
                          ]
                Right filePath -> do
                  _ <-
                    forkOS
                      $ GtkMainSyncAsync.gtkMainAsync
                        $ openLocalFileWithDefaultProgram filePath
                  GtkMainSyncAsync.gtkMainAsync
                    $ GI.Gtk.labelSetText statusLabel "Ready."
              GtkMainSyncAsync.gtkMainAsync
                $ GI.Gtk.widgetSetSensitive outFileButton True
        else GI.Gtk.labelSetText statusLabel "The settings are wrong."

handleOutFileChooserDialogViewButtonClick
  ::  GR.GuiComponents
  ->  IO ()
handleOutFileChooserDialogViewButtonClick
  GR.GuiComponents
    { GR.outFileChooserDialogViewButton
    , GR.outFileChooserDialog
    , GR.statusLabel
    }
  =
  void
    $ GI.Gtk.onWidgetButtonReleaseEvent
      outFileChooserDialogViewButton
      $ \ _ -> do
        maybeTextOverlaysFilePath <-
          fileChooserGetExistingFilePath outFileChooserDialog
        case maybeTextOverlaysFilePath of
          Just filePath -> do
            GI.Gtk.labelSetText
              statusLabel
              "One second..."
            void
              $ forkIO
                $ GtkMainSyncAsync.gtkMainAsync
                  $ openLocalFileWithDefaultProgram filePath
          Nothing ->
            GI.Gtk.labelSetText
              statusLabel
              "Couldn't find the file. Check your settings."
        updateStatusLabelAsync statusLabel 1000 "Ready."
        return True

handleDialogs
  ::  GR.GuiComponents
  ->  IO ()
handleDialogs
  guiComponents@GR.GuiComponents
    { GR.inFileChooserDialog
    , GR.outFileChooserDialog
    , GR.aboutDialog
    , GR.confirmMessageDialog
    , GR.aboutButton
    , GR.inFileChooserButton
    , GR.outFileButton
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
  _ <- GI.Gtk.onWidgetButtonReleaseEvent
    inFileChooserButton
    (\ _ -> GI.Gtk.dialogRun inFileChooserDialog >> return True)
  _ <- GI.Gtk.onDialogResponse
    inFileChooserDialog
    (handleInFileChooserDialogReponse guiComponents)
  _ <- GI.Gtk.onWidgetButtonReleaseEvent
    outFileButton
    (\ _ -> handleOutFileButtonClicked guiComponents)
  _ <- GI.Gtk.onDialogResponse
    outFileChooserDialog
    (handleOutFileChooserDialogResponse guiComponents)
  _ <- GI.Gtk.afterFileChooserSelectionChanged
    outFileChooserDialog
    (handleOutFileChooserDialogSelectionChanged guiComponents)
  return ()

handleOutFileButtonClicked
  ::  GR.GuiComponents
  ->  IO Bool
handleOutFileButtonClicked
  GR.GuiComponents
    { GR.outFileChooserDialog
    , GR.fileSizeToggleButton
    , GR.cropToggleButton
    , GR.textOverlaysToggleButton
    , GR.uploadToggleButton
    , GR.fileSizeSpinButtonsGrid
    , GR.cropSpinButtonsBox
    , GR.textOverlaysMainBox
    , GR.uploadBox
    }
  = do
    mapM_
      (\ (t, w) -> w >>= \ w' -> hideSidebarSection t w')
      [ (fileSizeToggleButton,     GI.Gtk.unsafeCastTo GI.Gtk.Widget fileSizeSpinButtonsGrid)
      , (cropToggleButton,         GI.Gtk.unsafeCastTo GI.Gtk.Widget cropSpinButtonsBox)
      , (textOverlaysToggleButton, GI.Gtk.unsafeCastTo GI.Gtk.Widget textOverlaysMainBox)
      , (uploadToggleButton,       GI.Gtk.unsafeCastTo GI.Gtk.Widget uploadBox)
      ]
    void $ GI.Gtk.dialogRun outFileChooserDialog
    return True
  where
    hideSidebarSection
      ::  ( GI.GLib.GObject a
          , Data.GI.Base.Overloading.IsDescendantOf GI.Gtk.Widget a
          )
      =>  GI.Gtk.ToggleButton
      ->  a
      ->  IO ()
    hideSidebarSection
      toggleButton
      box
      = do
      active <- GI.Gtk.getToggleButtonActive toggleButton
      when active $ do
        GI.Gtk.widgetHide box
        GI.Gtk.setToggleButtonActive toggleButton False

handleOutFileChooserDialogSelectionChanged
  ::  GR.GuiComponents
  ->  IO ()
handleOutFileChooserDialogSelectionChanged
  GR.GuiComponents
    { GR.outFileChooserDialog
    , GR.outFileChooserDialogViewButton
    , GR.outFileChooserDialogGifRadioButton
    }
  = do
  maybeTextOverlaysFilePath <-
    fileChooserGetExistingFilePath outFileChooserDialog
  saveAsVideo <-
    not <$> GI.Gtk.getToggleButtonActive outFileChooserDialogGifRadioButton
  let sensitive =
        case maybeTextOverlaysFilePath of
          Just filePath -> do
            let filePath' =
                  Gifcurry.getOutputFileWithExtension
                    Gifcurry.defaultGifParams
                      { Gifcurry.outputFile  = filePath
                      , Gifcurry.saveAsVideo = saveAsVideo
                      }
            filePath == filePath'
          Nothing ->
            False
  GI.Gtk.widgetSetSensitive outFileChooserDialogViewButton sensitive

-- GI.Gtk.Expander does not work.
-- While it visually hides its children, they are still present
-- in terms of activation. This prevents the children of
-- other expanded sections from working.
-- This workaround emulates the expander functionality.
handleSidebarSectionToggleButtons
  ::  GR.GuiComponents
  ->  IO ()
handleSidebarSectionToggleButtons
  GR.GuiComponents
    { GR.fileSizeToggleButton
    , GR.cropToggleButton
    , GR.textOverlaysToggleButton
    , GR.uploadToggleButton
    , GR.fileSizeSpinButtonsGrid
    , GR.cropSpinButtonsBox
    , GR.textOverlaysMainBox
    , GR.uploadBox
    }
  =
  handleToggles
    [ fileSizeToggleButton
    , cropToggleButton
    , textOverlaysToggleButton
    , uploadToggleButton
    ]
    [ GI.Gtk.unsafeCastTo GI.Gtk.Widget fileSizeSpinButtonsGrid
    , GI.Gtk.unsafeCastTo GI.Gtk.Widget cropSpinButtonsBox
    , GI.Gtk.unsafeCastTo GI.Gtk.Widget textOverlaysMainBox
    , GI.Gtk.unsafeCastTo GI.Gtk.Widget uploadBox
    ]
  where
    handleToggles
      ::  ( GI.GLib.GObject a
          , Data.GI.Base.Overloading.IsDescendantOf GI.Gtk.Widget a
          )
      =>  [GI.Gtk.ToggleButton]
      ->  [IO a]
      ->  IO ()
    handleToggles
      toggleButtons
      ioWidgets
      =
      mapM_
        (\ (toggleButton, ioWidget) ->
          void
            $ GI.Gtk.onToggleButtonToggled toggleButton $
              handleOnToggle
                toggleButton
                ioWidget
                toggleButtons
        )
        $ Data.List.zip toggleButtons ioWidgets
    handleOnToggle
      ::  ( GI.GLib.GObject a
          , Data.GI.Base.Overloading.IsDescendantOf GI.Gtk.Widget a
          )
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

handleUploadButtons
  ::  GR.GuiComponents
  ->  IO ()
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

handleGuiPreview
  ::  GR.GuiComponents
  ->  IO ()
handleGuiPreview = GuiPreview.runGuiPreview

handleWindow
  ::  GR.GuiComponents
  ->  IO ()
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

openLocalFileWithDefaultProgram
  ::  String
  ->  IO ()
openLocalFileWithDefaultProgram filePath = do
  fileExists <- doesFileExist filePath
  when fileExists $ openUriWithDefaultProgram filePath

openUriWithDefaultProgram
  ::  String
  ->  IO ()
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

highlightSpinButton
  ::  GI.Gtk.SpinButton
  ->  IO ()
highlightSpinButton =
  styleSpinButtonAndEntry
    "{ background-color: #eb3b5a; color: white; }"

unhighlightSpinButton
  ::  GI.Gtk.SpinButton
  ->  IO ()
unhighlightSpinButton = styleSpinButtonAndEntry "{}"

styleSpinButtonAndEntry
  ::  String
  ->  GI.Gtk.SpinButton
  ->  IO ()
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

setSpinButtonFraction
  ::  GI.Gtk.SpinButton
  ->  IO ()
setSpinButtonFraction spinButton = do
  (_, maxValue) <- GI.Gtk.spinButtonGetRange spinButton
  value         <- GI.Gtk.spinButtonGetValue spinButton
  let fraction  = if maxValue <= 0.0 then 0.0 else abs $ value / maxValue
  void $ GI.Gtk.setEntryProgressFraction spinButton $ truncatePastDigit fraction 2

updateStartAndEndTimeSpinButtonRanges
  ::  GR.GuiComponents
  ->  IO ()
updateStartAndEndTimeSpinButtonRanges
  GR.GuiComponents
    { GR.startTimeSpinButton
    , GR.endTimeSpinButton
    , GR.guiInFilePropertiesRef
    }
  = do
  startTime           <- GI.Gtk.spinButtonGetValue startTimeSpinButton
  fileDuration        <- GR.inFileDuration <$> readIORef guiInFilePropertiesRef
  let buffer          = if fileDuration * 0.01 > 0.1 then 0.1 else fileDuration * 0.01
  GI.Gtk.spinButtonSetRange startTimeSpinButton 0.0                  (fileDuration - buffer)
  GI.Gtk.spinButtonSetRange endTimeSpinButton   (startTime + buffer) fileDuration

syncStartAndEndTimeSpinButtons
  ::  GR.GuiComponents
  ->  IO ()
syncStartAndEndTimeSpinButtons
  guiComponents@GR.GuiComponents
    { GR.startTimeSpinButton
    , GR.endTimeSpinButton
    , GR.guiInFilePropertiesRef
    }
  = do
  startTime           <- GI.Gtk.spinButtonGetValue startTimeSpinButton
  endTime             <- GI.Gtk.spinButtonGetValue endTimeSpinButton
  fileDuration        <- GR.inFileDuration <$> readIORef guiInFilePropertiesRef
  let startTime'      = if startTime >= fileDuration then fileDuration else startTime
  let endTime'        = if endTime   <= startTime'   then startTime'   else endTime
  let endTime''       = if endTime'  >= fileDuration then fileDuration else endTime'
  updateStartAndEndTimeSpinButtons
    guiComponents
    startTime'
    endTime''

updateStartAndEndTimeSpinButtons
  ::  GR.GuiComponents
  ->  Double
  ->  Double
  ->  IO ()
updateStartAndEndTimeSpinButtons
  guiComponents@GR.GuiComponents
    { GR.startTimeSpinButton
    , GR.endTimeSpinButton
    }
  startTime
  endTime
  = do
  updateStartAndEndTimeSpinButtonRanges guiComponents
  GI.Gtk.spinButtonSetValue startTimeSpinButton $ truncatePastDigit startTime 3
  GI.Gtk.spinButtonSetValue endTimeSpinButton   $ truncatePastDigit endTime   3
  updateStartAndEndTimeAdjustments         guiComponents
  updateStartAndEndTimeSpinButtonFractions guiComponents

updateStartAndEndTimeAdjustments
  ::  GR.GuiComponents
  -> IO ()
updateStartAndEndTimeAdjustments
  GR.GuiComponents
    { GR.startTimeAdjustment
    , GR.endTimeAdjustment
    , GR.guiInFilePropertiesRef
    }
  = do
  inFileDuration <- GR.inFileDuration <$> readIORef guiInFilePropertiesRef
  let fraction = inFileDuration / 100.0
  let step     = if fraction < 0.1 then 0.1 else fraction
  GI.Gtk.adjustmentSetStepIncrement startTimeAdjustment step
  GI.Gtk.adjustmentSetStepIncrement endTimeAdjustment   step

updateStartAndEndTimeSpinButtonFractions
  ::  GR.GuiComponents
  ->  IO ()
updateStartAndEndTimeSpinButtonFractions
  GR.GuiComponents
    { GR.startTimeSpinButton
    , GR.endTimeSpinButton
    }
  = do
  setSpinButtonFraction startTimeSpinButton
  setSpinButtonFraction endTimeSpinButton

hideWidgetsOnRealize
  ::  GR.GuiComponents
  ->  IO ()
hideWidgetsOnRealize
  GR.GuiComponents
    { GR.sidebarControlsPreviewbox
    , GR.fileSizeSpinButtonsGrid
    , GR.cropSpinButtonsBox
    , GR.textOverlaysMainBox
    , GR.uploadBox
    }
  = do
  let widgets =
        [ GI.Gtk.unsafeCastTo GI.Gtk.Widget sidebarControlsPreviewbox
        , GI.Gtk.unsafeCastTo GI.Gtk.Widget fileSizeSpinButtonsGrid
        , GI.Gtk.unsafeCastTo GI.Gtk.Widget cropSpinButtonsBox
        , GI.Gtk.unsafeCastTo GI.Gtk.Widget textOverlaysMainBox
        , GI.Gtk.unsafeCastTo GI.Gtk.Widget uploadBox
        ]
  mapM_ hideOnRealize widgets
  where
    hideOnRealize
      ::  ( GI.GLib.GObject a
          , Data.GI.Base.Overloading.IsDescendantOf GI.Gtk.Widget a
          )
      =>  IO a
      ->  IO ()
    hideOnRealize ioWidget = do
      widget <- ioWidget
      void
        $ GI.Gtk.onWidgetRealize widget
          $ GI.Gtk.widgetHide widget

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
  ->  IO ()
handleLabelClick
  label
  =
  void
    $ GI.Gtk.onLabelActivateLink
      label
        $ \ uri -> do
          openUriWithDefaultProgram
            (Data.Text.unpack uri)
          return True

handleDitherToggleButtonClick
  ::  GR.GuiComponents
  ->  IO ()
handleDitherToggleButtonClick
  GR.GuiComponents
    { GR.ditherToggleButton
    }
  =
  void
    $ GI.Gtk.afterToggleButtonToggled
      ditherToggleButton
      $ toggleToggleButtonLabel
        ditherToggleButton
        " Dithered"
        " Dither"
        "Turn dither off?"
        "Turn dither on?"

handleOutFileChooserDialogGifRadioButton
  ::  GR.GuiComponents
  ->  IO ()
handleOutFileChooserDialogGifRadioButton
  GR.GuiComponents
    { GR.outFileChooserDialogGifRadioButton
    , GR.outFileChooserDialog
    , GR.outFileChooserDialogGifFileFilter
    , GR.outFileChooserDialogVideoFileFilter
    }
  =
  void
    $ GI.Gtk.afterToggleButtonToggled
      outFileChooserDialogGifRadioButton
      $ do
        active <-
          GI.Gtk.getToggleButtonActive
            outFileChooserDialogGifRadioButton
        if active
          then
            GI.Gtk.fileChooserSetFilter
              outFileChooserDialog
              outFileChooserDialogGifFileFilter
          else
            GI.Gtk.fileChooserSetFilter
              outFileChooserDialog
              outFileChooserDialogVideoFileFilter

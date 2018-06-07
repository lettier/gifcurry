{-
  Gifcurry
  (C) 2016 David Lettier
  lettier.com
-}

{-# LANGUAGE
    OverloadedStrings
  , NamedFieldPuns
#-}

import GHC.Float
import System.Directory
import System.FilePath
import System.Process
import Text.Printf
import Data.Maybe
import Data.Text
import Data.List
import Data.IORef
import Data.Word
import Data.Int
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
  ( gif
  , GifParams(..)
  , Quality(QualityMedium)
  , defaultGifParams
  , gifParamsValid
  , getVideoDurationInSeconds
  , getOutputFileWithExtension
  , getVideoWidthAndHeight
  , findOrCreateTemporaryDirectory
  , qualityFromString
  )
import qualified GtkMainSyncAsync (gtkMainAsync)
import qualified GuiRecords as GR
import qualified GuiCapabilities
import qualified GuiStyle
import qualified GuiTextOverlays
import qualified GuiPreview
import GuiMisc

durationTimeWarningLevel :: Float
durationTimeWarningLevel = 10.0

main :: IO ()
main = do
  _ <- GI.Gst.init Nothing
  _ <- GI.Gtk.init Nothing

  builder <- buildBuilder

  window                            <- builderGetObject GI.Gtk.Window            builder "gifcurry-window"
  startTimeSpinButton               <- builderGetObject GI.Gtk.SpinButton        builder "start-time-spin-button"
  durationTimeSpinButton            <- builderGetObject GI.Gtk.SpinButton        builder "duration-time-spin-button"
  widthSizeSpinButton               <- builderGetObject GI.Gtk.SpinButton        builder "width-size-spin-button"
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
  giphyUploadButton                 <- builderGetObject GI.Gtk.Button            builder "giphy-upload-button"
  imgurUploadButton                 <- builderGetObject GI.Gtk.Button            builder "imgur-upload-button"
  saveAsVideoRadioButton            <- builderGetObject GI.Gtk.RadioButton       builder "save-as-video-radio-button"
  widthQualityToggleButton          <- builderGetObject GI.Gtk.ToggleButton      builder "width-quality-toggle-button"
  cropToggleButton                  <- builderGetObject GI.Gtk.ToggleButton      builder "crop-toggle-button"
  textOverlaysToggleButton          <- builderGetObject GI.Gtk.ToggleButton      builder "text-overlays-toggle-button"
  saveOpenToggleButton              <- builderGetObject GI.Gtk.ToggleButton      builder "save-open-toggle-button"
  uploadToggleButton                <- builderGetObject GI.Gtk.ToggleButton      builder "upload-toggle-button"
  videoPreviewPauseToggleButton     <- builderGetObject GI.Gtk.ToggleButton      builder "video-preview-pause-toggle-button"
  inFileChooserDialogLabel          <- builderGetObject GI.Gtk.Label             builder "in-file-chooser-dialog-label"
  inFileChooserButtonLabel          <- builderGetObject GI.Gtk.Label             builder "in-file-chooser-button-label"
  startTimeAdjustment               <- builderGetObject GI.Gtk.Adjustment        builder "start-time-adjustment"
  durationTimeAdjustment            <- builderGetObject GI.Gtk.Adjustment        builder "duration-time-adjustment"
  widthSizeAdjustment               <- builderGetObject GI.Gtk.Adjustment        builder "width-size-adjustment"
  outFileNameEntry                  <- builderGetObject GI.Gtk.Entry             builder "out-file-name-entry"
  statusEntry                       <- builderGetObject GI.Gtk.Entry             builder "status-entry"
  sidebarControlsPreviewbox         <- builderGetObject GI.Gtk.Box               builder "sidebar-controls-preview-box"
  mainPreviewBox                    <- builderGetObject GI.Gtk.Box               builder "main-preview-box"
  imagesPreviewBox                  <- builderGetObject GI.Gtk.Box               builder "images-preview-box"
  videoPreviewBox                   <- builderGetObject GI.Gtk.Box               builder "video-preview-box"
  videoPreviewOverlayChildBox       <- builderGetObject GI.Gtk.Box               builder "video-preview-overlay-child-box"
  widthQualityBox                   <- builderGetObject GI.Gtk.Box               builder "width-quality-box"
  cropSpinButtonsBox                <- builderGetObject GI.Gtk.Box               builder "crop-spin-buttons-box"
  textOverlaysMainBox               <- builderGetObject GI.Gtk.Box               builder "text-overlays-main-box"
  textOverlaysBox                   <- builderGetObject GI.Gtk.Box               builder "text-overlays-box"
  saveOpenBox                       <- builderGetObject GI.Gtk.Box               builder "save-open-box"
  uploadBox                         <- builderGetObject GI.Gtk.Box               builder "upload-box"
  qualityComboBoxText               <- builderGetObject GI.Gtk.ComboBoxText      builder "quality-combo-box-text"
  videoPreviewDrawingArea           <- builderGetObject GI.Gtk.DrawingArea       builder "video-preview-drawing-area"
  timeSlicesDrawingArea             <- builderGetObject GI.Gtk.DrawingArea       builder "time-slices-drawing-area"
  firstFramePreviewImageDrawingArea <- builderGetObject GI.Gtk.DrawingArea       builder "first-frame-preview-image-drawing-area"
  lastFramePreviewImageDrawingArea  <- builderGetObject GI.Gtk.DrawingArea       builder "last-frame-preview-image-drawing-area"
  inFileChooserButtonImage          <- builderGetObject GI.Gtk.Image             builder "in-file-chooser-button-image"
  firstFrameImage                   <- builderGetObject GI.Gtk.Image             builder "first-frame-image"
  lastFrameImage                    <- builderGetObject GI.Gtk.Image             builder "last-frame-image"
  inFileChooserDialog               <- builderGetObject GI.Gtk.Dialog            builder "in-file-chooser-dialog"
  confirmMessageDialog              <- builderGetObject GI.Gtk.MessageDialog     builder "confirm-message-dialog"
  aboutDialog                       <- builderGetObject GI.Gtk.AboutDialog       builder "about-dialog"
  saveSpinner                       <- builderGetObject GI.Gtk.Spinner           builder "save-spinner"
  inFileChooserWidget               <- builderGetObject GI.Gtk.FileChooserWidget builder "in-file-chooser-widget"

  -- Glade does not allow us to use the response ID nicknames so we set them here.
  GI.Gtk.dialogAddActionWidget confirmMessageDialog confirmMessageDialogYesButton  $ enumToInt32 GI.Gtk.ResponseTypeYes
  GI.Gtk.dialogAddActionWidget confirmMessageDialog confirmMessageDialogNoButton   $ enumToInt32 GI.Gtk.ResponseTypeNo
  GI.Gtk.dialogAddActionWidget inFileChooserDialog inFileChooserDialogCancelButton $ enumToInt32 GI.Gtk.ResponseTypeCancel
  GI.Gtk.dialogAddActionWidget inFileChooserDialog inFileChooserDialogOpenButton   $ enumToInt32 GI.Gtk.ResponseTypeOk

  (maybeVideoPreviewWidget, maybePlaybinElement) <-
    GuiPreview.buildVideoPreviewWidgetAndPlaybinElement

  temporaryDirectory <- Gifcurry.findOrCreateTemporaryDirectory

  inVideoPropertiesRef <- newIORef GR.defaultInVideoProperties
  textOverlaysRef      <- newIORef []
  guiPreviewStateRef   <- newIORef GR.defaultGuiPreviewState

  let guiComponents =
        GR.GuiComponents
          { GR.window                            = window
          , GR.startTimeSpinButton               = startTimeSpinButton
          , GR.durationTimeSpinButton            = durationTimeSpinButton
          , GR.widthSizeSpinButton               = widthSizeSpinButton
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
          , GR.giphyUploadButton                 = giphyUploadButton
          , GR.imgurUploadButton                 = imgurUploadButton
          , GR.saveAsVideoRadioButton            = saveAsVideoRadioButton
          , GR.widthQualityToggleButton          = widthQualityToggleButton
          , GR.cropToggleButton                  = cropToggleButton
          , GR.textOverlaysToggleButton          = textOverlaysToggleButton
          , GR.saveOpenToggleButton              = saveOpenToggleButton
          , GR.uploadToggleButton                = uploadToggleButton
          , GR.videoPreviewPauseToggleButton     = videoPreviewPauseToggleButton
          , GR.inFileChooserDialogLabel          = inFileChooserDialogLabel
          , GR.inFileChooserButtonLabel          = inFileChooserButtonLabel
          , GR.startTimeAdjustment               = startTimeAdjustment
          , GR.durationTimeAdjustment            = durationTimeAdjustment
          , GR.widthSizeAdjustment               = widthSizeAdjustment
          , GR.outFileNameEntry                  = outFileNameEntry
          , GR.statusEntry                       = statusEntry
          , GR.sidebarControlsPreviewbox         = sidebarControlsPreviewbox
          , GR.mainPreviewBox                    = mainPreviewBox
          , GR.imagesPreviewBox                  = imagesPreviewBox
          , GR.videoPreviewBox                   = videoPreviewBox
          , GR.videoPreviewOverlayChildBox       = videoPreviewOverlayChildBox
          , GR.widthQualityBox                   = widthQualityBox
          , GR.cropSpinButtonsBox                = cropSpinButtonsBox
          , GR.textOverlaysMainBox               = textOverlaysMainBox
          , GR.textOverlaysBox                   = textOverlaysBox
          , GR.saveOpenBox                       = saveOpenBox
          , GR.uploadBox                         = uploadBox
          , GR.qualityComboBoxText               = qualityComboBoxText
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
          , GR.inVideoPropertiesRef              = inVideoPropertiesRef
          , GR.textOverlaysRef                   = textOverlaysRef
          , GR.guiPreviewStateRef                = guiPreviewStateRef
          }

  _ <- hideWidgetsOnRealize guiComponents

  _ <- handleSpinButtons guiComponents
  _ <- handleSaveButtonClick guiComponents
  _ <- handleOpenButtonClick guiComponents
  _ <- handleDialogs guiComponents
  _ <- handleSidebarSectionToggleButtons guiComponents
  _ <- handleUploadButtons guiComponents
  _ <- handleWindow guiComponents
  _ <- handleGuiPreview guiComponents

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
    , GR.widthSizeSpinButton
    , GR.qualityComboBoxText
    , GR.outFileNameEntry
    , GR.inFileChooserDialog
    , GR.statusEntry
    , GR.inFileChooserButtonLabel
    , GR.inFileChooserWidget
    , GR.inVideoPropertiesRef
    }
  responseId
  = do
  GI.Gtk.widgetHide inFileChooserDialog
  when (enumToInt32 GI.Gtk.ResponseTypeOk == responseId) $ do
    inFilePath <- fileChooserGetFilePath inFileChooserWidget
    maybeVideoDuration <-
      Gifcurry.getVideoDurationInSeconds
        Gifcurry.defaultGifParams
          { Gifcurry.inputFile = inFilePath }
    maybeWidthHeight <-
      Gifcurry.getVideoWidthAndHeight
        Gifcurry.defaultGifParams
          { Gifcurry.inputFile = inFilePath }
    case (maybeVideoDuration, maybeWidthHeight) of
      (Just videoDuration', Just (width, height)) -> do
        atomicWriteIORef inVideoPropertiesRef $
          GR.defaultInVideoProperties
            { GR.inVideoUri      = inFilePath
            , GR.inVideoDuration = videoDuration'
            , GR.inVideoWidth    = width
            , GR.inVideoHeight   = height
            }
        let videoDuration     = floatToDouble videoDuration'
        let startTimeFraction = 0.25
        let startTime         = videoDuration * startTimeFraction
        let endTime           = startTime * 3
        let durationTime      = endTime - startTime
        let videoDurationText = Data.Text.pack $ printf "%.3f" videoDuration
        _ <- updateStartAndDurationTimeSpinButtonRanges guiComponents
        _ <- GI.Gtk.spinButtonSetValue startTimeSpinButton    $ truncatePastDigit startTime    2
        _ <- GI.Gtk.spinButtonSetValue durationTimeSpinButton $ truncatePastDigit durationTime 2
        _ <- updateStatusEntryAsync statusEntry 1 $
          Data.Text.concat
            [ "That video is about "
            , videoDurationText
            , " seconds long."
            ]
        GI.Gtk.labelSetText inFileChooserButtonLabel $
          Data.Text.pack $
            takeFileName inFilePath
        _ <- GI.Gtk.widgetShow sidebarControlsPreviewbox
        return ()
      _ -> do
        atomicWriteIORef inVideoPropertiesRef GR.defaultInVideoProperties
        _ <- GI.Gtk.widgetHide sidebarControlsPreviewbox
        _ <- updateStartAndDurationTimeSpinButtonRanges guiComponents
        _ <- GI.Gtk.spinButtonSetValue startTimeSpinButton 0.0
        _ <- GI.Gtk.spinButtonSetValue durationTimeSpinButton 0.0
        _ <- updateStatusEntryAsync statusEntry 1 "Is that a video?"
        GI.Gtk.labelSetText inFileChooserButtonLabel "Open"
        return ()
    syncStartAndDurationTimeSpinButtons guiComponents
    resetTextEntries
    resetWidthAndQuality
    resetCropSpinButtons
    GuiTextOverlays.removeTextOverlays guiComponents
  where
    resetTextEntries :: IO ()
    resetTextEntries = do
      let textEntries =
            [ outFileNameEntry
            ]
      mapM_
        (`GI.Gtk.entrySetText` "")
        textEntries
    resetWidthAndQuality :: IO ()
    resetWidthAndQuality = do
      GI.Gtk.spinButtonSetValue  widthSizeSpinButton 500
      GI.Gtk.setComboBoxActiveId qualityComboBoxText "Medium"
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

handleSpinButtons :: GR.GuiComponents -> IO ()
handleSpinButtons
  guiComponents@GR.GuiComponents
    { GR.startTimeSpinButton
    , GR.durationTimeSpinButton
    , GR.widthSizeSpinButton
    , GR.leftCropSpinButton
    , GR.rightCropSpinButton
    , GR.topCropSpinButton
    , GR.bottomCropSpinButton
    , GR.statusEntry
    , GR.inVideoPropertiesRef
    }
  = do
  _ <- GI.Gtk.onSpinButtonValueChanged
    startTimeSpinButton
    handleStartTimeSpinButton
  _ <- GI.Gtk.onSpinButtonValueChanged
    durationTimeSpinButton
    handleDurationTimeSpinButton
  _ <- GI.Gtk.onSpinButtonValueChanged
    widthSizeSpinButton
    handleWidthSizeSpinButton
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
      startTime <- double2Float <$> GI.Gtk.spinButtonGetValue startTimeSpinButton
      _ <- setSpinButtonFraction startTimeSpinButton
      if startTime < 0.0
        then do
          GI.Gtk.entrySetText statusEntry "The start time is wrong."
          highlightSpinButton startTimeSpinButton
        else do
          GI.Gtk.entrySetText statusEntry "Ready."
          unhighlightSpinButton startTimeSpinButton
      void $ syncStartAndDurationTimeSpinButtons guiComponents
    handleDurationTimeSpinButton :: IO ()
    handleDurationTimeSpinButton = do
      startTime <- double2Float <$> GI.Gtk.spinButtonGetValue startTimeSpinButton
      durationTime <- double2Float <$> GI.Gtk.spinButtonGetValue durationTimeSpinButton
      videoDuration <- inVideoDuration
      _ <- GI.Gtk.setEntryProgressFraction durationTimeSpinButton 0.5
      _ <- setSpinButtonFraction durationTimeSpinButton
      if
           durationTime < 0.0
        -- 2.1 > 10.2 - 8.1
        || (10.0 * durationTime) > ((10.0 * videoDuration) - (10.0 * startTime))
        || durationTime > videoDuration
        then do
          GI.Gtk.entrySetText statusEntry "The duration time is wrong."
          highlightSpinButton durationTimeSpinButton
        else do
          GI.Gtk.entrySetText statusEntry "Ready."
          unhighlightSpinButton durationTimeSpinButton
      void $ syncStartAndDurationTimeSpinButtons guiComponents
    handleWidthSizeSpinButton :: IO ()
    handleWidthSizeSpinButton = do
      widthSize <- double2Float <$> GI.Gtk.spinButtonGetValue widthSizeSpinButton
      _ <- setSpinButtonFraction widthSizeSpinButton
      if widthSize <= 0.0
        then do
          GI.Gtk.entrySetText statusEntry "The width size is wrong."
          highlightSpinButton widthSizeSpinButton
        else do
          GI.Gtk.entrySetText statusEntry "Ready."
          unhighlightSpinButton widthSizeSpinButton
    handleCropSpinButton :: GI.Gtk.SpinButton -> GI.Gtk.SpinButton -> Text -> IO ()
    handleCropSpinButton a b t = do
      cropValue <- double2Float <$> GI.Gtk.spinButtonGetValue a
      _ <- setSpinButtonFraction a
      if cropValue < 0.0 || cropValue >= 1.0
        then do
          GI.Gtk.entrySetText statusEntry $ Data.Text.concat ["The ", t, " crop is wrong."]
          highlightSpinButton a
        else do
          GI.Gtk.entrySetText statusEntry "Ready."
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
    inVideoDuration :: IO Float
    inVideoDuration =
      GR.inVideoDuration <$> readIORef inVideoPropertiesRef

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
    , GR.widthSizeSpinButton
    , GR.qualityComboBoxText
    , GR.leftCropSpinButton
    , GR.rightCropSpinButton
    , GR.topCropSpinButton
    , GR.bottomCropSpinButton
    , GR.statusEntry
    , GR.confirmMessageDialog
    , GR.saveSpinner
    , GR.inVideoPropertiesRef
    }
  =
  void $ GI.Gtk.onWidgetButtonReleaseEvent saveButton $ \ _ -> do
    GR.InVideoProperties
      { GR.inVideoUri = inFilePath
      }            <- readIORef inVideoPropertiesRef
    startTime      <- double2Float <$> GI.Gtk.spinButtonGetValue startTimeSpinButton
    durationTime   <- double2Float <$> GI.Gtk.spinButtonGetValue durationTimeSpinButton
    widthSize      <- double2Float <$> GI.Gtk.spinButtonGetValue widthSizeSpinButton
    quality        <- getQuality
    leftCrop       <- double2Float <$> GI.Gtk.spinButtonGetValue leftCropSpinButton
    rightCrop      <- double2Float <$> GI.Gtk.spinButtonGetValue rightCropSpinButton
    topCrop        <- double2Float <$> GI.Gtk.spinButtonGetValue topCropSpinButton
    bottomCrop     <- double2Float <$> GI.Gtk.spinButtonGetValue bottomCropSpinButton
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
            , Gifcurry.widthSize      = truncate widthSize
            , Gifcurry.quality        = quality
            , Gifcurry.leftCrop       = leftCrop
            , Gifcurry.rightCrop      = rightCrop
            , Gifcurry.topCrop        = topCrop
            , Gifcurry.bottomCrop     = bottomCrop
            , Gifcurry.textOverlays   = textOverlays
            }
    paramsValid <- Gifcurry.gifParamsValid params
    GI.Gtk.entrySetText statusEntry "Ready."
    if paramsValid
      then do
        GI.Gtk.setMessageDialogText
          confirmMessageDialog
          "Create a GIF with that long of a duration?"
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
              GI.Gtk.entrySetText statusEntry "One GIF coming up!"
            result <- Gifcurry.gif params
            case result of
              Left _ ->
                GtkMainSyncAsync.gtkMainAsync $
                  GI.Gtk.entrySetText statusEntry "Didn't work. Check your settings."
              Right filePath -> do
                _ <-
                  forkOS $
                    GtkMainSyncAsync.gtkMainAsync $
                      openLocalFileWithDefaultProgram filePath
                GtkMainSyncAsync.gtkMainAsync $ GI.Gtk.entrySetText statusEntry "Ready."
            GtkMainSyncAsync.gtkMainAsync $ do
              GI.Gtk.setSpinnerActive saveSpinner False
              GI.Gtk.widgetHide saveSpinner
              GI.Gtk.widgetShow saveButton
              GI.Gtk.widgetSetSensitive saveButton True
              GI.Gtk.widgetSetSensitive openButton True
      else GI.Gtk.entrySetText statusEntry "The settings are wrong."
    return True
  where
    getQuality :: IO Gifcurry.Quality
    getQuality =
      fromMaybe Gifcurry.QualityMedium .
        Gifcurry.qualityFromString .
          Data.Text.unpack .
            fromMaybe "Medium" <$>
              GI.Gtk.getComboBoxActiveId qualityComboBoxText

handleOpenButtonClick :: GR.GuiComponents -> IO ()
handleOpenButtonClick
  GR.GuiComponents
    { GR.openButton
    , GR.saveAsVideoRadioButton
    , GR.outFileChooserButton
    , GR.outFileNameEntry
    , GR.statusEntry
    }
  =
  void $ GI.Gtk.onWidgetButtonReleaseEvent openButton $ \ _ -> do
    saveAsVideo <- GI.Gtk.toggleButtonGetActive saveAsVideoRadioButton
    outFilePath <- outFileChooserButtonGetFilePath outFileChooserButton outFileNameEntry
    let outFilePath' =
          Gifcurry.getOutputFileWithExtension $
          Gifcurry.defaultGifParams
            { Gifcurry.outputFile = outFilePath, Gifcurry.saveAsVideo = saveAsVideo }
    fileExists <- doesFileExist outFilePath'
    if fileExists
      then do
        GI.Gtk.entrySetText statusEntry "Ready."
        void $
          forkIO $
            GtkMainSyncAsync.gtkMainAsync $
              openLocalFileWithDefaultProgram outFilePath'
      else GI.Gtk.entrySetText statusEntry "Couldn't find the file. Check your settings."
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
    { GR.widthQualityToggleButton
    , GR.cropToggleButton
    , GR.textOverlaysToggleButton
    , GR.saveOpenToggleButton
    , GR.uploadToggleButton
    , GR.widthQualityBox
    , GR.cropSpinButtonsBox
    , GR.textOverlaysMainBox
    , GR.saveOpenBox
    , GR.uploadBox
    }
  = do
  let toggleButtons =
        [ widthQualityToggleButton
        , cropToggleButton
        , textOverlaysToggleButton
        , saveOpenToggleButton
        , uploadToggleButton
        ]
  let boxes =
        [ widthQualityBox
        , cropSpinButtonsBox
        , textOverlaysMainBox
        , saveOpenBox
        , uploadBox
        ]
  mapM_
    (\ (toggleButton, box) ->
      void $
        GI.Gtk.onToggleButtonToggled toggleButton $
          handleOnToggle
            toggleButton
            box
            toggleButtons
    ) $
    Data.List.zip toggleButtons boxes
  where
    handleOnToggle :: GI.Gtk.ToggleButton -> GI.Gtk.Box -> [GI.Gtk.ToggleButton] -> IO ()
    handleOnToggle toggleButton box toggleButtons = do
      active <- GI.Gtk.toggleButtonGetActive toggleButton
      if active
        then do
          GI.Gtk.widgetShow box
          mapM_
            (\ x ->
              isNotToggleButton x >>= \ b -> when b $ GI.Gtk.setToggleButtonActive x False
            )
            toggleButtons
        else
          GI.Gtk.widgetHide box
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

updateStatusEntryAsync :: GI.Gtk.Entry -> Word32 -> Text -> IO ()
updateStatusEntryAsync statusEntry seconds message =
  void $ GI.GLib.timeoutAdd
    GI.GLib.PRIORITY_DEFAULT
    seconds $ do
      GtkMainSyncAsync.gtkMainAsync $
        GI.Gtk.entrySetText statusEntry message
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
    , GR.inVideoPropertiesRef
    }
  = do
  startTime           <- GI.Gtk.spinButtonGetValue startTimeSpinButton
  videoDuration       <- floatToDouble . GR.inVideoDuration <$> readIORef inVideoPropertiesRef
  let startTime'      = if startTime >= videoDuration        then videoDuration else startTime
  let maxDurationTime = if videoDuration - startTime' <= 0.0 then 0.0           else videoDuration - startTime'
  let buffer          = if videoDuration * 0.01 > 0.1        then 0.1           else videoDuration * 0.01
  _ <- GI.Gtk.spinButtonSetRange startTimeSpinButton 0.0 (videoDuration - buffer)
  _ <- GI.Gtk.spinButtonSetRange durationTimeSpinButton buffer maxDurationTime
  return ()

syncStartAndDurationTimeSpinButtons :: GR.GuiComponents -> IO ()
syncStartAndDurationTimeSpinButtons
  guiComponents@GR.GuiComponents
    { GR.startTimeSpinButton
    , GR.durationTimeSpinButton
    , GR.inVideoPropertiesRef
    }
  = do
  startTime           <- GI.Gtk.spinButtonGetValue startTimeSpinButton
  durationTime        <- GI.Gtk.spinButtonGetValue durationTimeSpinButton
  videoDuration       <- floatToDouble . GR.inVideoDuration <$> readIORef inVideoPropertiesRef
  let startTime'      = if startTime >= videoDuration        then videoDuration   else startTime
  let maxDurationTime = if videoDuration - startTime' <= 0.0 then 0.0             else videoDuration - startTime'
  let durationTime'   = if durationTime >= maxDurationTime   then maxDurationTime else durationTime
  _ <- updateStartAndDurationTimeSpinButtonRanges guiComponents
  _ <- GI.Gtk.spinButtonSetValue startTimeSpinButton    $ truncatePastDigit startTime'    2
  _ <- GI.Gtk.spinButtonSetValue durationTimeSpinButton $ truncatePastDigit durationTime' 2
  updateStartAndDurationTimeSpinButtonFractions guiComponents

updateStartAndDurationTimeSpinButtonFractions :: GR.GuiComponents -> IO ()
updateStartAndDurationTimeSpinButtonFractions
  GR.GuiComponents
    { GR.startTimeSpinButton
    , GR.durationTimeSpinButton
    }
  = do
  _ <- setSpinButtonFraction startTimeSpinButton
  _ <- setSpinButtonFraction durationTimeSpinButton
  return ()

hideWidgetsOnRealize :: GR.GuiComponents -> IO ()
hideWidgetsOnRealize
  GR.GuiComponents
    { GR.saveSpinner
    , GR.sidebarControlsPreviewbox
    , GR.widthQualityBox
    , GR.cropSpinButtonsBox
    , GR.textOverlaysMainBox
    , GR.saveOpenBox
    , GR.uploadBox
    }
  = do
  hideOnRealize saveSpinner
  let boxes =
        [ sidebarControlsPreviewbox
        , widthQualityBox
        , cropSpinButtonsBox
        , textOverlaysMainBox
        , saveOpenBox
        , uploadBox
        ]
  mapM_ hideOnRealize boxes
  where
    hideOnRealize :: GI.Gtk.IsWidget a => a -> IO ()
    hideOnRealize w = void $ GI.Gtk.onWidgetRealize w $ GI.Gtk.widgetHide w

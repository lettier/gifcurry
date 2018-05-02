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
  , defaultGifParams
  , gifParamsValid
  , getVideoDurationInSeconds
  , getOutputFileWithExtension
  , getVideoWidthAndHeight
  , findOrCreateTemporaryDirectory
  )
import qualified GtkMainSyncAsync (gtkMainAsync)
import qualified GuiRecords as GR
import qualified GuiCapabilities
import qualified GuiStyle
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
  qualityPercentSpinButton          <- builderGetObject GI.Gtk.SpinButton        builder "quality-percent-spin-button"
  leftCropSpinButton                <- builderGetObject GI.Gtk.SpinButton        builder "left-crop-spin-button"
  rightCropSpinButton               <- builderGetObject GI.Gtk.SpinButton        builder "right-crop-spin-button"
  topCropSpinButton                 <- builderGetObject GI.Gtk.SpinButton        builder "top-crop-spin-button"
  bottomCropSpinButton              <- builderGetObject GI.Gtk.SpinButton        builder "bottom-crop-spin-button"
  inFileChooserButton               <- builderGetObject GI.Gtk.Button            builder "in-file-chooser-button"
  inFileChooserDialogCancelButton   <- builderGetObject GI.Gtk.Button            builder "in-file-chooser-dialog-cancel-button"
  inFileChooserDialogOpenButton     <- builderGetObject GI.Gtk.Button            builder "in-file-chooser-dialog-open-button"
  outFileChooserButton              <- builderGetObject GI.Gtk.FileChooserButton builder "out-file-chooser-button"
  fontChooserButton                 <- builderGetObject GI.Gtk.FontButton        builder "font-chooser-button"
  saveButton                        <- builderGetObject GI.Gtk.Button            builder "save-button"
  openButton                        <- builderGetObject GI.Gtk.Button            builder "open-button"
  yesGtkButton                      <- builderGetObject GI.Gtk.Button            builder "yes-button"
  noGtkButton                       <- builderGetObject GI.Gtk.Button            builder "no-button"
  aboutButton                       <- builderGetObject GI.Gtk.Button            builder "about-button"
  giphyUploadButton                 <- builderGetObject GI.Gtk.Button            builder "giphy-upload-button"
  imgurUploadButton                 <- builderGetObject GI.Gtk.Button            builder "imgur-upload-button"
  saveAsVideoRadioButton            <- builderGetObject GI.Gtk.RadioButton       builder "save-as-video-radio-button"
  widthQualityPercentToggleButton   <- builderGetObject GI.Gtk.ToggleButton      builder "width-quality-percent-toggle-button"
  cropToggleButton                  <- builderGetObject GI.Gtk.ToggleButton      builder "crop-toggle-button"
  topBottomTextToggleButton         <- builderGetObject GI.Gtk.ToggleButton      builder "top-bottom-text-toggle-button"
  saveOpenToggleButton              <- builderGetObject GI.Gtk.ToggleButton      builder "save-open-toggle-button"
  uploadToggleButton                <- builderGetObject GI.Gtk.ToggleButton      builder "upload-toggle-button"
  inFileChooserDialogLabel          <- builderGetObject GI.Gtk.Label             builder "in-file-chooser-dialog-label"
  inFileChooserButtonLabel          <- builderGetObject GI.Gtk.Label             builder "in-file-chooser-button-label"
  startTimeAdjustment               <- builderGetObject GI.Gtk.Adjustment        builder "start-time-adjustment"
  durationTimeAdjustment            <- builderGetObject GI.Gtk.Adjustment        builder "duration-time-adjustment"
  widthSizeAdjustment               <- builderGetObject GI.Gtk.Adjustment        builder "width-size-adjustment"
  qualityPercentAdjustment          <- builderGetObject GI.Gtk.Adjustment        builder "quality-percent-adjustment"
  outFileNameEntry                  <- builderGetObject GI.Gtk.Entry             builder "out-file-name-entry"
  topTextEntry                      <- builderGetObject GI.Gtk.Entry             builder "top-text-entry"
  bottomTextEntry                   <- builderGetObject GI.Gtk.Entry             builder "bottom-text-entry"
  statusEntry                       <- builderGetObject GI.Gtk.Entry             builder "status-entry"
  mainPreviewBox                    <- builderGetObject GI.Gtk.Box               builder "main-preview-box"
  imagesPreviewBox                  <- builderGetObject GI.Gtk.Box               builder "images-preview-box"
  videoPreviewBox                   <- builderGetObject GI.Gtk.Box               builder "video-preview-box"
  videoPreviewOverlayChildBox       <- builderGetObject GI.Gtk.Box               builder "video-preview-overlay-child-box"
  widthQualityPercentBox            <- builderGetObject GI.Gtk.Box               builder "width-quality-percent-box"
  cropSpinButtonsBox                <- builderGetObject GI.Gtk.Box               builder "crop-spin-buttons-box"
  topBottomTextFontChooserBox       <- builderGetObject GI.Gtk.Box               builder "top-bottom-text-font-chooser-box"
  saveOpenBox                       <- builderGetObject GI.Gtk.Box               builder "save-open-box"
  uploadBox                         <- builderGetObject GI.Gtk.Box               builder "upload-box"
  videoPreviewDrawingArea           <- builderGetObject GI.Gtk.DrawingArea       builder "video-preview-drawing-area"
  firstFramePreviewImageDrawingArea <- builderGetObject GI.Gtk.DrawingArea       builder "first-frame-preview-image-drawing-area"
  lastFramePreviewImageDrawingArea  <- builderGetObject GI.Gtk.DrawingArea       builder "last-frame-preview-image-drawing-area"
  inFileChooserButtonImage          <- builderGetObject GI.Gtk.Image             builder "in-file-chooser-button-image"
  firstFrameImage                   <- builderGetObject GI.Gtk.Image             builder "first-frame-image"
  lastFrameImage                    <- builderGetObject GI.Gtk.Image             builder "last-frame-image"
  inFileChooserDialog               <- builderGetObject GI.Gtk.Dialog            builder "in-file-chooser-dialog"
  longGifGtkMessageDialog           <- builderGetObject GI.Gtk.MessageDialog     builder "long-gif-message-dialog"
  aboutDialog                       <- builderGetObject GI.Gtk.AboutDialog       builder "about-dialog"
  startTimeProgressBar              <- builderGetObject GI.Gtk.ProgressBar       builder "start-time-progress-bar"
  endTimeProgressBar                <- builderGetObject GI.Gtk.ProgressBar       builder "end-time-progress-bar"
  saveSpinner                       <- builderGetObject GI.Gtk.Spinner           builder "save-spinner"
  inFileChooserWidget               <- builderGetObject GI.Gtk.FileChooserWidget builder "in-file-chooser-widget"

  -- Glade does not allow us to use the response ID nicknames so we set them here.
  GI.Gtk.dialogAddActionWidget longGifGtkMessageDialog yesGtkButton $ enumToInt32 GI.Gtk.ResponseTypeYes
  GI.Gtk.dialogAddActionWidget longGifGtkMessageDialog noGtkButton  $ enumToInt32 GI.Gtk.ResponseTypeNo
  GI.Gtk.dialogAddActionWidget inFileChooserDialog inFileChooserDialogCancelButton $ enumToInt32 GI.Gtk.ResponseTypeCancel
  GI.Gtk.dialogAddActionWidget inFileChooserDialog inFileChooserDialogOpenButton   $ enumToInt32 GI.Gtk.ResponseTypeOk

  (maybeVideoPreviewWidget, maybePlaybinElement) <-
    GuiPreview.buildVideoPreviewWidgetAndPlaybinElement

  temporaryDirectory <- Gifcurry.findOrCreateTemporaryDirectory

  guiPreviewStateRef <- newIORef GR.defaultGuiPreviewState

  inVideoPropertiesRef <- newIORef GR.defaultInVideoProperties

  let guiComponents =
        GR.GuiComponents
          { GR.window                            = window
          , GR.startTimeSpinButton               = startTimeSpinButton
          , GR.durationTimeSpinButton            = durationTimeSpinButton
          , GR.widthSizeSpinButton               = widthSizeSpinButton
          , GR.qualityPercentSpinButton          = qualityPercentSpinButton
          , GR.leftCropSpinButton                = leftCropSpinButton
          , GR.rightCropSpinButton               = rightCropSpinButton
          , GR.topCropSpinButton                 = topCropSpinButton
          , GR.bottomCropSpinButton              = bottomCropSpinButton
          , GR.inFileChooserButton               = inFileChooserButton
          , GR.inFileChooserDialogCancelButton   = inFileChooserDialogCancelButton
          , GR.inFileChooserDialogOpenButton     = inFileChooserDialogOpenButton
          , GR.outFileChooserButton              = outFileChooserButton
          , GR.fontChooserButton                 = fontChooserButton
          , GR.saveButton                        = saveButton
          , GR.openButton                        = openButton
          , GR.yesGtkButton                      = yesGtkButton
          , GR.noGtkButton                       = noGtkButton
          , GR.aboutButton                       = aboutButton
          , GR.giphyUploadButton                 = giphyUploadButton
          , GR.imgurUploadButton                 = imgurUploadButton
          , GR.saveAsVideoRadioButton            = saveAsVideoRadioButton
          , GR.widthQualityPercentToggleButton   = widthQualityPercentToggleButton
          , GR.cropToggleButton                  = cropToggleButton
          , GR.topBottomTextToggleButton         = topBottomTextToggleButton
          , GR.saveOpenToggleButton              = saveOpenToggleButton
          , GR.uploadToggleButton                = uploadToggleButton
          , GR.inFileChooserDialogLabel          = inFileChooserDialogLabel
          , GR.inFileChooserButtonLabel          = inFileChooserButtonLabel
          , GR.startTimeAdjustment               = startTimeAdjustment
          , GR.durationTimeAdjustment            = durationTimeAdjustment
          , GR.widthSizeAdjustment               = widthSizeAdjustment
          , GR.qualityPercentAdjustment          = qualityPercentAdjustment
          , GR.outFileNameEntry                  = outFileNameEntry
          , GR.topTextEntry                      = topTextEntry
          , GR.bottomTextEntry                   = bottomTextEntry
          , GR.statusEntry                       = statusEntry
          , GR.mainPreviewBox                    = mainPreviewBox
          , GR.imagesPreviewBox                  = imagesPreviewBox
          , GR.videoPreviewBox                   = videoPreviewBox
          , GR.videoPreviewOverlayChildBox       = videoPreviewOverlayChildBox
          , GR.widthQualityPercentBox            = widthQualityPercentBox
          , GR.cropSpinButtonsBox                = cropSpinButtonsBox
          , GR.topBottomTextFontChooserBox       = topBottomTextFontChooserBox
          , GR.saveOpenBox                       = saveOpenBox
          , GR.uploadBox                         = uploadBox
          , GR.videoPreviewDrawingArea           = videoPreviewDrawingArea
          , GR.firstFramePreviewImageDrawingArea = firstFramePreviewImageDrawingArea
          , GR.lastFramePreviewImageDrawingArea  = lastFramePreviewImageDrawingArea
          , GR.inFileChooserButtonImage          = inFileChooserButtonImage
          , GR.firstFrameImage                   = firstFrameImage
          , GR.lastFrameImage                    = lastFrameImage
          , GR.inFileChooserDialog               = inFileChooserDialog
          , GR.longGifGtkMessageDialog           = longGifGtkMessageDialog
          , GR.aboutDialog                       = aboutDialog
          , GR.startTimeProgressBar              = startTimeProgressBar
          , GR.endTimeProgressBar                = endTimeProgressBar
          , GR.saveSpinner                       = saveSpinner
          , GR.inFileChooserWidget               = inFileChooserWidget
          , GR.maybeVideoPreviewWidget           = maybeVideoPreviewWidget
          , GR.maybePlaybinElement               = maybePlaybinElement
          , GR.temporaryDirectory                = temporaryDirectory
          , GR.guiPreviewStateRef                = guiPreviewStateRef
          , GR.inVideoPropertiesRef              = inVideoPropertiesRef
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
builderGetObject objectTypeClass builder objectId =
  fromJust <$> GI.Gtk.builderGetObject builder (pack objectId) >>=
  GI.Gtk.unsafeCastTo objectTypeClass

handleFileChooserDialogReponse :: GR.GuiComponents -> Int32 -> IO ()
handleFileChooserDialogReponse
  guiComponents@GR.GuiComponents
    { GR.startTimeSpinButton
    , GR.durationTimeSpinButton
    , GR.leftCropSpinButton
    , GR.rightCropSpinButton
    , GR.topCropSpinButton
    , GR.bottomCropSpinButton
    , GR.widthSizeSpinButton
    , GR.qualityPercentSpinButton
    , GR.topTextEntry
    , GR.bottomTextEntry
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
        let videoDuration = float2Double videoDuration'
        let startTimeFraction = 0.25
        let startTime = videoDuration * startTimeFraction
        let endTime = startTime * 3
        let durationTime = endTime - startTime
        let videoDurationText = Data.Text.pack $ printf "%.3f" videoDuration
        _ <- updateStartAndDurationTimeSpinButtonRanges guiComponents
        _ <- GI.Gtk.spinButtonSetValue startTimeSpinButton startTime
        _ <- GI.Gtk.spinButtonSetValue durationTimeSpinButton durationTime
        _ <- updateStatusEntryAsync statusEntry 1 $
          Data.Text.concat
            [ "That video is about "
            , videoDurationText
            , " seconds long."
            ]
        GI.Gtk.labelSetText inFileChooserButtonLabel $
          Data.Text.pack $
            takeFileName inFilePath
        return ()
      _ -> do
        atomicWriteIORef inVideoPropertiesRef GR.defaultInVideoProperties
        _ <- updateStartAndDurationTimeSpinButtonRanges guiComponents
        _ <- GI.Gtk.spinButtonSetValue startTimeSpinButton 0.0
        _ <- GI.Gtk.spinButtonSetValue durationTimeSpinButton 0.0
        _ <- updateStatusEntryAsync statusEntry 1 "Is that a video?"
        GI.Gtk.labelSetText inFileChooserButtonLabel "Open"
        return ()
    syncStartAndDurationTimeSpinButtons guiComponents
    resetTextEntries
    resetWidthAndQualityPercentSpinButtons
    resetCropSpinButtons
  where
    resetTextEntries :: IO ()
    resetTextEntries = do
      let textEntries =
            [ topTextEntry
            , bottomTextEntry
            , outFileNameEntry
            ]
      mapM_
        (flip GI.Gtk.entrySetText "")
        textEntries
    resetWidthAndQualityPercentSpinButtons :: IO ()
    resetWidthAndQualityPercentSpinButtons = do
      GI.Gtk.spinButtonSetValue widthSizeSpinButton 500
      GI.Gtk.spinButtonSetValue qualityPercentSpinButton 100
    resetCropSpinButtons :: IO ()
    resetCropSpinButtons = do
      let spinButtons =
            [ leftCropSpinButton
            , rightCropSpinButton
            , topCropSpinButton
            , bottomCropSpinButton
            ]
      mapM_
        (flip GI.Gtk.spinButtonSetValue 0.0)
        spinButtons

handleSpinButtons :: GR.GuiComponents -> IO ()
handleSpinButtons
  guiComponents@GR.GuiComponents
    { GR.startTimeSpinButton
    , GR.durationTimeSpinButton
    , GR.widthSizeSpinButton
    , GR.qualityPercentSpinButton
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
    qualityPercentSpinButton
    handleQualityPercentSpinButton
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
        || durationTime > (videoDuration - startTime)
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
    handleQualityPercentSpinButton :: IO ()
    handleQualityPercentSpinButton = do
      qualityPercent <- double2Float <$> GI.Gtk.spinButtonGetValue qualityPercentSpinButton
      _ <- setSpinButtonFraction qualityPercentSpinButton
      if qualityPercent <= 0.0 || qualityPercent > 100.0
        then do
          GI.Gtk.entrySetText statusEntry "The quality percent is wrong."
          highlightSpinButton qualityPercentSpinButton
        else do
          GI.Gtk.entrySetText statusEntry "Ready."
          unhighlightSpinButton qualityPercentSpinButton
    handleCropSpinButton :: GI.Gtk.SpinButton -> GI.Gtk.SpinButton -> Text -> IO ()
    handleCropSpinButton a b t = do
      cropValue <- double2Float <$> GI.Gtk.spinButtonGetValue a
      _ <- setSpinButtonFraction a
      if cropValue < 0.0 || cropValue > 100.0
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
      when (aValue + bValue >= 100) $ do
        let newValue  = 100.0 - aValue - 1.0
        let newValue' = if newValue < 0.0 then 0.0 else newValue
        void $ GI.Gtk.spinButtonSetValue b newValue'
      return ()
    inVideoDuration :: IO Float
    inVideoDuration =
      GR.inVideoDuration <$> readIORef inVideoPropertiesRef

handleSaveButtonClick :: GR.GuiComponents -> IO ()
handleSaveButtonClick
  GR.GuiComponents
    { GR.outFileChooserButton
    , GR.outFileNameEntry
    , GR.saveButton
    , GR.openButton
    , GR.fontChooserButton
    , GR.saveAsVideoRadioButton
    , GR.startTimeSpinButton
    , GR.durationTimeSpinButton
    , GR.widthSizeSpinButton
    , GR.qualityPercentSpinButton
    , GR.leftCropSpinButton
    , GR.rightCropSpinButton
    , GR.topCropSpinButton
    , GR.bottomCropSpinButton
    , GR.topTextEntry
    , GR.bottomTextEntry
    , GR.statusEntry
    , GR.inFileChooserWidget
    , GR.longGifGtkMessageDialog
    , GR.saveSpinner
    }
  =
  void $ GI.Gtk.onWidgetButtonReleaseEvent saveButton $ \ _ -> do
    inFilePath     <- fileChooserGetFilePath inFileChooserWidget
    startTime      <- double2Float <$> GI.Gtk.spinButtonGetValue startTimeSpinButton
    durationTime   <- double2Float <$> GI.Gtk.spinButtonGetValue durationTimeSpinButton
    widthSize      <- double2Float <$> GI.Gtk.spinButtonGetValue widthSizeSpinButton
    qualityPercent <- double2Float <$> GI.Gtk.spinButtonGetValue qualityPercentSpinButton
    leftCrop       <- double2Float <$> GI.Gtk.spinButtonGetValue leftCropSpinButton
    rightCrop      <- double2Float <$> GI.Gtk.spinButtonGetValue rightCropSpinButton
    topCrop        <- double2Float <$> GI.Gtk.spinButtonGetValue topCropSpinButton
    bottomCrop     <- double2Float <$> GI.Gtk.spinButtonGetValue bottomCropSpinButton
    fontChoice     <- GI.Gtk.fontButtonGetFontName fontChooserButton
    topText        <- GI.Gtk.entryGetText topTextEntry
    bottomText     <- GI.Gtk.entryGetText bottomTextEntry
    saveAsVideo    <- GI.Gtk.toggleButtonGetActive saveAsVideoRadioButton
    outFilePath    <- outFileChooserButtonGetFilePath outFileChooserButton outFileNameEntry
    let params =
          Gifcurry.defaultGifParams
            { Gifcurry.inputFile      = inFilePath
            , Gifcurry.outputFile     = outFilePath
            , Gifcurry.saveAsVideo    = saveAsVideo
            , Gifcurry.startTime      = startTime
            , Gifcurry.durationTime   = durationTime
            , Gifcurry.widthSize      = truncate widthSize
            , Gifcurry.qualityPercent = qualityPercent
            , Gifcurry.fontChoice     = unpack fontChoice
            , Gifcurry.topText        = unpack topText
            , Gifcurry.bottomText     = unpack bottomText
            , Gifcurry.leftCrop       = leftCrop
            , Gifcurry.rightCrop      = rightCrop
            , Gifcurry.topCrop        = topCrop
            , Gifcurry.bottomCrop     = bottomCrop
            }
    paramsValid <- Gifcurry.gifParamsValid params
    GI.Gtk.entrySetText statusEntry "Ready."
    if paramsValid
      then do
        longGifGtkMessageDialogResponse <-
          if durationTime >= durationTimeWarningLevel
            then GI.Gtk.dialogRun longGifGtkMessageDialog
            else return (enumToInt32 GI.Gtk.ResponseTypeYes)
        when (longGifGtkMessageDialogResponse == enumToInt32 GI.Gtk.ResponseTypeYes) $ do
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
    , GR.longGifGtkMessageDialog
    , GR.aboutButton
    , GR.inFileChooserButton
    }
  = do
  _ <- GI.Gtk.onWidgetButtonReleaseEvent
    aboutButton
    (\ _ -> GI.Gtk.dialogRun aboutDialog >> return True)
  _ <- GI.Gtk.onDialogResponse
    longGifGtkMessageDialog
    (\ _ -> GI.Gtk.widgetHide longGifGtkMessageDialog)
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
    { GR.widthQualityPercentToggleButton
    , GR.cropToggleButton
    , GR.topBottomTextToggleButton
    , GR.saveOpenToggleButton
    , GR.uploadToggleButton
    , GR.widthQualityPercentBox
    , GR.cropSpinButtonsBox
    , GR.topBottomTextFontChooserBox
    , GR.saveOpenBox
    , GR.uploadBox
    }
  = do
  let toggleButtons =
        [ widthQualityPercentToggleButton
        , cropToggleButton
        , topBottomTextToggleButton
        , saveOpenToggleButton
        , uploadToggleButton
        ]
  let boxes =
        [ widthQualityPercentBox
        , cropSpinButtonsBox
        , topBottomTextFontChooserBox
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
styleSpinButtonAndEntry style =
  GuiStyle.styleWidget
    (  "spinbutton "
    ++ style
    ++ " .spinbutton "
    ++ style
    ++ " spinbutton entry "
    ++ style
    ++ " .spinbutton .entry "
    ++ style
    )

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
  void $ GI.Gtk.setEntryProgressFraction spinButton fraction

updateStartAndDurationTimeSpinButtonRanges :: GR.GuiComponents -> IO ()
updateStartAndDurationTimeSpinButtonRanges
  GR.GuiComponents
    { GR.startTimeSpinButton
    , GR.durationTimeSpinButton
    , GR.inVideoPropertiesRef
    }
  = do
  videoDuration <- float2Double . GR.inVideoDuration <$> readIORef inVideoPropertiesRef
  let buffer    = if videoDuration * 0.01 > 0.1 then 0.1 else videoDuration * 0.01
  _ <- GI.Gtk.spinButtonSetRange startTimeSpinButton 0.0 (videoDuration - buffer)
  _ <- GI.Gtk.spinButtonSetRange durationTimeSpinButton buffer videoDuration
  return ()

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

syncStartAndDurationTimeSpinButtons :: GR.GuiComponents -> IO ()
syncStartAndDurationTimeSpinButtons
  guiComponents@GR.GuiComponents
    { GR.startTimeSpinButton
    , GR.durationTimeSpinButton
    , GR.inVideoPropertiesRef
    }
  = do
  startTime     <- GI.Gtk.spinButtonGetValue startTimeSpinButton
  durationTime  <- GI.Gtk.spinButtonGetValue durationTimeSpinButton
  videoDuration <- float2Double . GR.inVideoDuration <$> readIORef inVideoPropertiesRef
  let startTime' = if startTime >= videoDuration then videoDuration else startTime
  let maxDurationTime =
        if videoDuration - startTime' <= 0.0 then 0.0 else videoDuration - startTime'
  let durationTime' =
        if durationTime >= maxDurationTime then maxDurationTime else durationTime
  _ <- updateStartAndDurationTimeSpinButtonRanges guiComponents
  _ <- GI.Gtk.spinButtonSetValue startTimeSpinButton startTime'
  _ <- GI.Gtk.spinButtonSetValue durationTimeSpinButton durationTime'
  updateStartAndDurationTimeSpinButtonFractions guiComponents
  updateStartAndEndTimeProgressBars guiComponents

updateStartAndEndTimeProgressBars :: GR.GuiComponents -> IO ()
updateStartAndEndTimeProgressBars
  GR.GuiComponents
    { GR.startTimeSpinButton
    , GR.durationTimeSpinButton
    , GR.startTimeProgressBar
    , GR.endTimeProgressBar
    , GR.inVideoPropertiesRef
    }
  = do
  videoDuration <- float2Double . GR.inVideoDuration <$> readIORef inVideoPropertiesRef
  startTime     <- GuiMisc.clamp 0.0 videoDuration <$> GI.Gtk.spinButtonGetValue startTimeSpinButton
  durationTime  <- GuiMisc.clamp 0.0 videoDuration <$> GI.Gtk.spinButtonGetValue durationTimeSpinButton
  let endTime   = startTime + durationTime
  let endTime'  = GuiMisc.clamp 0.0 videoDuration $ videoDuration - endTime
  let startTimeProgressBarFraction = fromMaybe 0.0 $ safeDivide startTime videoDuration
  let endTimeProgressBarFraction   = fromMaybe 0.0 $ safeDivide endTime'  videoDuration
  _ <- GI.Gtk.progressBarSetFraction startTimeProgressBar startTimeProgressBarFraction
  _ <- GI.Gtk.progressBarSetFraction endTimeProgressBar   endTimeProgressBarFraction
  return ()

hideWidgetsOnRealize :: GR.GuiComponents -> IO ()
hideWidgetsOnRealize
  GR.GuiComponents
    { GR.saveSpinner
    , GR.widthQualityPercentBox
    , GR.cropSpinButtonsBox
    , GR.topBottomTextFontChooserBox
    , GR.saveOpenBox
    , GR.uploadBox
    }
  = do
  hideOnRealize saveSpinner
  let boxes =
        [ widthQualityPercentBox
        , cropSpinButtonsBox
        , topBottomTextFontChooserBox
        , saveOpenBox
        , uploadBox
        ]
  mapM_ hideOnRealize boxes
  where
    hideOnRealize :: GI.Gtk.IsWidget a => a -> IO ()
    hideOnRealize w = void $ GI.Gtk.onWidgetRealize w $ GI.Gtk.widgetHide w

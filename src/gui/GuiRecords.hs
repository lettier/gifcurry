{-
  Gifcurry
  (C) 2018 David Lettier
  lettier.com
-}

module GuiRecords where

import Data.IORef
import qualified GI.Gtk
import GI.Gst

data GuiComponents =
  GuiComponents
    { window                            :: GI.Gtk.Window
    , startTimeSpinButton               :: GI.Gtk.SpinButton
    , durationTimeSpinButton            :: GI.Gtk.SpinButton
    , widthSizeSpinButton               :: GI.Gtk.SpinButton
    , qualityPercentSpinButton          :: GI.Gtk.SpinButton
    , leftCropSpinButton                :: GI.Gtk.SpinButton
    , rightCropSpinButton               :: GI.Gtk.SpinButton
    , topCropSpinButton                 :: GI.Gtk.SpinButton
    , bottomCropSpinButton              :: GI.Gtk.SpinButton
    , inFileChooserButton               :: GI.Gtk.Button
    , inFileChooserDialogCancelButton   :: GI.Gtk.Button
    , inFileChooserDialogOpenButton     :: GI.Gtk.Button
    , outFileChooserButton              :: GI.Gtk.FileChooserButton
    , fontChooserButton                 :: GI.Gtk.FontButton
    , saveButton                        :: GI.Gtk.Button
    , openButton                        :: GI.Gtk.Button
    , yesGtkButton                      :: GI.Gtk.Button
    , noGtkButton                       :: GI.Gtk.Button
    , aboutButton                       :: GI.Gtk.Button
    , giphyUploadButton                 :: GI.Gtk.Button
    , imgurUploadButton                 :: GI.Gtk.Button
    , saveAsVideoRadioButton            :: GI.Gtk.RadioButton
    , widthQualityPercentToggleButton   :: GI.Gtk.ToggleButton
    , cropToggleButton                  :: GI.Gtk.ToggleButton
    , topBottomTextToggleButton         :: GI.Gtk.ToggleButton
    , saveOpenToggleButton              :: GI.Gtk.ToggleButton
    , uploadToggleButton                :: GI.Gtk.ToggleButton
    , inFileChooserDialogLabel          :: GI.Gtk.Label
    , inFileChooserButtonLabel          :: GI.Gtk.Label
    , startTimeAdjustment               :: GI.Gtk.Adjustment
    , durationTimeAdjustment            :: GI.Gtk.Adjustment
    , widthSizeAdjustment               :: GI.Gtk.Adjustment
    , qualityPercentAdjustment          :: GI.Gtk.Adjustment
    , outFileNameEntry                  :: GI.Gtk.Entry
    , topTextEntry                      :: GI.Gtk.Entry
    , bottomTextEntry                   :: GI.Gtk.Entry
    , statusEntry                       :: GI.Gtk.Entry
    , mainPreviewBox                    :: GI.Gtk.Box
    , imagesPreviewBox                  :: GI.Gtk.Box
    , videoPreviewBox                   :: GI.Gtk.Box
    , videoPreviewOverlayChildBox       :: GI.Gtk.Box
    , widthQualityPercentBox            :: GI.Gtk.Box
    , cropSpinButtonsBox                :: GI.Gtk.Box
    , topBottomTextFontChooserBox       :: GI.Gtk.Box
    , saveOpenBox                       :: GI.Gtk.Box
    , uploadBox                         :: GI.Gtk.Box
    , videoPreviewDrawingArea           :: GI.Gtk.DrawingArea
    , firstFramePreviewImageDrawingArea :: GI.Gtk.DrawingArea
    , lastFramePreviewImageDrawingArea  :: GI.Gtk.DrawingArea
    , inFileChooserButtonImage          :: GI.Gtk.Image
    , firstFrameImage                   :: GI.Gtk.Image
    , lastFrameImage                    :: GI.Gtk.Image
    , inFileChooserDialog               :: GI.Gtk.Dialog
    , longGifGtkMessageDialog           :: GI.Gtk.MessageDialog
    , aboutDialog                       :: GI.Gtk.AboutDialog
    , startTimeProgressBar              :: GI.Gtk.ProgressBar
    , endTimeProgressBar                :: GI.Gtk.ProgressBar
    , saveSpinner                       :: GI.Gtk.Spinner
    , inFileChooserWidget               :: GI.Gtk.FileChooserWidget
    , maybeVideoPreviewWidget           :: Maybe GI.Gtk.Widget
    , maybePlaybinElement               :: Maybe GI.Gst.Element
    , temporaryDirectory                :: FilePath
    , guiPreviewStateRef                :: IORef GuiPreviewState
    , inVideoPropertiesRef              :: IORef InVideoProperties
    }

data GuiPreviewState =
  GuiPreviewState
    { maybeInFilePath   :: Maybe String
    , maybeStartTime    :: Maybe Float
    , maybeDurationTime :: Maybe Float
    , loopRunning       :: Bool
    }

data InVideoProperties =
  InVideoProperties
    { inVideoUri      :: String
    , inVideoDuration :: Float
    , inVideoWidth    :: Float
    , inVideoHeight   :: Float
    }

defaultGuiPreviewState :: GuiPreviewState
defaultGuiPreviewState =
  GuiPreviewState
    { maybeInFilePath = Nothing
    , maybeStartTime = Nothing
    , maybeDurationTime = Nothing
    , loopRunning = False
    }

defaultInVideoProperties :: InVideoProperties
defaultInVideoProperties =
  InVideoProperties
    { inVideoUri = ""
    , inVideoDuration = 0.0
    , inVideoWidth = 0.0
    , inVideoHeight = 0.0
    }

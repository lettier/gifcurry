{-
  Gifcurry
  (C) 2018 David Lettier
  lettier.com
-}

{-# LANGUAGE
    DuplicateRecordFields
#-}

module GuiRecords where

import Data.IORef
import Data.Int
import qualified GI.Gtk
import qualified Graphics.Rendering.Pango.Font as GRPF
import GI.Gst

data GuiComponents =
  GuiComponents
    { window                              :: GI.Gtk.Window
    , startTimeSpinButton                 :: GI.Gtk.SpinButton
    , endTimeSpinButton                   :: GI.Gtk.SpinButton
    , widthSpinButton                     :: GI.Gtk.SpinButton
    , fpsSpinButton                       :: GI.Gtk.SpinButton
    , colorCountSpinButton                :: GI.Gtk.SpinButton
    , leftCropSpinButton                  :: GI.Gtk.SpinButton
    , rightCropSpinButton                 :: GI.Gtk.SpinButton
    , topCropSpinButton                   :: GI.Gtk.SpinButton
    , bottomCropSpinButton                :: GI.Gtk.SpinButton
    , inFileChooserButton                 :: GI.Gtk.Button
    , inFileChooserDialogCancelButton     :: GI.Gtk.Button
    , inFileChooserDialogOpenButton       :: GI.Gtk.Button
    , outFileChooserDialogCancelButton    :: GI.Gtk.Button
    , outFileChooserDialogViewButton      :: GI.Gtk.Button
    , outFileChooserDialogSaveButton      :: GI.Gtk.Button
    , outFileButton                       :: GI.Gtk.Button
    , textOverlaysAddButton               :: GI.Gtk.Button
    , textOverlaysOpenButton              :: GI.Gtk.Button
    , textOverlaysSaveButton              :: GI.Gtk.Button
    , textOverlaysRemoveAllButton         :: GI.Gtk.Button
    , confirmMessageDialogYesButton       :: GI.Gtk.Button
    , confirmMessageDialogNoButton        :: GI.Gtk.Button
    , aboutButton                         :: GI.Gtk.Button
    , aboutDialogCloseButton              :: GI.Gtk.Button
    , giphyUploadButton                   :: GI.Gtk.Button
    , imgurUploadButton                   :: GI.Gtk.Button
    , outFileChooserDialogGifRadioButton  :: GI.Gtk.RadioButton
    , fileSizeToggleButton                :: GI.Gtk.ToggleButton
    , cropToggleButton                    :: GI.Gtk.ToggleButton
    , textOverlaysToggleButton            :: GI.Gtk.ToggleButton
    , uploadToggleButton                  :: GI.Gtk.ToggleButton
    , videoPreviewPauseToggleButton       :: GI.Gtk.ToggleButton
    , ditherToggleButton                  :: GI.Gtk.ToggleButton
    , inFileChooserDialogLabel            :: GI.Gtk.Label
    , inFileChooserButtonLabel            :: GI.Gtk.Label
    , startTimeAdjustment                 :: GI.Gtk.Adjustment
    , endTimeAdjustment                   :: GI.Gtk.Adjustment
    , widthAdjustment                     :: GI.Gtk.Adjustment
    , fpsAdjustment                       :: GI.Gtk.Adjustment
    , colorCountAdjustment                :: GI.Gtk.Adjustment
    , aboutDialogLabel                    :: GI.Gtk.Label
    , statusLabel                         :: GI.Gtk.Label
    , sidebarControlsPreviewbox           :: GI.Gtk.Box
    , mainPreviewBox                      :: GI.Gtk.Box
    , imagesPreviewBox                    :: GI.Gtk.Box
    , videoPreviewBox                     :: GI.Gtk.Box
    , videoPreviewOverlayChildBox         :: GI.Gtk.Box
    , cropSpinButtonsBox                  :: GI.Gtk.Box
    , textOverlaysMainBox                 :: GI.Gtk.Box
    , textOverlaysBox                     :: GI.Gtk.Box
    , uploadBox                           :: GI.Gtk.Box
    , fileSizeSpinButtonsGrid             :: GI.Gtk.Grid
    , videoPreviewDrawingArea             :: GI.Gtk.DrawingArea
    , timeSlicesDrawingArea               :: GI.Gtk.DrawingArea
    , firstFramePreviewImageDrawingArea   :: GI.Gtk.DrawingArea
    , lastFramePreviewImageDrawingArea    :: GI.Gtk.DrawingArea
    , inFileChooserButtonImage            :: GI.Gtk.Image
    , firstFrameImage                     :: GI.Gtk.Image
    , lastFrameImage                      :: GI.Gtk.Image
    , inFileChooserDialog                 :: GI.Gtk.Dialog
    , outFileChooserDialog                :: GI.Gtk.FileChooserDialog
    , textOverlaysOpenDialog              :: GI.Gtk.FileChooserDialog
    , textOverlaysSaveDialog              :: GI.Gtk.FileChooserDialog
    , confirmMessageDialog                :: GI.Gtk.MessageDialog
    , aboutDialog                         :: GI.Gtk.Dialog
    , inFileChooserWidget                 :: GI.Gtk.FileChooserWidget
    , outFileChooserDialogGifFileFilter   :: GI.Gtk.FileFilter
    , outFileChooserDialogVideoFileFilter :: GI.Gtk.FileFilter
    , maybeVideoPreviewWidget             :: Maybe GI.Gtk.Widget
    , maybePlaybinElement                 :: Maybe GI.Gst.Element
    , temporaryDirectory                  :: FilePath
    , textOverlaysRef                     :: IORef [GuiTextOverlayComponents]
    , guiInFilePropertiesRef              :: IORef GuiInFileProperties
    , guiPreviewStateRef                  :: IORef GuiPreviewState
    }

data GuiPreviewState =
  GuiPreviewState
    { maybeInFilePath :: Maybe String
    , maybeStartTime  :: Maybe Double
    , maybeEndTime    :: Maybe Double
    , maybeColorCount :: Maybe Double
    , maybeDither     :: Maybe Bool
    , loopRunning     :: Bool
    }

data GuiInFileProperties =
  GuiInFileProperties
    { inFileUri      :: String
    , inFileFps      :: Double
    , inFileDuration :: Double
    , inFileWidth    :: Double
    , inFileHeight   :: Double
    }

data GuiTextOverlayComponents =
  GuiTextOverlayComponents
    { textOverlayId                     :: Int
    , textOverlayBox                    :: GI.Gtk.Box
    , textOverlayVisibilityBox          :: GI.Gtk.Box
    , textOverlayVisibilityToggleButton :: GI.Gtk.ToggleButton
    , textOverlayHorizontalSpinButton   :: GI.Gtk.SpinButton
    , textOverlayVerticalSpinButton     :: GI.Gtk.SpinButton
    , textOverlayStartTimeSpinButton    :: GI.Gtk.SpinButton
    , textOverlayEndTimeSpinButton      :: GI.Gtk.SpinButton
    , textOverlayRotationSpinButton     :: GI.Gtk.SpinButton
    , textOverlayOutlineSizeSpinButton  :: GI.Gtk.SpinButton
    , textOverlayOutlineColorButton     :: GI.Gtk.ColorButton
    , textOverlayFillColorButton        :: GI.Gtk.ColorButton
    , textOverlayTextEntry              :: GI.Gtk.Entry
    , textOverlayFontButton             :: GI.Gtk.FontButton
    , textOverlayCloneButton            :: GI.Gtk.Button
    , textOverlayRemoveButton           :: GI.Gtk.Button
    , textOverlayOrderUpButton          :: GI.Gtk.Button
    , textOverlayOrderDownButton        :: GI.Gtk.Button
    }

data GuiTextOverlayData =
  GuiTextOverlayData
    { textOverlayControlsVisible :: Bool
    , textOverlayText            :: String
    , textOverlayHorizontal      :: Double
    , textOverlayVertical        :: Double
    , textOverlayStartTime       :: Double
    , textOverlayEndTime         :: Double
    , textOverlayEndTime         :: Double
    , textOverlayRotation        :: Int32
    , textOverlayOutlineSize     :: Int32
    , textOverlayOutlineColor    :: String
    , textOverlayFillColor       :: String
    , textOverlayMaybeFontDesc   :: Maybe GRPF.FontDescription
    }

data GuiPreviewFunctionArgs =
  GuiPreviewFunctionArgs
    { guiComponents     :: GuiComponents
    , inFilePath        :: String
    , startTime         :: Double
    , endTime           :: Double
    , colorCount        :: Double
    , inFileWidth       :: Double
    , inFileHeight      :: Double
    , dither            :: Bool
    , inFilePathChanged :: Bool
    , startTimeChanged  :: Bool
    , endTimeChanged    :: Bool
    , colorCountChanged :: Bool
    , ditherChanged     :: Bool
    }

data GuiMakeFramePreviewFunctionArgs =
  GuiMakeFramePreviewFunctionArgs
    { inFilePath         :: String
    , startTime          :: Double
    , endTime            :: Double
    , colorCount         :: Double
    , previewWidth       :: Double
    , dither             :: Bool
    , firstFrameImage    :: GI.Gtk.Image
    , lastFrameImage     :: GI.Gtk.Image
    , temporaryDirectory :: String
    , window             :: GI.Gtk.Window
    }

data GuiSetOrResetFramePrevewFunctionArgs =
  GuiSetOrResetFramePrevewFunctionArgs
    { inputValid   :: Bool
    , inFilePath   :: String
    , outFilePath  :: String
    , time         :: Double
    , colorCount   :: Double
    , previewWidth :: Double
    , dither       :: Bool
    , image        :: GI.Gtk.Image
    , window       :: GI.Gtk.Window
    }

defaultGuiPreviewState :: GuiPreviewState
defaultGuiPreviewState =
  GuiPreviewState
    { maybeInFilePath   = Nothing
    , maybeStartTime    = Nothing
    , maybeEndTime      = Nothing
    , maybeColorCount   = Nothing
    , maybeDither       = Nothing
    , loopRunning       = False
    }

defaultGuiInFileProperties
  :: GuiInFileProperties
defaultGuiInFileProperties =
  GuiInFileProperties
    { inFileUri      = ""
    , inFileFps      = 0.0
    , inFileDuration = 0.0
    , inFileWidth    = 0.0
    , inFileHeight   = 0.0
    }

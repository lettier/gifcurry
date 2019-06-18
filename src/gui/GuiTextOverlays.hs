{-
  Gifcurry
  (C) 2018 David Lettier
  lettier.com
-}

{-# LANGUAGE
    OverloadedStrings
  , NamedFieldPuns
  , BangPatterns
  , DuplicateRecordFields
#-}

module GuiTextOverlays where

import Control.Monad
import Control.Exception
import Data.Maybe
import Data.IORef
import Data.Int
import qualified Data.Text
import Data.List (sort)
import qualified GI.Gdk
import qualified GI.GdkPixbuf
import qualified GI.Gtk
import qualified GI.Pango
import qualified Graphics.Rendering.Pango.Enums as GRPF
import qualified Graphics.Rendering.Pango.Font as GRPF
import qualified Graphics.Rendering.Pango.Cairo as GRPC
import qualified Graphics.Rendering.Pango.Layout as GRPL

import Paths_Gifcurry
import qualified Gifcurry
import qualified GuiRecords as GR
import GuiStyle
import GuiMisc

handleTextOverlaysControls
  ::  GR.GuiComponents
  ->  IO ()
handleTextOverlaysControls
  guiComponents@GR.GuiComponents
    { GR.textOverlaysAddButton
    , GR.textOverlaysOpenButton
    , GR.textOverlaysSaveButton
    , GR.textOverlaysRemoveAllButton
    , GR.textOverlaysOpenDialog
    , GR.textOverlaysSaveDialog
    }
  = do
  GI.Gtk.widgetHide textOverlaysRemoveAllButton

  void
    $ GI.Gtk.onWidgetButtonReleaseEvent
      textOverlaysOpenButton
      (\ _ -> GI.Gtk.dialogRun textOverlaysOpenDialog >> return True)
  void
    $ GI.Gtk.onWidgetButtonReleaseEvent
      textOverlaysSaveButton
      (\ _ -> GI.Gtk.dialogRun textOverlaysSaveDialog >> return True)
  void
    $ GI.Gtk.onWidgetButtonReleaseEvent
      textOverlaysRemoveAllButton
      (\ _ -> handleRemoveAllButtonPress guiComponents >> return True)
  void
    $ GI.Gtk.onDialogResponse
      textOverlaysOpenDialog
      (handleOpenDialogResponse guiComponents)
  void
    $ GI.Gtk.onDialogResponse
      textOverlaysSaveDialog
      (handleSaveDialogResponse guiComponents)
  void
    $ GI.Gtk.onButtonClicked
      textOverlaysAddButton
      (addTextOverlay guiComponents Nothing)

handleRemoveAllButtonPress
  ::  GR.GuiComponents
  ->  IO ()
handleRemoveAllButtonPress
  GR.GuiComponents
    { GR.confirmMessageDialog
    , GR.textOverlaysBox
    , GR.textOverlaysRef
    , GR.textOverlaysRemoveAllButton
    }
  = do
  GI.Gtk.setMessageDialogText
    confirmMessageDialog
    "Remove all text?"
  confirmMessageDialogResponse <- GI.Gtk.dialogRun confirmMessageDialog
  when (confirmMessageDialogResponse == enumToInt32 GI.Gtk.ResponseTypeYes) $ do
    textOverlays <- readIORef textOverlaysRef
    mapM_
      (\ GR.GuiTextOverlayComponents { GR.textOverlayBox } -> do
        GI.Gtk.containerRemove textOverlaysBox textOverlayBox
        GI.Gtk.widgetDestroy   textOverlayBox
      )
      textOverlays
    atomicModifyIORef' textOverlaysRef
      $ const ([], ())
    GI.Gtk.widgetHide textOverlaysRemoveAllButton

handleOpenDialogResponse
  ::  GR.GuiComponents
  ->  Int32
  ->  IO ()
handleOpenDialogResponse
  guiComponents@GR.GuiComponents
    { GR.textOverlaysOpenDialog
    , GR.statusLabel
    , GR.guiInFilePropertiesRef
    }
  responseId
  = do
  GI.Gtk.widgetHide textOverlaysOpenDialog
  when (enumToInt32 GI.Gtk.ResponseTypeOk == responseId) $ do
    maybeTextOverlaysFilePath <-
      fileChooserGetExistingFilePath textOverlaysOpenDialog
    case maybeTextOverlaysFilePath of
      Just textOverlaysFilePath -> do
        GR.GuiInFileProperties
          { GR.inFileWidth
          , GR.inFileHeight
          } <- readIORef guiInFilePropertiesRef
        textOverlays <-
          Gifcurry.convertFileToTextOverlays
            textOverlaysFilePath
            (Just (inFileWidth, inFileHeight))
        when (null textOverlays) $ do
          GI.Gtk.labelSetMarkup
            statusLabel $
              Data.Text.concat
                [ "Didn't find any text. "
                , "If you think it's a bug, please open an "
                , "<a href=\"https://github.com/lettier/gifcurry/issues\">issue</a>"
                , "."
                ]
          updateStatusLabelAsync statusLabel 5000 "Ready."
        mapM_
          (addTextOverlay guiComponents . Just)
          textOverlays
      _ -> return ()

handleSaveDialogResponse
  ::  GR.GuiComponents
  ->  Int32
  ->  IO ()
handleSaveDialogResponse
  guiComponents@GR.GuiComponents
    { GR.textOverlaysSaveDialog
    }
  responseId
  = do
  GI.Gtk.widgetHide textOverlaysSaveDialog
  when (enumToInt32 GI.Gtk.ResponseTypeOk == responseId) $ do
    gifcurryTextOverlays <- getGifcurryTextOverlays guiComponents False
    textOverlaysFilePath <- fileChooserGetFilePath  textOverlaysSaveDialog
    let textOverlaysFilePath' =
          if hasFileExtension textOverlaysFilePath ".yaml"
            then textOverlaysFilePath
            else textOverlaysFilePath ++ ".yaml"
    Gifcurry.saveTextOverlaysToFile
      textOverlaysFilePath'
      gifcurryTextOverlays

getGifcurryTextOverlays
  ::  GR.GuiComponents
  ->  Bool
  ->  IO [Gifcurry.TextOverlay]
getGifcurryTextOverlays
  guiComponents
  scaleFontSize
  = do
  guiTextOverlaysData  <- getTextOverlaysData guiComponents
  (_, previewWidth, _) <- getPreviewDurationWidthAndHeight guiComponents
  gifWidth             <- getGifWidth guiComponents
  gifcurryTextOverlays <- mapM
                            ( getGifcurryTextOverlay
                                previewWidth
                                gifWidth
                                scaleFontSize
                            )
                            guiTextOverlaysData
  return
    $ filter
      (not . null . Gifcurry.textOverlayText)
      gifcurryTextOverlays

getGifcurryTextOverlay
  ::  Double
  ->  Double
  ->  Bool
  ->  GR.GuiTextOverlayData
  ->  IO Gifcurry.TextOverlay
getGifcurryTextOverlay
  previewWidth
  gifWidth
  scaleFontSize
  GR.GuiTextOverlayData
    { GR.textOverlayText
    , GR.textOverlayHorizontal
    , GR.textOverlayVertical
    , GR.textOverlayStartTime
    , GR.textOverlayEndTime
    , GR.textOverlayRotation
    , GR.textOverlayOutlineSize
    , GR.textOverlayOutlineColor
    , GR.textOverlayFillColor
    , GR.textOverlayMaybeFontDesc
    }
  = do
  newFontDesc      <- GRPF.fontDescriptionNew
  let fontDesc     = fromMaybe newFontDesc textOverlayMaybeFontDesc
  fontFamily       <-                       fromMaybe ""                 <$> GRPF.fontDescriptionGetFamily  fontDesc
  fontStyle        <- removeQuotes . show . fromMaybe GRPF.StyleNormal   <$> GRPF.fontDescriptionGetStyle   fontDesc
  fontStretch      <- removeQuotes . show . fromMaybe GRPF.StretchNormal <$> GRPF.fontDescriptionGetStretch fontDesc
  fontWeight       <-                                                        getFontWeight                  fontDesc
  fontSize         <-                       fromMaybe 30.0               <$> GRPF.fontDescriptionGetSize    fontDesc
  let fontSize'    = doubleToInt $ fontSize * if scaleFontSize then gifWidth / previewWidth else 1
  let xTranslate   = textOverlayHorizontal
  let yTranslate   = textOverlayVertical
  let startTime    = textOverlayStartTime
  let endTime      = textOverlayEndTime
  let rotation     = int32ToInt textOverlayRotation
  let outlineSize  = int32ToInt textOverlayOutlineSize
  return
    Gifcurry.TextOverlay
      { Gifcurry.textOverlayText         = textOverlayText
      , Gifcurry.textOverlayFontFamily   = fontFamily
      , Gifcurry.textOverlayFontStyle    = fontStyle
      , Gifcurry.textOverlayFontStretch  = fontStretch
      , Gifcurry.textOverlayFontWeight   = fontWeight
      , Gifcurry.textOverlayFontSize     = fontSize'
      , Gifcurry.textOverlayOrigin       = Gifcurry.TextOverlayOriginCenter
      , Gifcurry.textOverlayXTranslation = xTranslate
      , Gifcurry.textOverlayYTranslation = yTranslate
      , Gifcurry.textOverlayRotation     = rotation
      , Gifcurry.textOverlayStartTime    = startTime
      , Gifcurry.textOverlayEndTime      = endTime
      , Gifcurry.textOverlayOutlineSize  = outlineSize
      , Gifcurry.textOverlayOutlineColor = textOverlayOutlineColor
      , Gifcurry.textOverlayFillColor    = textOverlayFillColor
      }
  where
    -- `show` adds extra quotes to GRPF.Style* and GRPF.Stretch
    removeQuotes :: String -> String
    removeQuotes = foldl (\ xs x -> if x /= '\"' then xs ++ [x] else xs) ""
    getFontWeight :: GRPF.FontDescription -> IO Int
    getFontWeight fontDesc =
      catch
        getFontWeight'
        (\ msg -> do
          -- Some font weights, like 860, are not supported by the library.
          putStrLn $ "[ERROR] " ++ show (msg :: SomeException)
          return $ fromEnum GRPF.WeightNormal
        )
      where
        getFontWeight' :: IO Int
        getFontWeight' = do
          !fontWeight <- fromEnum . fromMaybe GRPF.WeightNormal <$> GRPF.fontDescriptionGetWeight fontDesc
          return fontWeight

getTextOverlaysData
  ::  GR.GuiComponents
  ->  IO [GR.GuiTextOverlayData]
getTextOverlaysData
  GR.GuiComponents
    { GR.textOverlaysRef
    }
  = do
  textOverlays <- readIORef textOverlaysRef
  mapM getTextOverlayData textOverlays

getTextOverlayData
  ::  GR.GuiTextOverlayComponents
  ->  IO GR.GuiTextOverlayData
getTextOverlayData
  GR.GuiTextOverlayComponents
    { GR.textOverlayVisibilityToggleButton
    , GR.textOverlayTextEntry
    , GR.textOverlayHorizontalSpinButton
    , GR.textOverlayVerticalSpinButton
    , GR.textOverlayStartTimeSpinButton
    , GR.textOverlayEndTimeSpinButton
    , GR.textOverlayRotationSpinButton
    , GR.textOverlayOutlineSizeSpinButton
    , GR.textOverlayOutlineColorButton
    , GR.textOverlayFillColorButton
    , GR.textOverlayFontButton
    }
  = do
  controlsVisible <- GI.Gtk.getToggleButtonActive textOverlayVisibilityToggleButton
  text            <- Data.Text.unpack <$> GI.Gtk.entryGetText textOverlayTextEntry
  horizontal      <- GI.Gtk.spinButtonGetValue textOverlayHorizontalSpinButton
  vertical        <- GI.Gtk.spinButtonGetValue textOverlayVerticalSpinButton
  start           <- GI.Gtk.spinButtonGetValue textOverlayStartTimeSpinButton
  end             <- GI.Gtk.spinButtonGetValue textOverlayEndTimeSpinButton
  rotation        <- GI.Gtk.spinButtonGetValueAsInt textOverlayRotationSpinButton
  outlineSize     <- GI.Gtk.spinButtonGetValueAsInt textOverlayOutlineSizeSpinButton
  outlineColor    <- getColorButtonString textOverlayOutlineColorButton "rgba(0,0,0,1.0)"
  fillColor       <- getColorButtonString textOverlayFillColorButton    "rgba(255,255,255,1.0)"
  maybeFontDesc   <- GI.Gtk.fontChooserGetFontDesc textOverlayFontButton
  maybeFontDesc'  <-
    case maybeFontDesc of
      Nothing -> return Nothing
      Just fd -> do
        fds <- Data.Text.unpack <$> GI.Pango.fontDescriptionToString fd
        fd' <- GRPF.fontDescriptionFromString fds
        return $ Just fd'
  return
    GR.GuiTextOverlayData
      { GR.textOverlayControlsVisible = controlsVisible
      , GR.textOverlayText            = text
      , GR.textOverlayHorizontal      = horizontal
      , GR.textOverlayVertical        = vertical
      , GR.textOverlayStartTime       = start
      , GR.textOverlayEndTime         = end
      , GR.textOverlayRotation        = rotation
      , GR.textOverlayOutlineSize     = outlineSize
      , GR.textOverlayOutlineColor    = outlineColor
      , GR.textOverlayFillColor       = fillColor
      , GR.textOverlayMaybeFontDesc   = maybeFontDesc'
      }

updateTextOverlays :: Bool -> GR.GuiComponents -> IO ()
updateTextOverlays
  reset
  guiComponents@GR.GuiComponents
    { GR.textOverlaysRef
    }
  = do
  textOverlays                <- readIORef textOverlaysRef
  (duration, _width, _height) <- getPreviewDurationWidthAndHeight guiComponents
  mapM_ updateRotationOutlineSizeButtons textOverlays
  mapM_ updatePositionSpinButtons        textOverlays
  mapM_ (updateTimeSpinButtons duration) textOverlays
  when reset $
    mapM_ clearEntry textOverlays
  where
    clearEntry :: GR.GuiTextOverlayComponents -> IO ()
    clearEntry
      GR.GuiTextOverlayComponents
        { GR.textOverlayTextEntry
        }
      = GI.Gtk.entrySetText textOverlayTextEntry ""
    updateRotationOutlineSizeButtons :: GR.GuiTextOverlayComponents -> IO ()
    updateRotationOutlineSizeButtons
      GR.GuiTextOverlayComponents
        { GR.textOverlayRotationSpinButton
        , GR.textOverlayOutlineSizeSpinButton
        }
      = do
      rotation <- GI.Gtk.spinButtonGetValue textOverlayRotationSpinButton
      GI.Gtk.entrySetProgressFraction
        textOverlayRotationSpinButton
          $ rotation / 360.0
      outlineSize <- GI.Gtk.spinButtonGetValue textOverlayOutlineSizeSpinButton
      GI.Gtk.entrySetProgressFraction
        textOverlayOutlineSizeSpinButton
          $ outlineSize / 10.0
      when reset $ do
        GI.Gtk.spinButtonSetValue textOverlayRotationSpinButton     0.0
        GI.Gtk.spinButtonSetValue textOverlayOutlineSizeSpinButton 10.0
        GI.Gtk.entrySetProgressFraction
          textOverlayRotationSpinButton
          0.0
        GI.Gtk.entrySetProgressFraction
          textOverlayOutlineSizeSpinButton
          10.0
    updatePositionSpinButtons :: GR.GuiTextOverlayComponents -> IO ()
    updatePositionSpinButtons
      GR.GuiTextOverlayComponents
        { GR.textOverlayHorizontalSpinButton
        , GR.textOverlayVerticalSpinButton
        }
      = do
      GI.Gtk.spinButtonSetRange
        textOverlayHorizontalSpinButton
        (-0.5)
        0.5
      GI.Gtk.spinButtonSetRange
        textOverlayVerticalSpinButton
        (-0.5)
        0.5
      when reset $ do
        GI.Gtk.spinButtonSetValue textOverlayHorizontalSpinButton 0.0
        GI.Gtk.spinButtonSetValue textOverlayVerticalSpinButton   0.0
      horizontal <- GI.Gtk.spinButtonGetValue textOverlayVerticalSpinButton
      GI.Gtk.entrySetProgressFraction
        textOverlayHorizontalSpinButton
          $ horizontal + 0.5
      vertical   <- GI.Gtk.spinButtonGetValue textOverlayVerticalSpinButton
      GI.Gtk.entrySetProgressFraction
        textOverlayVerticalSpinButton
          $ vertical + 0.5
    updateTimeSpinButtons :: Double -> GR.GuiTextOverlayComponents -> IO ()
    updateTimeSpinButtons
      duration
      GR.GuiTextOverlayComponents
        { GR.textOverlayStartTimeSpinButton
        , GR.textOverlayEndTimeSpinButton
        }
      = do
      GI.Gtk.spinButtonSetRange
        textOverlayStartTimeSpinButton
        0.0
        duration
      if reset
        then do
          GI.Gtk.spinButtonSetRange
            textOverlayEndTimeSpinButton
            0.0
            duration
          GI.Gtk.spinButtonSetValue
            textOverlayStartTimeSpinButton
            0.0
          GI.Gtk.spinButtonSetValue
            textOverlayEndTimeSpinButton $
              truncatePastDigit duration 2
          GI.Gtk.entrySetProgressFraction
            textOverlayStartTimeSpinButton
            0.0
          GI.Gtk.entrySetProgressFraction
            textOverlayEndTimeSpinButton
            1.0
        else do
          startTime <- GI.Gtk.spinButtonGetValue textOverlayStartTimeSpinButton
          endTime   <- GI.Gtk.spinButtonGetValue textOverlayEndTimeSpinButton
          GI.Gtk.spinButtonSetRange
            textOverlayEndTimeSpinButton
            startTime
            duration
          GI.Gtk.entrySetProgressFraction
            textOverlayStartTimeSpinButton
              $ fromMaybe 0.0 $ safeDivide startTime duration
          GI.Gtk.entrySetProgressFraction
            textOverlayEndTimeSpinButton
              $ fromMaybe 0.0 $ safeDivide endTime duration

addTextOverlay :: GR.GuiComponents -> Maybe Gifcurry.TextOverlay -> IO ()
addTextOverlay
  guiComponents@GR.GuiComponents
    { GR.textOverlaysRemoveAllButton
    , GR.textOverlaysBox
    , GR.confirmMessageDialog
    , GR.textOverlaysRef
    }
  maybeGifcurryTextOverlay
  = do
  (duration, width, height) <- getPreviewDurationWidthAndHeight guiComponents
  when (duration > 0.0 && width > 0.0 && height > 0.0) $ do
    let isDefaultGifcurryTextOverlay  = isNothing maybeGifcurryTextOverlay
    let gifcurryTextOverlay           = fromMaybe Gifcurry.defaultTextOverlay maybeGifcurryTextOverlay
    isGtkVersionGte318                <- isGtkVersionGte 3 18
    tIconFilePath                     <- getDataFileName "data/t-icon.svg"
    upIconFilePath                    <- getDataFileName "data/up-icon.svg"
    downIconFilePath                  <- getDataFileName "data/down-icon.svg"
    leftRightIconFilePath             <- getDataFileName "data/left-right-icon.svg"
    upDownIconFilePath                <- getDataFileName "data/up-down-icon.svg"
    cloneIconFilePath                 <- getDataFileName "data/clone-icon.svg"
    startIconFilePath                 <- getDataFileName "data/start-icon.svg"
    endIconFilePath                   <- getDataFileName "data/end-icon.svg"
    spiralIconFilePath                <- getDataFileName "data/spiral-icon.svg"
    widthIconFilePath                 <- getDataFileName "data/width-icon.svg"
    minusIconFilePath                 <- getDataFileName "data/minus-icon.svg"
    leftRightIconPixbuf               <- GI.GdkPixbuf.pixbufNewFromFile leftRightIconFilePath
    upDownIconPixbuf                  <- GI.GdkPixbuf.pixbufNewFromFile upDownIconFilePath
    startIconPixbuf                   <- GI.GdkPixbuf.pixbufNewFromFile startIconFilePath
    endIconPixbuf                     <- GI.GdkPixbuf.pixbufNewFromFile endIconFilePath
    spiralIconPixbuf                  <- GI.GdkPixbuf.pixbufNewFromFile spiralIconFilePath
    widthIconPixbuf                   <- GI.GdkPixbuf.pixbufNewFromFile widthIconFilePath
    tIconImage                        <- GI.Gtk.imageNewFromFile tIconFilePath
    minusIconImage                    <- GI.Gtk.imageNewFromFile minusIconFilePath
    upIconImage                       <- GI.Gtk.imageNewFromFile upIconFilePath
    downIconImage                     <- GI.Gtk.imageNewFromFile downIconFilePath
    cloneIconImage                    <- GI.Gtk.imageNewFromFile cloneIconFilePath
    box                               <- GI.Gtk.boxNew GI.Gtk.OrientationVertical   0
    visibilityBox                     <- GI.Gtk.boxNew GI.Gtk.OrientationVertical   0
    positionSpinButtonsBox            <- GI.Gtk.boxNew GI.Gtk.OrientationHorizontal 0
    timeSpinButtonsBox                <- GI.Gtk.boxNew GI.Gtk.OrientationHorizontal 0
    rotationOutlineSizeSpinButtonsBox <- GI.Gtk.boxNew GI.Gtk.OrientationHorizontal 0
    colorButtonsBox                   <- GI.Gtk.boxNew GI.Gtk.OrientationHorizontal 0
    orderButtonsBox                   <- GI.Gtk.boxNew GI.Gtk.OrientationHorizontal 0
    cloneRemoveButtonsBox             <- GI.Gtk.boxNew GI.Gtk.OrientationHorizontal 0
    horizontalAdjustment              <- GI.Gtk.adjustmentNew 0.0 0.0   0.0 0.01 0.0 0.0
    verticalAdjustment                <- GI.Gtk.adjustmentNew 0.0 0.0   0.0 0.01 0.0 0.0
    startTimeAdjustment               <- GI.Gtk.adjustmentNew 0.0 0.0   0.0 1.0  0.0 0.0
    endTimeAdjustment                 <- GI.Gtk.adjustmentNew 0.0 0.0   0.0 1.0  0.0 0.0
    rotationAdjustment                <- GI.Gtk.adjustmentNew 0.0 0.0 360.0 1.0  0.0 0.0
    outlineSizeAdjustment             <- GI.Gtk.adjustmentNew 0.0 0.0  10.0 1.0  0.0 0.0
    horizontalSpinButton              <- GI.Gtk.spinButtonNew (Just horizontalAdjustment)  1.0 2
    verticalSpinButton                <- GI.Gtk.spinButtonNew (Just verticalAdjustment)    1.0 2
    startTimeSpinButton               <- GI.Gtk.spinButtonNew (Just startTimeAdjustment)   1.0 2
    endTimeSpinButton                 <- GI.Gtk.spinButtonNew (Just endTimeAdjustment)     1.0 2
    rotationSpinButton                <- GI.Gtk.spinButtonNew (Just rotationAdjustment)    1.0 0
    outlineSizeSpinButton             <- GI.Gtk.spinButtonNew (Just outlineSizeAdjustment) 1.0 0
    textEntry                         <- GI.Gtk.entryNew
    visibilityToggleButton            <- GI.Gtk.toggleButtonNew
    fontButton                        <- GI.Gtk.fontButtonNew
    blackRgba                         <- GI.Gdk.newZeroRGBA
    whiteRgba                         <- GI.Gdk.newZeroRGBA
    _                                 <- GI.Gdk.rGBAParse blackRgba "rgba(0,0,0,1.0)"
    _                                 <- GI.Gdk.rGBAParse whiteRgba "rgba(255,255,255,1.0)"
    outlineColorButton                <- GI.Gtk.colorButtonNewWithRgba blackRgba
    fillColorButton                   <- GI.Gtk.colorButtonNewWithRgba whiteRgba
    orderUpButton                     <- GI.Gtk.buttonNewWithLabel "Raise"
    orderDownButton                   <- GI.Gtk.buttonNewWithLabel "Lower"
    cloneButton                       <- GI.Gtk.buttonNewWithLabel "Clone"
    removeButton                      <- GI.Gtk.buttonNewFromIconName (Just "gtk-remove") (enumToInt32 GI.Gtk.IconSizeButton)
    fontDescription                   <- GI.Pango.fontDescriptionFromString "Sans Regular 30"
    GI.Gtk.setToggleButtonDrawIndicator visibilityToggleButton            False
    GI.Gtk.setToggleButtonActive        visibilityToggleButton            True
    GI.Gtk.buttonSetImage               visibilityToggleButton            (Just tIconImage)
    GI.Gtk.buttonSetImage               orderUpButton                     (Just upIconImage)
    GI.Gtk.buttonSetImage               orderDownButton                   (Just downIconImage)
    GI.Gtk.buttonSetImage               cloneButton                       (Just cloneIconImage)
    GI.Gtk.buttonSetImage               removeButton                      (Just minusIconImage)
    GI.Gtk.setEntryPrimaryIconPixbuf    horizontalSpinButton              leftRightIconPixbuf
    GI.Gtk.setEntryPrimaryIconPixbuf    verticalSpinButton                upDownIconPixbuf
    GI.Gtk.setEntryPrimaryIconPixbuf    startTimeSpinButton               startIconPixbuf
    GI.Gtk.setEntryPrimaryIconPixbuf    endTimeSpinButton                 endIconPixbuf
    GI.Gtk.setEntryPrimaryIconPixbuf    rotationSpinButton                spiralIconPixbuf
    GI.Gtk.setEntryPrimaryIconPixbuf    outlineSizeSpinButton             widthIconPixbuf
    GI.Gtk.setWidgetDoubleBuffered      visibilityToggleButton            True
    GI.Gtk.setButtonAlwaysShowImage     visibilityToggleButton            True
    GI.Gtk.fontChooserSetFontDesc       fontButton                        fontDescription
    GI.Gtk.buttonSetLabel               removeButton                      "Remove"
    GI.Gtk.setButtonAlwaysShowImage     orderUpButton                     True
    GI.Gtk.setButtonAlwaysShowImage     orderDownButton                   True
    GI.Gtk.setButtonAlwaysShowImage     cloneButton                       True
    GI.Gtk.setButtonAlwaysShowImage     removeButton                      True
    GI.Gtk.boxSetHomogeneous            positionSpinButtonsBox            True
    GI.Gtk.boxSetHomogeneous            timeSpinButtonsBox                True
    GI.Gtk.boxSetHomogeneous            rotationOutlineSizeSpinButtonsBox True
    GI.Gtk.boxSetHomogeneous            colorButtonsBox                   True
    GI.Gtk.boxSetHomogeneous            orderButtonsBox                   True
    GI.Gtk.boxSetHomogeneous            cloneRemoveButtonsBox             True
    if isGtkVersionGte318
      then do
        GI.Gtk.widgetSetMarginEnd       tIconImage                        5
        GI.Gtk.widgetSetMarginEnd       minusIconImage                    5
        GI.Gtk.widgetSetMarginEnd       upIconImage                       5
        GI.Gtk.widgetSetMarginEnd       downIconImage                     5
        GI.Gtk.widgetSetMarginEnd       cloneIconImage                    5
      else do
        -- To support GTK 3.10, Ubuntu 14.04.
        GI.Gtk.widgetSetMarginRight     tIconImage                        5
        GI.Gtk.widgetSetMarginRight     minusIconImage                    5
        GI.Gtk.widgetSetMarginRight     upIconImage                       5
        GI.Gtk.widgetSetMarginRight     downIconImage                     5
        GI.Gtk.widgetSetMarginRight     cloneIconImage                    5
    GI.Gtk.widgetSetTooltipText         visibilityToggleButton            $ Just "Close this text?"
    GI.Gtk.widgetSetTooltipText         horizontalSpinButton              $ Just "How much to the left or right?"
    GI.Gtk.widgetSetTooltipText         verticalSpinButton                $ Just "How much going up or down?"
    GI.Gtk.widgetSetTooltipText         startTimeSpinButton               $ Just "What is the start time?"
    GI.Gtk.widgetSetTooltipText         endTimeSpinButton                 $ Just "What is the end time?"
    GI.Gtk.widgetSetTooltipText         rotationSpinButton                $ Just "Rotate by how much?"
    GI.Gtk.widgetSetTooltipText         outlineSizeSpinButton             $ Just "How thick is the outline size?"
    GI.Gtk.widgetSetTooltipText         outlineColorButton                $ Just "What is the outline color?"
    GI.Gtk.widgetSetTooltipText         fillColorButton                   $ Just "What is the fill color?"
    GI.Gtk.widgetSetTooltipText         orderUpButton                     $ Just "Raise this text up?"
    GI.Gtk.widgetSetTooltipText         orderDownButton                   $ Just "Lower this text down?"
    GI.Gtk.widgetSetTooltipText         cloneButton                       $ Just "Clone this text?"
    GI.Gtk.widgetSetTooltipText         removeButton                      $ Just "Remove this text?"
    GI.Gtk.entrySetPlaceholderText      textEntry                         $ Just "What is the text?"
    GI.Gtk.spinButtonSetNumeric horizontalSpinButton                      True
    GI.Gtk.spinButtonSetNumeric verticalSpinButton                        True
    GI.Gtk.spinButtonSetNumeric startTimeSpinButton                       True
    GI.Gtk.spinButtonSetNumeric endTimeSpinButton                         True
    GI.Gtk.spinButtonSetNumeric rotationSpinButton                        True
    GI.Gtk.spinButtonSetNumeric outlineSizeSpinButton                     True
    GI.Gtk.spinButtonSetUpdatePolicy horizontalSpinButton                 GI.Gtk.SpinButtonUpdatePolicyIfValid
    GI.Gtk.spinButtonSetUpdatePolicy verticalSpinButton                   GI.Gtk.SpinButtonUpdatePolicyIfValid
    GI.Gtk.spinButtonSetUpdatePolicy startTimeSpinButton                  GI.Gtk.SpinButtonUpdatePolicyIfValid
    GI.Gtk.spinButtonSetUpdatePolicy endTimeSpinButton                    GI.Gtk.SpinButtonUpdatePolicyIfValid
    GI.Gtk.spinButtonSetUpdatePolicy rotationSpinButton                   GI.Gtk.SpinButtonUpdatePolicyIfValid
    GI.Gtk.spinButtonSetUpdatePolicy outlineSizeSpinButton                GI.Gtk.SpinButtonUpdatePolicyIfValid
    GI.Gtk.spinButtonSetRange horizontalSpinButton                        (-0.5) 0.5
    GI.Gtk.spinButtonSetRange verticalSpinButton                          (-0.5) 0.5
    GI.Gtk.spinButtonSetRange startTimeSpinButton                         0.0    duration
    GI.Gtk.spinButtonSetRange endTimeSpinButton                           0.0    duration
    GI.Gtk.spinButtonSetRange rotationSpinButton                          0.0    360.0
    GI.Gtk.spinButtonSetRange outlineSizeSpinButton                       0.0    10.0
    GI.Gtk.spinButtonSetValue horizontalSpinButton                        0.0
    GI.Gtk.spinButtonSetValue verticalSpinButton                          0.0
    GI.Gtk.spinButtonSetValue startTimeSpinButton                         0.0
    GI.Gtk.spinButtonSetValue endTimeSpinButton                           duration
    GI.Gtk.spinButtonSetValue rotationSpinButton                          0.0
    GI.Gtk.spinButtonSetValue outlineSizeSpinButton                       10.0
    GI.Gtk.entrySetProgressFraction horizontalSpinButton                  0.5
    GI.Gtk.entrySetProgressFraction verticalSpinButton                    0.5
    GI.Gtk.entrySetProgressFraction startTimeSpinButton                   0.0
    GI.Gtk.entrySetProgressFraction endTimeSpinButton                     1.0
    GI.Gtk.entrySetProgressFraction rotationSpinButton                    0.0
    GI.Gtk.entrySetProgressFraction outlineSizeSpinButton                 1.0
    GI.Gtk.widgetSetMarginTop    visibilityToggleButton                   0
    GI.Gtk.widgetSetMarginBottom removeButton                             0
    widgetAddStyleClass          removeButton                             "gifcurry-button-dangerous"
    GI.Gtk.widgetShow box
    GI.Gtk.widgetShow visibilityBox
    GI.Gtk.widgetShow visibilityToggleButton
    GI.Gtk.widgetShow positionSpinButtonsBox
    GI.Gtk.widgetShow timeSpinButtonsBox
    GI.Gtk.widgetShow rotationOutlineSizeSpinButtonsBox
    GI.Gtk.widgetShow colorButtonsBox
    GI.Gtk.widgetShow orderButtonsBox
    GI.Gtk.widgetShow cloneRemoveButtonsBox
    GI.Gtk.widgetShow horizontalSpinButton
    GI.Gtk.widgetShow verticalSpinButton
    GI.Gtk.widgetShow startTimeSpinButton
    GI.Gtk.widgetShow endTimeSpinButton
    GI.Gtk.widgetShow rotationSpinButton
    GI.Gtk.widgetShow outlineSizeSpinButton
    GI.Gtk.widgetShow outlineColorButton
    GI.Gtk.widgetShow fillColorButton
    GI.Gtk.widgetShow textEntry
    GI.Gtk.widgetShow fontButton
    GI.Gtk.widgetShow orderUpButton
    GI.Gtk.widgetShow orderDownButton
    GI.Gtk.widgetShow cloneButton
    GI.Gtk.widgetShow cloneButton
    GI.Gtk.widgetShow removeButton
    GI.Gtk.containerAdd positionSpinButtonsBox            horizontalSpinButton
    GI.Gtk.containerAdd positionSpinButtonsBox            verticalSpinButton
    GI.Gtk.containerAdd timeSpinButtonsBox                startTimeSpinButton
    GI.Gtk.containerAdd timeSpinButtonsBox                endTimeSpinButton
    GI.Gtk.containerAdd rotationOutlineSizeSpinButtonsBox rotationSpinButton
    GI.Gtk.containerAdd rotationOutlineSizeSpinButtonsBox outlineSizeSpinButton
    GI.Gtk.containerAdd colorButtonsBox                   outlineColorButton
    GI.Gtk.containerAdd colorButtonsBox                   fillColorButton
    GI.Gtk.containerAdd orderButtonsBox                   orderUpButton
    GI.Gtk.containerAdd orderButtonsBox                   orderDownButton
    GI.Gtk.containerAdd cloneRemoveButtonsBox             cloneButton
    GI.Gtk.containerAdd cloneRemoveButtonsBox             removeButton
    GI.Gtk.containerAdd visibilityBox                     positionSpinButtonsBox
    GI.Gtk.containerAdd visibilityBox                     timeSpinButtonsBox
    GI.Gtk.containerAdd visibilityBox                     rotationOutlineSizeSpinButtonsBox
    GI.Gtk.containerAdd visibilityBox                     colorButtonsBox
    GI.Gtk.containerAdd visibilityBox                     textEntry
    GI.Gtk.containerAdd visibilityBox                     fontButton
    GI.Gtk.containerAdd visibilityBox                     orderButtonsBox
    GI.Gtk.containerAdd visibilityBox                     cloneRemoveButtonsBox
    GI.Gtk.containerAdd box                               visibilityToggleButton
    GI.Gtk.containerAdd box                               visibilityBox
    GI.Gtk.containerAdd textOverlaysBox                   box

    textOverlays <- readIORef textOverlaysRef
    let ids = sort $ map GR.textOverlayId textOverlays
    let (_, minId) =
          foldl
            (\ (i, m) i' -> if i == i' then (i + 1, i + 1) else (i + 1, m))
            (0, 0)
            ids

    let textOverlay =
          GR.GuiTextOverlayComponents
            { GR.textOverlayId                      = minId
            , GR.textOverlayBox                     = box
            , GR.textOverlayVisibilityBox           = visibilityBox
            , GR.textOverlayVisibilityToggleButton  = visibilityToggleButton
            , GR.textOverlayHorizontalSpinButton    = horizontalSpinButton
            , GR.textOverlayVerticalSpinButton      = verticalSpinButton
            , GR.textOverlayStartTimeSpinButton     = startTimeSpinButton
            , GR.textOverlayEndTimeSpinButton       = endTimeSpinButton
            , GR.textOverlayRotationSpinButton      = rotationSpinButton
            , GR.textOverlayOutlineSizeSpinButton   = outlineSizeSpinButton
            , GR.textOverlayOutlineColorButton      = outlineColorButton
            , GR.textOverlayFillColorButton         = fillColorButton
            , GR.textOverlayTextEntry               = textEntry
            , GR.textOverlayFontButton              = fontButton
            , GR.textOverlayCloneButton             = cloneButton
            , GR.textOverlayRemoveButton            = removeButton
            , GR.textOverlayOrderUpButton           = orderUpButton
            , GR.textOverlayOrderDownButton         = orderDownButton
            }

    unless isDefaultGifcurryTextOverlay
      $ populateTextOverlay
        textOverlay
        gifcurryTextOverlay
        duration
        width
        height
        False

    atomicModifyIORef' textOverlaysRef
      $ \ textOverlays' ->
        ( textOverlays' ++ [textOverlay]
        , ()
        )

    _ <- GI.Gtk.onButtonClicked cloneButton $ do
      gifWidth             <- getGifWidth guiComponents
      textOverlayData      <- getTextOverlayData textOverlay
      gifcurryTextOverlay' <- getGifcurryTextOverlay width gifWidth False textOverlayData
      addTextOverlay guiComponents (Just gifcurryTextOverlay')

    _ <- GI.Gtk.onButtonClicked removeButton $ do
      GI.Gtk.setMessageDialogText
        confirmMessageDialog
        "Remove this text?"
      confirmMessageDialogResponse <- GI.Gtk.dialogRun confirmMessageDialog
      when (confirmMessageDialogResponse == enumToInt32 GI.Gtk.ResponseTypeYes) $ do
        textOverlays' <- filterTextOverlay minId <$> readIORef textOverlaysRef
        atomicModifyIORef' textOverlaysRef
          $ const (textOverlays', ())
        GI.Gtk.containerRemove textOverlaysBox box
        GI.Gtk.widgetDestroy   box
        if length textOverlays' > 1
          then GI.Gtk.widgetShow textOverlaysRemoveAllButton
          else GI.Gtk.widgetHide textOverlaysRemoveAllButton

    _ <- GI.Gtk.onButtonClicked orderUpButton $ do
      textOverlays' <- readIORef textOverlaysRef
      let index = getTextOverlayIndex textOverlays' minId
      when (index > 0) $ do
        let indexBefore        = index - 1
        let indexAfter         = index + 1
        let textOverlaysBefore = take indexBefore textOverlays'
        let textOverlayBefore  = textOverlays'!!indexBefore
        let textOverlay'       = textOverlays'!!index
        let textOverlaysAfter  = drop indexAfter textOverlays'
        let textOverlays''     =    textOverlaysBefore
                                 ++ [textOverlay']
                                 ++ [textOverlayBefore]
                                 ++ textOverlaysAfter
        atomicModifyIORef' textOverlaysRef
          $ const
            ( textOverlays''
            , ()
            )
        GI.Gtk.boxReorderChild textOverlaysBox box (intToInt32 indexBefore)
        hideAllOtherTextOverlays guiComponents minId
        return ()

    _ <- GI.Gtk.onButtonClicked orderDownButton $ do
      textOverlays' <- readIORef textOverlaysRef
      let index = getTextOverlayIndex textOverlays' minId
      when (index < length textOverlays' - 1) $ do
        let indexAfter         = index + 1
        let textOverlaysBefore = take index textOverlays'
        let textOverlayAfter   = textOverlays'!!indexAfter
        let textOverlay'       = textOverlays'!!index
        let textOverlaysAfter  = drop (indexAfter + 1) textOverlays'
        let textOverlays''     =    textOverlaysBefore
                                 ++ [textOverlayAfter]
                                 ++ [textOverlay']
                                 ++ textOverlaysAfter
        atomicModifyIORef' textOverlaysRef
          $ const
            ( textOverlays''
            , ()
            )
        GI.Gtk.boxReorderChild textOverlaysBox box (intToInt32 indexAfter)
        hideAllOtherTextOverlays guiComponents minId
        return ()

    _ <- GI.Gtk.onWidgetButtonReleaseEvent visibilityToggleButton $ \ _ -> do
      active <- GI.Gtk.getToggleButtonActive visibilityToggleButton
      if active
        then hideAllOtherTextOverlays guiComponents (-1)
        else hideAllOtherTextOverlays guiComponents minId
      return True

    _ <- GI.Gtk.afterEditableChanged textEntry $
      updateVisibilityToggleButtonLabel visibilityToggleButton textEntry

    updateTextOverlays False guiComponents
    hideAllOtherTextOverlays guiComponents minId

    if length textOverlays + 1 > 1
      then GI.Gtk.widgetShow textOverlaysRemoveAllButton
      else GI.Gtk.widgetHide textOverlaysRemoveAllButton

    return ()
  where
    getTextOverlayIndex
      :: [GR.GuiTextOverlayComponents]
      -> Int
      -> Int
    getTextOverlayIndex
      textOverlays
      textOverlayId'
      =
      snd
        $ foldl
          (\ (x, i) GR.GuiTextOverlayComponents { GR.textOverlayId } ->
            if textOverlayId == textOverlayId'
              then (x + 1, x + 1)
              else (x + 1, i)
          )
          (-1, -1)
          textOverlays
    updateVisibilityToggleButtonLabel
      ::  GI.Gtk.ToggleButton
      ->  GI.Gtk.Entry
      ->  IO ()
    updateVisibilityToggleButtonLabel
      visibilityToggleButton
      textEntry
      = do
      text      <- GI.Gtk.entryGetText textEntry
      let limit = 27
      let label =
            if Data.Text.length text >= limit
              then Data.Text.concat [Data.Text.take limit text, "..."]
              else text
      GI.Gtk.buttonSetLabel
        visibilityToggleButton
        label
    populateTextOverlay
      ::  GR.GuiTextOverlayComponents
      ->  Gifcurry.TextOverlay
      ->  Double
      ->  Double
      ->  Double
      ->  Bool
      ->  IO ()
    populateTextOverlay
      GR.GuiTextOverlayComponents
        { GR.textOverlayHorizontalSpinButton
        , GR.textOverlayVerticalSpinButton
        , GR.textOverlayStartTimeSpinButton
        , GR.textOverlayEndTimeSpinButton
        , GR.textOverlayTextEntry
        , GR.textOverlayRotationSpinButton
        , GR.textOverlayOutlineSizeSpinButton
        , GR.textOverlayOutlineColorButton
        , GR.textOverlayFillColorButton
        , GR.textOverlayVisibilityToggleButton
        , GR.textOverlayFontButton
        }
      Gifcurry.TextOverlay
        { Gifcurry.textOverlayText
        , Gifcurry.textOverlayOrigin
        , Gifcurry.textOverlayXTranslation
        , Gifcurry.textOverlayYTranslation
        , Gifcurry.textOverlayStartTime
        , Gifcurry.textOverlayEndTime
        , Gifcurry.textOverlayRotation
        , Gifcurry.textOverlayOutlineSize
        , Gifcurry.textOverlayOutlineColor
        , Gifcurry.textOverlayFillColor
        , Gifcurry.textOverlayFontFamily
        , Gifcurry.textOverlayFontStyle
        , Gifcurry.textOverlayFontStretch
        , Gifcurry.textOverlayFontWeight
        , Gifcurry.textOverlayFontSize
        }
      inFileDuration
      previewWidth
      previewHeight
      scaleFontSize
      = do
      when (textOverlayStartTime <= inFileDuration)
        $ GI.Gtk.spinButtonSetValue textOverlayStartTimeSpinButton textOverlayStartTime
      when (textOverlayEndTime   <= inFileDuration)
        $ GI.Gtk.spinButtonSetValue textOverlayEndTimeSpinButton   textOverlayEndTime

      GI.Gtk.entrySetText textOverlayTextEntry $ Data.Text.pack textOverlayText
      updateVisibilityToggleButtonLabel
        textOverlayVisibilityToggleButton
        textOverlayTextEntry

      let rotation = fromIntegral textOverlayRotation
      GI.Gtk.spinButtonSetValue textOverlayRotationSpinButton rotation

      let outlineSize = fromIntegral textOverlayOutlineSize
      GI.Gtk.spinButtonSetValue textOverlayOutlineSizeSpinButton outlineSize

      outlineRgba <- GI.Gdk.newZeroRGBA
      fillRgba    <- GI.Gdk.newZeroRGBA
      _           <- GI.Gdk.rGBAParse outlineRgba $ Data.Text.pack textOverlayOutlineColor
      _           <- GI.Gdk.rGBAParse fillRgba    $ Data.Text.pack textOverlayFillColor
      _           <- GI.Gtk.colorChooserSetRgba textOverlayOutlineColorButton outlineRgba
      _           <- GI.Gtk.colorChooserSetRgba textOverlayFillColorButton    fillRgba

      fontDescription <- GI.Pango.fontDescriptionNew
      GI.Pango.fontDescriptionSetFamily  fontDescription $ Data.Text.pack textOverlayFontFamily

      gifWidth                 <- getGifWidth guiComponents
      let ratio                = if gifWidth <= 0 then 1 else previewWidth / gifWidth
      let textOverlayFontSize' =
            fromIntegral textOverlayFontSize * if scaleFontSize then ratio else 1
      GI.Pango.fontDescriptionSetSize    fontDescription $ GI.Pango.SCALE * doubleToInt32 textOverlayFontSize'
      GI.Pango.fontDescriptionSetStretch fontDescription
        $ case textOverlayFontStretch of
          "UltraCondensed" -> GI.Pango.StretchUltraCondensed
          "ExtraCondensed" -> GI.Pango.StretchExtraCondensed
          "Condensed"      -> GI.Pango.StretchCondensed
          "SemiCondensed"  -> GI.Pango.StretchSemiCondensed
          "Normal"         -> GI.Pango.StretchNormal
          "SemiExpanded"   -> GI.Pango.StretchSemiExpanded
          "Expanded"       -> GI.Pango.StretchExpanded
          "ExtraExpanded"  -> GI.Pango.StretchExtraExpanded
          "UltraExpanded"  -> GI.Pango.StretchUltraExpanded
          _                -> GI.Pango.StretchNormal
      GI.Pango.fontDescriptionSetStyle fontDescription
        $ case textOverlayFontStyle of
          "Normal"         -> GI.Pango.StyleNormal
          "Oblique"        -> GI.Pango.StyleOblique
          "Italic"         -> GI.Pango.StyleItalic
          _                -> GI.Pango.StyleNormal
      GI.Pango.fontDescriptionSetWeight fontDescription
        $ case textOverlayFontWeight of
            100            -> GI.Pango.WeightThin
            200            -> GI.Pango.WeightUltralight
            300            -> GI.Pango.WeightLight
            350            -> GI.Pango.WeightSemilight
            380            -> GI.Pango.WeightBook
            400            -> GI.Pango.WeightNormal
            500            -> GI.Pango.WeightMedium
            600            -> GI.Pango.WeightSemibold
            700            -> GI.Pango.WeightBold
            800            -> GI.Pango.WeightUltrabold
            900            -> GI.Pango.WeightHeavy
            1000           -> GI.Pango.WeightUltraheavy
            _              -> GI.Pango.AnotherWeight textOverlayFontWeight
      GI.Gtk.fontChooserSetFontDesc textOverlayFontButton fontDescription

      -- TODO Fix.
      --
      -- This:
      --pangoContext <- GI.Pango.contextNew
      --pangoLayout  <- GI.Pango.layoutNew pangoContext
      --GI.Pango.layoutSetAlignment pangoLayout GI.Pango.AlignmentCenter
      --GI.Pango.layoutSetFontDescription pangoLayout (Just fontDescription)
      --GI.Pango.layoutSetText
      --  pangoLayout
      --  (Data.Text.pack textOverlayText)
      --  (intToInt32 $ length textOverlayText)
      --(textWidth, textHeight) <- GI.Pango.layoutGetSize pangoLayout
      --let xAdjust     = (int32ToDouble textWidth  / 2.0) / previewWidth
      --let yAdjust     = (int32ToDouble textHeight / 2.0) / previewHeight
      --
      -- Crashes with:
      -- GLib-GObject-CRITICAL **: g_object_replace_qdata: assertion 'G_IS_OBJECT (object)' failed
      -- GLib-GObject-CRITICAL **: g_object_get_qdata: assertion 'G_IS_OBJECT (object)' failed
      --
      -- Using the older library as a workaround:
      pangoContext <- GRPC.cairoCreateContext Nothing
      pangoLayout  <- GRPL.layoutEmpty pangoContext
      GRPL.layoutSetText pangoLayout textOverlayText
      GRPL.layoutSetAlignment pangoLayout GRPL.AlignCenter
      fontDescriptionString <- Data.Text.unpack <$> GI.Pango.fontDescriptionToString fontDescription
      fontDescription'      <- GRPF.fontDescriptionFromString fontDescriptionString
      GRPL.layoutSetFontDescription pangoLayout (Just fontDescription')
      (_, GRPL.PangoRectangle _x _y textWidth textHeight) <- GRPL.layoutGetExtents pangoLayout

      let xAdjust     = (textWidth  / 2.0) / previewWidth
      let yAdjust     = (textHeight / 2.0) / previewHeight
      let (horizontal, vertical) =
            case textOverlayOrigin of
              Gifcurry.TextOverlayOriginNorthWest -> (-0.5 + xAdjust, -0.5 + yAdjust)
              Gifcurry.TextOverlayOriginNorth     -> (   0          , -0.5 + yAdjust)
              Gifcurry.TextOverlayOriginNorthEast -> ( 0.5 - xAdjust, -0.5 + yAdjust)
              Gifcurry.TextOverlayOriginEast      -> ( 0.5 - xAdjust,  0  )
              Gifcurry.TextOverlayOriginSouthEast -> ( 0.5 - xAdjust,  0.5 - yAdjust)
              Gifcurry.TextOverlayOriginSouth     -> ( 0            ,  0.5 - yAdjust)
              Gifcurry.TextOverlayOriginSouthWest -> (-0.5 + xAdjust,  0.5 - yAdjust)
              Gifcurry.TextOverlayOriginWest      -> (-0.5 + xAdjust,  0  )
              Gifcurry.TextOverlayOriginCenter    -> ( 0            ,  0  )

      GI.Gtk.spinButtonSetValue textOverlayHorizontalSpinButton (horizontal + textOverlayXTranslation)
      GI.Gtk.spinButtonSetValue textOverlayVerticalSpinButton   (vertical   + textOverlayYTranslation)

      return ()
    filterTextOverlay
      ::  Int
      ->  [GR.GuiTextOverlayComponents]
      ->  [GR.GuiTextOverlayComponents]
    filterTextOverlay textOverlayId =
      foldl
      (\ x t -> if textOverlayId /= GR.textOverlayId t then x ++ [t] else x)
      []

removeTextOverlays
  ::  GR.GuiComponents
  ->  IO ()
removeTextOverlays
  GR.GuiComponents
    { GR.textOverlaysBox
    , GR.textOverlaysRef
    }
  = do
  textOverlays <- readIORef textOverlaysRef
  mapM_
    (\ GR.GuiTextOverlayComponents { GR.textOverlayBox } ->
      GI.Gtk.containerRemove textOverlaysBox textOverlayBox
    )
    textOverlays
  atomicModifyIORef' textOverlaysRef $ const ([], ())

hideAllOtherTextOverlays :: GR.GuiComponents -> Int -> IO ()
hideAllOtherTextOverlays
  GR.GuiComponents
    { GR.textOverlaysRef
    }
  showTextOverlayId
  = do
  textOverlays <- readIORef textOverlaysRef
  mapM_
    (\ GR.GuiTextOverlayComponents
        { GR.textOverlayId
        , GR.textOverlayVisibilityBox
        , GR.textOverlayVisibilityToggleButton
        }
      ->
      if textOverlayId == showTextOverlayId
        then do
          GI.Gtk.widgetShow            textOverlayVisibilityBox
          GI.Gtk.setToggleButtonActive textOverlayVisibilityToggleButton True
          GI.Gtk.widgetSetTooltipText  textOverlayVisibilityToggleButton
            $ Just "Close this text?"
        else do
          GI.Gtk.widgetHide textOverlayVisibilityBox
          GI.Gtk.setToggleButtonActive textOverlayVisibilityToggleButton False
          GI.Gtk.widgetSetTooltipText  textOverlayVisibilityToggleButton
            $ Just "Open this text?"
    )
    textOverlays

getPreviewDurationWidthAndHeight
  ::  GR.GuiComponents
  ->  IO (Double, Double, Double)
getPreviewDurationWidthAndHeight
  GR.GuiComponents
    { GR.maybeVideoPreviewWidget
    , GR.videoPreviewDrawingArea
    , GR.firstFramePreviewImageDrawingArea
    , GR.guiInFilePropertiesRef
    }
  = do
  GR.GuiInFileProperties
    { GR.inFileDuration
    , GR.inFileWidth
    , GR.inFileHeight
    }                   <- readIORef guiInFilePropertiesRef
  let usingVideoPreview = isJust maybeVideoPreviewWidget
  let fileHasSize       = if inFileWidth > 0.0 && inFileHeight > 0.0 then 1.0 else 0.0
  let drawingArea       = if usingVideoPreview
                            then videoPreviewDrawingArea
                            else firstFramePreviewImageDrawingArea
  width                 <- (*) fileHasSize . int32ToDouble <$> GI.Gtk.widgetGetAllocatedWidth  drawingArea
  height                <- (*) fileHasSize . int32ToDouble <$> GI.Gtk.widgetGetAllocatedHeight drawingArea
  return (inFileDuration, width, height)

getColorButtonString
  ::  GI.Gtk.ColorButton
  ->  String
  ->  IO String
getColorButtonString colorButton defaultString = do
  defaultRgba <- GI.Gdk.newZeroRGBA
  _           <- GI.Gdk.rGBAParse defaultRgba (Data.Text.pack defaultString)
  maybeRgba   <- GI.Gtk.getColorButtonRgba colorButton
  text <-
    case maybeRgba of
      Just rgba -> GI.Gdk.rGBAToString rgba
      Nothing   -> GI.Gdk.rGBAToString defaultRgba
  return $ Data.Text.unpack text

getGifWidth
  :: GR.GuiComponents
  -> IO Double
getGifWidth
  GR.GuiComponents
    { GR.leftCropSpinButton
    , GR.rightCropSpinButton
    , GR.widthSpinButton
    }
  = do
  leftCrop  <- GI.Gtk.spinButtonGetValue leftCropSpinButton
  rightCrop <- GI.Gtk.spinButtonGetValue rightCropSpinButton
  width     <- GI.Gtk.spinButtonGetValue widthSpinButton
  return (width / (1 - leftCrop - rightCrop))

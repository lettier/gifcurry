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
import qualified Data.Text
import Data.List (sort)
import qualified GI.Gdk
import qualified GI.GdkPixbuf
import qualified GI.Gtk
import qualified GI.Pango
import qualified Graphics.Rendering.Pango.Enums as GRPF
import qualified Graphics.Rendering.Pango.Font as GRPF

import Paths_Gifcurry
import qualified Gifcurry
import qualified GuiRecords as GR
import GuiStyle
import GuiMisc

handleTextOverlaysAddButton :: GR.GuiComponents -> IO ()
handleTextOverlaysAddButton
  guiComponents@GR.GuiComponents
    { GR.textOverlaysAddButton
    }
  =
  void $
    GI.Gtk.onButtonClicked
      textOverlaysAddButton
      (addTextOverlay guiComponents)

getGifcurryTextOverlays :: GR.GuiComponents -> IO [Gifcurry.TextOverlay]
getGifcurryTextOverlays
  guiComponents@GR.GuiComponents
    { GR.widthSpinButton
    , GR.leftCropSpinButton
    , GR.rightCropSpinButton
    }
  = do
  guiTextOverlaysData  <- getTextOverlaysData guiComponents
  (_, previewWidth, _) <- getPreviewDurationWidthAndHeight guiComponents
  leftCrop             <- GI.Gtk.spinButtonGetValue leftCropSpinButton
  rightCrop            <- GI.Gtk.spinButtonGetValue rightCropSpinButton
  width                <- GI.Gtk.spinButtonGetValue widthSpinButton
  mapM
    ( getGifcurryTextOverlay
        previewWidth
        (width / (1 - leftCrop - rightCrop))
    )
    guiTextOverlaysData
  where
    getGifcurryTextOverlay
      ::  Double
      ->  Double
      ->  GR.GuiTextOverlayData
      ->  IO Gifcurry.TextOverlay
    getGifcurryTextOverlay
      previewWidth
      gifWidth
      GR.GuiTextOverlayData
        { GR.textOverlayText
        , GR.textOverlayLeft
        , GR.textOverlayTop
        , GR.textOverlayStartTime
        , GR.textOverlayDurationTime
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
      let fontSize'    = doubleToInt $ fontSize * (gifWidth / previewWidth)
      let xTranslate   = textOverlayLeft
      let yTranslate   = textOverlayTop
      let startTime    = textOverlayStartTime
      let durationTime = textOverlayDurationTime
      let rotation     = int32ToInt    textOverlayRotation
      let outlineSize  = int32ToInt    textOverlayOutlineSize
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
          , Gifcurry.textOverlayDurationTime = durationTime
          , Gifcurry.textOverlayOutlineSize  = outlineSize
          , Gifcurry.textOverlayOutlineColor = textOverlayOutlineColor
          , Gifcurry.textOverlayFillColor    = textOverlayFillColor
          }
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

getTextOverlaysData :: GR.GuiComponents -> IO [GR.GuiTextOverlayData]
getTextOverlaysData
  GR.GuiComponents
    { GR.textOverlaysRef
    }
  = do
  textOverlays <- readIORef textOverlaysRef
  mapM
    (\
      GR.GuiTextOverlayComponents
        { GR.textOverlayTextEntry
        , GR.textOverlayLeftSpinButton
        , GR.textOverlayTopSpinButton
        , GR.textOverlayStartTimeSpinButton
        , GR.textOverlayDurationTimeSpinButton
        , GR.textOverlayRotationSpinButton
        , GR.textOverlayOutlineSizeSpinButton
        , GR.textOverlayOutlineColorButton
        , GR.textOverlayFillColorButton
        , GR.textOverlayFontButton
        }
      -> do
      text           <- Data.Text.unpack <$> GI.Gtk.entryGetText textOverlayTextEntry
      left           <- GI.Gtk.spinButtonGetValue textOverlayLeftSpinButton
      top            <- GI.Gtk.spinButtonGetValue textOverlayTopSpinButton
      start          <- GI.Gtk.spinButtonGetValue textOverlayStartTimeSpinButton
      duration       <- GI.Gtk.spinButtonGetValue textOverlayDurationTimeSpinButton
      rotation       <- GI.Gtk.spinButtonGetValueAsInt textOverlayRotationSpinButton
      outlineSize    <- GI.Gtk.spinButtonGetValueAsInt textOverlayOutlineSizeSpinButton
      outlineColor   <- getColorButtonString textOverlayOutlineColorButton "rgba(0,0,0,1)"
      fillColor      <- getColorButtonString textOverlayFillColorButton    "rgba(255,255,255,1)"
      maybeFontDesc  <- GI.Gtk.fontChooserGetFontDesc textOverlayFontButton
      maybeFontDesc' <-
        case maybeFontDesc of
          Nothing -> return Nothing
          Just fd -> do
            fds <- Data.Text.unpack <$> GI.Pango.fontDescriptionToString fd
            fd' <- GRPF.fontDescriptionFromString fds
            return $ Just fd'
      return
        GR.GuiTextOverlayData
          { GR.textOverlayText          = text
          , GR.textOverlayLeft          = left
          , GR.textOverlayTop           = top
          , GR.textOverlayStartTime     = start
          , GR.textOverlayDurationTime  = duration
          , GR.textOverlayRotation      = rotation
          , GR.textOverlayEndTime       = start + duration
          , GR.textOverlayOutlineSize   = outlineSize
          , GR.textOverlayOutlineColor  = outlineColor
          , GR.textOverlayFillColor     = fillColor
          , GR.textOverlayMaybeFontDesc = maybeFontDesc'
          }
    )
    textOverlays

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
        textOverlayRotationSpinButton $
          rotation / 360.0
      outlineSize <- GI.Gtk.spinButtonGetValue textOverlayOutlineSizeSpinButton
      GI.Gtk.entrySetProgressFraction
        textOverlayOutlineSizeSpinButton $
          outlineSize / 10.0
      when reset $ do
        GI.Gtk.spinButtonSetValue textOverlayRotationSpinButton    0.0
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
        { GR.textOverlayLeftSpinButton
        , GR.textOverlayTopSpinButton
        }
      = do
      GI.Gtk.spinButtonSetRange
        textOverlayLeftSpinButton
        (-0.5)
        0.5
      GI.Gtk.spinButtonSetRange
        textOverlayTopSpinButton
        (-0.5)
        0.5
      when reset $ do
        GI.Gtk.spinButtonSetValue textOverlayLeftSpinButton 0.0
        GI.Gtk.spinButtonSetValue textOverlayTopSpinButton  0.0
      left <- GI.Gtk.spinButtonGetValue textOverlayLeftSpinButton
      GI.Gtk.entrySetProgressFraction
        textOverlayLeftSpinButton $
          left + 0.5
      top  <- GI.Gtk.spinButtonGetValue textOverlayTopSpinButton
      GI.Gtk.entrySetProgressFraction
        textOverlayTopSpinButton $
          top + 0.5
    updateTimeSpinButtons :: Double -> GR.GuiTextOverlayComponents -> IO ()
    updateTimeSpinButtons
      duration
      GR.GuiTextOverlayComponents
        { GR.textOverlayStartTimeSpinButton
        , GR.textOverlayDurationTimeSpinButton
        }
      = do
      GI.Gtk.spinButtonSetRange
        textOverlayStartTimeSpinButton
        0.0
        duration
      if reset
        then do
          GI.Gtk.spinButtonSetRange
            textOverlayDurationTimeSpinButton
            0.0
            duration
          GI.Gtk.spinButtonSetValue
            textOverlayStartTimeSpinButton
            0.0
          GI.Gtk.spinButtonSetValue
            textOverlayDurationTimeSpinButton $
              truncatePastDigit duration 2
          GI.Gtk.entrySetProgressFraction
            textOverlayStartTimeSpinButton
            0.0
          GI.Gtk.entrySetProgressFraction
            textOverlayDurationTimeSpinButton
            1.0
        else do
          startTime           <- GI.Gtk.spinButtonGetValue textOverlayStartTimeSpinButton
          durationTime        <- GI.Gtk.spinButtonGetValue textOverlayDurationTimeSpinButton
          let maxDurationTime = clamp 0.0 duration (duration - startTime)
          GI.Gtk.spinButtonSetRange
            textOverlayDurationTimeSpinButton
            0.0
            maxDurationTime
          GI.Gtk.entrySetProgressFraction
            textOverlayStartTimeSpinButton $
              fromMaybe 0.0 $ safeDivide startTime duration
          GI.Gtk.entrySetProgressFraction
            textOverlayDurationTimeSpinButton $
              fromMaybe 0.0 $ safeDivide durationTime maxDurationTime

addTextOverlay :: GR.GuiComponents -> IO ()
addTextOverlay
  guiComponents@GR.GuiComponents
    { GR.textOverlaysBox
    , GR.confirmMessageDialog
    , GR.textOverlaysRef
    }
  = do
  (duration, width, height) <- getPreviewDurationWidthAndHeight guiComponents
  when (duration > 0.0 && width > 0.0 && height > 0.0) $ do
    isGtkVersionGte318                <- isGtkVersionGte 3 18
    penIconFilePathName               <- getDataFileName "data/pen-icon.svg"
    rightIconFilePathName             <- getDataFileName "data/right-icon.svg"
    downIconFilePathName              <- getDataFileName "data/down-icon.svg"
    startIconFilePathName             <- getDataFileName "data/start-icon.svg"
    endIconFilePathName               <- getDataFileName "data/end-icon.svg"
    spiralIconFilePathName            <- getDataFileName "data/spiral-icon.svg"
    widthIconFilePathName             <- getDataFileName "data/width-icon.svg"
    minusIconFilePathName             <- getDataFileName "data/minus-icon.svg"
    rightIconPixbuf                   <- GI.GdkPixbuf.pixbufNewFromFile rightIconFilePathName
    downIconPixbuf                    <- GI.GdkPixbuf.pixbufNewFromFile downIconFilePathName
    startIconPixbuf                   <- GI.GdkPixbuf.pixbufNewFromFile startIconFilePathName
    endIconPixbuf                     <- GI.GdkPixbuf.pixbufNewFromFile endIconFilePathName
    spiralIconPixbuf                  <- GI.GdkPixbuf.pixbufNewFromFile spiralIconFilePathName
    widthIconPixbuf                   <- GI.GdkPixbuf.pixbufNewFromFile widthIconFilePathName
    penIconImage                      <- GI.Gtk.imageNewFromFile penIconFilePathName
    minusIconImage                    <- GI.Gtk.imageNewFromFile minusIconFilePathName
    box                               <- GI.Gtk.boxNew GI.Gtk.OrientationVertical   0
    visibilityBox                     <- GI.Gtk.boxNew GI.Gtk.OrientationVertical   0
    positionSpinButtonsBox            <- GI.Gtk.boxNew GI.Gtk.OrientationHorizontal 0
    timeSpinButtonsBox                <- GI.Gtk.boxNew GI.Gtk.OrientationHorizontal 0
    rotationOutlineSizeSpinButtonsBox <- GI.Gtk.boxNew GI.Gtk.OrientationHorizontal 0
    colorButtonsBox                   <- GI.Gtk.boxNew GI.Gtk.OrientationHorizontal 0
    leftAdjustment                    <- GI.Gtk.adjustmentNew 0.0 0.0   0.0 0.01 0.0 0.0
    topAdjustment                     <- GI.Gtk.adjustmentNew 0.0 0.0   0.0 0.01 0.0 0.0
    startTimeAdjustment               <- GI.Gtk.adjustmentNew 0.0 0.0   0.0 1.0  0.0 0.0
    durationTimeAdjustment            <- GI.Gtk.adjustmentNew 0.0 0.0   0.0 1.0  0.0 0.0
    rotationAdjustment                <- GI.Gtk.adjustmentNew 0.0 0.0 360.0 1.0  0.0 0.0
    outlineSizeAdjustment             <- GI.Gtk.adjustmentNew 0.0 0.0  10.0 1.0  0.0 0.0
    leftSpinButton                    <- GI.Gtk.spinButtonNew (Just leftAdjustment)         1.0 2
    topSpinButton                     <- GI.Gtk.spinButtonNew (Just topAdjustment)          1.0 2
    startTimeSpinButton               <- GI.Gtk.spinButtonNew (Just startTimeAdjustment)    1.0 2
    durationTimeSpinButton            <- GI.Gtk.spinButtonNew (Just durationTimeAdjustment) 1.0 2
    rotationSpinButton                <- GI.Gtk.spinButtonNew (Just rotationAdjustment)     1.0 0
    outlineSizeSpinButton             <- GI.Gtk.spinButtonNew (Just outlineSizeAdjustment)  1.0 0
    textEntry                         <- GI.Gtk.entryNew
    visibilityToggleButton            <- GI.Gtk.toggleButtonNew
    fontButton                        <- GI.Gtk.fontButtonNew
    blackRgba                         <- GI.Gdk.newZeroRGBA
    whiteRgba                         <- GI.Gdk.newZeroRGBA
    _                                 <- GI.Gdk.rGBAParse blackRgba "rgba(0,0,0,1)"
    _                                 <- GI.Gdk.rGBAParse whiteRgba "rgba(255,255,255,1)"
    outlineColorButton                <- GI.Gtk.colorButtonNewWithRgba blackRgba
    fillColorButton                   <- GI.Gtk.colorButtonNewWithRgba whiteRgba
    removeButton                      <- GI.Gtk.buttonNewFromIconName (Just "gtk-remove") (enumToInt32 GI.Gtk.IconSizeButton)
    fontDescription                   <- GI.Pango.fontDescriptionFromString "Sans Regular 30"
    GI.Gtk.setToggleButtonDrawIndicator visibilityToggleButton            False
    GI.Gtk.setToggleButtonActive        visibilityToggleButton            True
    GI.Gtk.buttonSetImage               visibilityToggleButton            (Just penIconImage)
    GI.Gtk.buttonSetImage               removeButton                      (Just minusIconImage)
    GI.Gtk.setEntryPrimaryIconPixbuf    leftSpinButton                    rightIconPixbuf
    GI.Gtk.setEntryPrimaryIconPixbuf    topSpinButton                     downIconPixbuf
    GI.Gtk.setEntryPrimaryIconPixbuf    startTimeSpinButton               startIconPixbuf
    GI.Gtk.setEntryPrimaryIconPixbuf    durationTimeSpinButton            endIconPixbuf
    GI.Gtk.setEntryPrimaryIconPixbuf    rotationSpinButton                spiralIconPixbuf
    GI.Gtk.setEntryPrimaryIconPixbuf    outlineSizeSpinButton             widthIconPixbuf
    GI.Gtk.setWidgetDoubleBuffered      visibilityToggleButton            True
    GI.Gtk.setButtonAlwaysShowImage     visibilityToggleButton            True
    GI.Gtk.fontChooserSetFontDesc       fontButton                        fontDescription
    GI.Gtk.buttonSetLabel               removeButton                      "Remove"
    GI.Gtk.setButtonAlwaysShowImage     removeButton                      True
    GI.Gtk.boxSetHomogeneous            positionSpinButtonsBox            True
    GI.Gtk.boxSetHomogeneous            timeSpinButtonsBox                True
    GI.Gtk.boxSetHomogeneous            rotationOutlineSizeSpinButtonsBox True
    GI.Gtk.boxSetHomogeneous            colorButtonsBox                   True
    if isGtkVersionGte318
      then do
        GI.Gtk.widgetSetMarginEnd       penIconImage                      5
        GI.Gtk.widgetSetMarginEnd       minusIconImage                    5
      else do
        -- To support GTK 3.10, Ubuntu 14.04.
        GI.Gtk.widgetSetMarginRight     penIconImage                      5
        GI.Gtk.widgetSetMarginRight     minusIconImage                    5
    GI.Gtk.widgetSetTooltipText         leftSpinButton                    $ Just "How much space from the left?"
    GI.Gtk.widgetSetTooltipText         topSpinButton                     $ Just "How much space from the top?"
    GI.Gtk.widgetSetTooltipText         startTimeSpinButton               $ Just "What is the start time?"
    GI.Gtk.widgetSetTooltipText         durationTimeSpinButton            $ Just "How long is the duration?"
    GI.Gtk.widgetSetTooltipText         rotationSpinButton                $ Just "Rotate by how much?"
    GI.Gtk.widgetSetTooltipText         outlineSizeSpinButton             $ Just "How thick is the outline size?"
    GI.Gtk.widgetSetTooltipText         outlineColorButton                $ Just "What is the outline color?"
    GI.Gtk.widgetSetTooltipText         fillColorButton                   $ Just "What is the fill color?"
    GI.Gtk.widgetSetTooltipText         removeButton                      $ Just "Remove this text?"
    GI.Gtk.entrySetPlaceholderText      textEntry                         $ Just "What is the text?"
    GI.Gtk.spinButtonSetNumeric leftSpinButton                            True
    GI.Gtk.spinButtonSetNumeric topSpinButton                             True
    GI.Gtk.spinButtonSetNumeric startTimeSpinButton                       True
    GI.Gtk.spinButtonSetNumeric durationTimeSpinButton                    True
    GI.Gtk.spinButtonSetNumeric rotationSpinButton                        True
    GI.Gtk.spinButtonSetNumeric outlineSizeSpinButton                     True
    GI.Gtk.spinButtonSetUpdatePolicy leftSpinButton                       GI.Gtk.SpinButtonUpdatePolicyIfValid
    GI.Gtk.spinButtonSetUpdatePolicy topSpinButton                        GI.Gtk.SpinButtonUpdatePolicyIfValid
    GI.Gtk.spinButtonSetUpdatePolicy startTimeSpinButton                  GI.Gtk.SpinButtonUpdatePolicyIfValid
    GI.Gtk.spinButtonSetUpdatePolicy durationTimeSpinButton               GI.Gtk.SpinButtonUpdatePolicyIfValid
    GI.Gtk.spinButtonSetUpdatePolicy rotationSpinButton                   GI.Gtk.SpinButtonUpdatePolicyIfValid
    GI.Gtk.spinButtonSetUpdatePolicy outlineSizeSpinButton                GI.Gtk.SpinButtonUpdatePolicyIfValid
    GI.Gtk.spinButtonSetRange leftSpinButton                              (-0.5) 0.5
    GI.Gtk.spinButtonSetRange topSpinButton                               (-0.5) 0.5
    GI.Gtk.spinButtonSetRange startTimeSpinButton                         0.0    duration
    GI.Gtk.spinButtonSetRange durationTimeSpinButton                      0.0    duration
    GI.Gtk.spinButtonSetRange rotationSpinButton                          0.0    360.0
    GI.Gtk.spinButtonSetRange outlineSizeSpinButton                       0.0    10.0
    GI.Gtk.spinButtonSetValue leftSpinButton                              0.0
    GI.Gtk.spinButtonSetValue topSpinButton                               0.0
    GI.Gtk.spinButtonSetValue startTimeSpinButton                         0.0
    GI.Gtk.spinButtonSetValue durationTimeSpinButton                      duration
    GI.Gtk.spinButtonSetValue rotationSpinButton                          0.0
    GI.Gtk.spinButtonSetValue outlineSizeSpinButton                       10.0
    GI.Gtk.entrySetProgressFraction leftSpinButton                        0.5
    GI.Gtk.entrySetProgressFraction topSpinButton                         0.5
    GI.Gtk.entrySetProgressFraction startTimeSpinButton                   0.0
    GI.Gtk.entrySetProgressFraction durationTimeSpinButton                1.0
    GI.Gtk.entrySetProgressFraction rotationSpinButton                    0.0
    GI.Gtk.entrySetProgressFraction outlineSizeSpinButton                 1.0
    GI.Gtk.widgetSetMarginTop    visibilityToggleButton                   0
    GI.Gtk.widgetSetMarginBottom removeButton                             0
    GI.Gtk.widgetShow box
    GI.Gtk.widgetShow visibilityBox
    GI.Gtk.widgetShow visibilityToggleButton
    GI.Gtk.widgetShow positionSpinButtonsBox
    GI.Gtk.widgetShow timeSpinButtonsBox
    GI.Gtk.widgetShow rotationOutlineSizeSpinButtonsBox
    GI.Gtk.widgetShow colorButtonsBox
    GI.Gtk.widgetShow leftSpinButton
    GI.Gtk.widgetShow topSpinButton
    GI.Gtk.widgetShow startTimeSpinButton
    GI.Gtk.widgetShow durationTimeSpinButton
    GI.Gtk.widgetShow rotationSpinButton
    GI.Gtk.widgetShow outlineSizeSpinButton
    GI.Gtk.widgetShow outlineColorButton
    GI.Gtk.widgetShow fillColorButton
    GI.Gtk.widgetShow textEntry
    GI.Gtk.widgetShow fontButton
    GI.Gtk.widgetShow removeButton
    GI.Gtk.containerAdd positionSpinButtonsBox            leftSpinButton
    GI.Gtk.containerAdd positionSpinButtonsBox            topSpinButton
    GI.Gtk.containerAdd timeSpinButtonsBox                startTimeSpinButton
    GI.Gtk.containerAdd timeSpinButtonsBox                durationTimeSpinButton
    GI.Gtk.containerAdd rotationOutlineSizeSpinButtonsBox rotationSpinButton
    GI.Gtk.containerAdd rotationOutlineSizeSpinButtonsBox outlineSizeSpinButton
    GI.Gtk.containerAdd colorButtonsBox                   outlineColorButton
    GI.Gtk.containerAdd colorButtonsBox                   fillColorButton
    GI.Gtk.containerAdd visibilityBox                     positionSpinButtonsBox
    GI.Gtk.containerAdd visibilityBox                     timeSpinButtonsBox
    GI.Gtk.containerAdd visibilityBox                     rotationOutlineSizeSpinButtonsBox
    GI.Gtk.containerAdd visibilityBox                     colorButtonsBox
    GI.Gtk.containerAdd visibilityBox                     textEntry
    GI.Gtk.containerAdd visibilityBox                     fontButton
    GI.Gtk.containerAdd visibilityBox                     removeButton
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
    let minIdText = Data.Text.pack $ show minId
    GI.Gtk.buttonSetLabel       visibilityToggleButton minIdText
    GI.Gtk.widgetSetTooltipText visibilityToggleButton $
      Just $
        Data.Text.concat ["Close text ", minIdText, "?"]
    let textOverlay =
          GR.GuiTextOverlayComponents
            { GR.textOverlayId                      = minId
            , GR.textOverlayBox                     = box
            , GR.textOverlayVisibilityBox           = visibilityBox
            , GR.textOverlayVisibilityToggleButton  = visibilityToggleButton
            , GR.textOverlayLeftSpinButton          = leftSpinButton
            , GR.textOverlayTopSpinButton           = topSpinButton
            , GR.textOverlayStartTimeSpinButton     = startTimeSpinButton
            , GR.textOverlayDurationTimeSpinButton  = durationTimeSpinButton
            , GR.textOverlayRotationSpinButton      = rotationSpinButton
            , GR.textOverlayOutlineSizeSpinButton   = outlineSizeSpinButton
            , GR.textOverlayOutlineColorButton      = outlineColorButton
            , GR.textOverlayFillColorButton         = fillColorButton
            , GR.textOverlayTextEntry               = textEntry
            , GR.textOverlayFontButton              = fontButton
            , GR.textOverlayRemoveButton            = removeButton
            }
    atomicModifyIORef' textOverlaysRef $
      \ textOverlays' ->
        ( textOverlays' ++ [textOverlay]
        , ()
        )
    _ <- GI.Gtk.onButtonClicked removeButton $ do
      GI.Gtk.setMessageDialogText
        confirmMessageDialog $
          Data.Text.concat ["Remove text ", minIdText, "?"]
      confirmMessageDialogResponse <- GI.Gtk.dialogRun confirmMessageDialog
      when (confirmMessageDialogResponse == enumToInt32 GI.Gtk.ResponseTypeYes) $ do
        textOverlays' <- filterTextOverlay minId <$> readIORef textOverlaysRef
        atomicModifyIORef' textOverlaysRef $
          const (textOverlays', ())
        GI.Gtk.containerRemove textOverlaysBox box
    _ <- GI.Gtk.onWidgetButtonReleaseEvent visibilityToggleButton $ \ _ -> do
      active <- GI.Gtk.getToggleButtonActive visibilityToggleButton
      if active
        then hideAllOtherTextOverlays guiComponents (-1)
        else hideAllOtherTextOverlays guiComponents minId
      return True
    _ <- GI.Gtk.afterWidgetKeyReleaseEvent textEntry $ \ _ -> do
      text               <- GI.Gtk.entryGetText textEntry
      let limit          = 27
      let label          = Data.Text.concat [minIdText, " ", text]
      let label'         =
            if Data.Text.length label >= limit
              then Data.Text.concat [Data.Text.take limit label, "..."]
              else label
      GI.Gtk.buttonSetLabel
        visibilityToggleButton
        label'
      return False
    updateTextOverlays False guiComponents
    hideAllOtherTextOverlays guiComponents minId
    return ()
  where
    filterTextOverlay :: Int -> [GR.GuiTextOverlayComponents] -> [GR.GuiTextOverlayComponents]
    filterTextOverlay textOverlayId =
      foldl
      (\ x t -> if textOverlayId /= GR.textOverlayId t then x ++ [t] else x)
      []

removeTextOverlays :: GR.GuiComponents -> IO ()
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
      -> do
      let textOverlayId' = Data.Text.pack $ show textOverlayId
      if textOverlayId == showTextOverlayId
        then do
          GI.Gtk.widgetShow            textOverlayVisibilityBox
          GI.Gtk.setToggleButtonActive textOverlayVisibilityToggleButton True
          GI.Gtk.widgetSetTooltipText  textOverlayVisibilityToggleButton $
            Just $
              Data.Text.concat ["Close text ", textOverlayId', "?"]
        else do
          GI.Gtk.widgetHide textOverlayVisibilityBox
          GI.Gtk.setToggleButtonActive textOverlayVisibilityToggleButton False
          GI.Gtk.widgetSetTooltipText  textOverlayVisibilityToggleButton $
            Just $
              Data.Text.concat ["Open text ", textOverlayId', "?"]
    )
    textOverlays
  return ()

getPreviewDurationWidthAndHeight :: GR.GuiComponents -> IO (Double, Double, Double)
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

getColorButtonString :: GI.Gtk.ColorButton -> String -> IO String
getColorButtonString colorButton defaultString = do
  defaultRgba <- GI.Gdk.newZeroRGBA
  _           <- GI.Gdk.rGBAParse defaultRgba (Data.Text.pack defaultString)
  maybeRgba   <- GI.Gtk.getColorButtonRgba colorButton
  text <-
    case maybeRgba of
      Just rgba -> GI.Gdk.rGBAToString rgba
      Nothing   -> GI.Gdk.rGBAToString defaultRgba
  return $ Data.Text.unpack text


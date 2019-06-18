{-
  Gifcurry
  (C) 2018 David Lettier
  lettier.com
-}

{-# LANGUAGE
    OverloadedStrings
  , NamedFieldPuns
  , DuplicateRecordFields
#-}

module GuiPreview where

import System.FilePath
import System.IO.Temp
import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent
import Text.Printf
import Data.Int
import Data.Maybe
import Data.Text
import Data.List
import Data.Bits
import Data.IORef
import Data.GI.Base.Properties
import qualified GI.GLib
import qualified GI.Gdk
import qualified GI.Gtk
import qualified GI.Gst
import qualified GI.Cairo
import qualified GiCairoCairoBridge
import qualified Graphics.Rendering.Cairo as GRC
import qualified Graphics.Rendering.Pango.Cairo as GRPC
import qualified Graphics.Rendering.Pango.Layout as GRPL

import Paths_Gifcurry
import qualified Gifcurry
  ( createGif
  , GifParams(..)
  , defaultGifParams
  )
import qualified GtkMainSyncAsync (gtkMainSync, gtkMainAsync)
import qualified GuiRecords as GR
import qualified GuiTextOverlays
import GuiMisc

data ForFramePreview =
    ForFramePreviewFirst
  | ForFramePreviewLast
  | ForFramePreviewNone

blankPreviewIcon :: String
blankPreviewIcon = "gtk-discard"

framePreviewDirectoryName :: String
framePreviewDirectoryName = "gifcurry-frame-previews"

desiredPreviewSize :: Double
desiredPreviewSize = 700.0

buildVideoPreviewWidgetAndPlaybinElement :: IO (Maybe GI.Gtk.Widget, Maybe GI.Gst.Element)
buildVideoPreviewWidgetAndPlaybinElement = do
  maybeGtkSink <- GI.Gst.elementFactoryMake "gtksink" (Just "MultimediaPlayerGtkSink")
  case maybeGtkSink of
    Nothing      -> return (Nothing, Nothing)
    Just gtkSink -> do
      maybeVideoPreviewWidget <- Data.GI.Base.Properties.getObjectPropertyObject gtkSink "widget" GI.Gtk.Widget
      maybePlaybinElement     <- GI.Gst.elementFactoryMake "playbin" (Just "MultimediaPlayerPlaybin")
      case (maybeVideoPreviewWidget, maybePlaybinElement) of
        (Just videoPreviewWidget, Just playbinElement) -> do
            -- Turns off the subtitles.
            let flags = flip setBit 10 $ flip setBit 9 $ flip setBit 4 $ flip setBit 1 $ bit 0
            _ <- Data.GI.Base.Properties.setObjectPropertyObject playbinElement "video-sink" maybeGtkSink
            _ <- Data.GI.Base.Properties.setObjectPropertyBool   playbinElement "force-aspect-ratio" True
            _ <- Data.GI.Base.Properties.setObjectPropertyDouble playbinElement "volume" 0.0
            _ <- Data.GI.Base.Properties.setObjectPropertyInt    playbinElement "flags" flags
            _ <- GI.Gtk.widgetShow videoPreviewWidget
            return ()
        _ -> return ()
      return (maybeVideoPreviewWidget, maybePlaybinElement)

runGuiPreview :: GR.GuiComponents -> IO ()
runGuiPreview guiComponents = do
  setupVideoPreviewPauseToggleButton guiComponents
  handlePreviewType                  guiComponents
  runPreviewOverlay                  guiComponents
  runTimeSlicesWidget                guiComponents
  where
    handlePreviewType :: GR.GuiComponents -> IO ()
    handlePreviewType
      GR.GuiComponents
        { GR.mainPreviewBox
        , GR.maybeVideoPreviewWidget = (Just videoPreviewWidget)
        , GR.maybePlaybinElement     = (Just playbinElement)
        , GR.videoPreviewBox
        , GR.videoPreviewOverlayChildBox
        }
      = do
      mainPreviewBoxChildCount <-
        Data.List.length <$> GI.Gtk.containerGetChildren mainPreviewBox
      when (mainPreviewBoxChildCount <= 0) $ do
        GI.Gtk.boxPackStart mainPreviewBox videoPreviewBox True True 0
        runPreviewLoopIfNotRunning
          guiComponents $
            preview
              guiComponents
              videoPreview
              resetVideoPreview
      videoPreviewOverlayChildBoxChildCount <-
        Data.List.length <$> GI.Gtk.containerGetChildren videoPreviewOverlayChildBox
      when (videoPreviewOverlayChildBoxChildCount <= 0) $
        GI.Gtk.boxPackStart videoPreviewOverlayChildBox videoPreviewWidget True True 0
      addPlaybinElementBusWatch
      where
        addPlaybinElementBusWatch :: IO ()
        addPlaybinElementBusWatch = do
          maybePlaybinElementBus <- GI.Gst.elementGetBus playbinElement
          case maybePlaybinElementBus of
            (Just bus) ->
              void $
                GI.Gst.busAddWatch
                  bus
                  GI.GLib.PRIORITY_DEFAULT $
                    \ _ message -> do
                      messageTypes <- GI.Gst.getMessageType message
                      let messageType =
                            case messageTypes of
                              []    -> GI.Gst.MessageTypeUnknown
                              (x:_) -> x
                      when (messageType == GI.Gst.MessageTypeEos) $ do
                        seekPlaybinElement
                          guiComponents
                          Nothing
                          Nothing
                        return ()
                      return True
            _ -> return ()
    handlePreviewType
      GR.GuiComponents
        { GR.mainPreviewBox
        , GR.imagesPreviewBox
        }
      = do
      childrenCount <- Data.List.length <$> GI.Gtk.containerGetChildren mainPreviewBox
      when (childrenCount <= 0) $ do
        _ <- GI.Gtk.boxPackStart mainPreviewBox imagesPreviewBox True True 0
        runPreviewLoopIfNotRunning
          guiComponents $
            preview
              guiComponents
              firstAndLastFramePreview
              resetFirstAndLastFramePreview
        return ()

runPreviewLoopIfNotRunning :: GR.GuiComponents -> IO Bool -> IO ()
runPreviewLoopIfNotRunning
  GR.GuiComponents
    { GR.guiPreviewStateRef }
  f
  = do
  loopRunning <- getLoopRunning
  unless loopRunning $
    void $
      GI.GLib.timeoutAdd
        GI.GLib.PRIORITY_DEFAULT
        1
        f
  where
    getLoopRunning :: IO Bool
    getLoopRunning =
      GR.loopRunning <$> readIORef guiPreviewStateRef

preview
  ::  GR.GuiComponents
  ->  (  GR.GuiPreviewFunctionArgs
      -> IO ()
      )
  ->  (  GR.GuiComponents
      -> IO ()
      )
  ->  IO Bool
preview
  guiComponents@GR.GuiComponents
    { GR.startTimeSpinButton
    , GR.endTimeSpinButton
    , GR.colorCountSpinButton
    , GR.ditherToggleButton
    , GR.guiPreviewStateRef
    , GR.guiInFilePropertiesRef
    }
  previewFunction
  resetPreviewFunction
  = do
  GR.GuiInFileProperties
    { GR.inFileUri = inFilePath
    , GR.inFileWidth
    , GR.inFileHeight
    } <- readIORef guiInFilePropertiesRef
  GR.GuiPreviewState
    { GR.maybeInFilePath
    , GR.maybeStartTime
    , GR.maybeEndTime
    , GR.maybeColorCount
    , GR.maybeDither
    }        <- readIORef guiPreviewStateRef
  startTime  <- GI.Gtk.spinButtonGetValue startTimeSpinButton
  endTime    <- GI.Gtk.spinButtonGetValue endTimeSpinButton
  colorCount <- GI.Gtk.spinButtonGetValue colorCountSpinButton
  dither     <- GI.Gtk.toggleButtonGetActive ditherToggleButton
  let invalidInFilePath   = inFilePath   == ""
  let invalidStartTime    = startTime    < 0.0
  let invalidEndTime      = endTime      <= 0.0
  let invalidInFileWidth  = inFileWidth  <= 0.0
  let invalidInFileHeight = inFileHeight <= 0.0
  let invalidColorCount   = colorCount < 1 || colorCount > 256
  let inputInvalid        =
           invalidInFilePath
        || invalidStartTime
        || invalidEndTime
        || invalidInFileWidth
        || invalidInFileHeight
        || invalidColorCount
  if not inputInvalid
    then
      previewFunction
        GR.GuiPreviewFunctionArgs
          { GR.guiComponents       = guiComponents
          , GR.inFilePath          = inFilePath
          , GR.startTime           = startTime
          , GR.endTime             = endTime
          , GR.colorCount          = colorCount
          , GR.dither              = dither
          , GR.inFileWidth         = inFileWidth
          , GR.inFileHeight        = inFileHeight
          , GR.inFilePathChanged   = fromMaybe ""     maybeInFilePath /= inFilePath
          , GR.startTimeChanged    = fromMaybe (-1.0) maybeStartTime  /= startTime
          , GR.endTimeChanged      = fromMaybe (-1.0) maybeEndTime    /= endTime
          , GR.colorCountChanged   = fromMaybe (-1.0) maybeColorCount /= colorCount
          , GR.ditherChanged       = fromMaybe False  maybeDither     /= dither
          }
    else do
      resetPreviewFunction guiComponents
      resetWindow guiComponents
  atomicModifyIORef' guiPreviewStateRef $
    \ guiPreviewState' ->
      ( guiPreviewState'
          { GR.maybeInFilePath = if invalidInFilePath then Nothing else Just inFilePath
          , GR.maybeStartTime  = if invalidStartTime  then Nothing else Just startTime
          , GR.maybeEndTime    = if invalidEndTime    then Nothing else Just endTime
          , GR.maybeColorCount = if invalidColorCount then Nothing else Just colorCount
          , GR.maybeDither     = Just dither
          , GR.loopRunning     = True
          }
      , ()
      )
  return True

videoPreview
  ::  GR.GuiPreviewFunctionArgs
  ->  IO ()
videoPreview
  GR.GuiPreviewFunctionArgs
    { GR.guiComponents =
        guiComponents@GR.GuiComponents
            { GR.window
            , GR.cropToggleButton
            , GR.textOverlaysToggleButton
            , GR.mainPreviewBox
            , GR.videoPreviewBox
            , GR.maybePlaybinElement = (Just playbinElement)
            }
    , GR.inFilePath
    , GR.startTime
    , GR.endTime
    , GR.inFileWidth
    , GR.inFileHeight
    , GR.inFilePathChanged
    , GR.startTimeChanged
    , GR.endTimeChanged
    }
  = do
  GI.Gtk.widgetShow videoPreviewBox
  sizePreviewAndWindow
  handleChanges
  updateVideoPreviewAspectRatio
  where
    sizePreviewAndWindow :: IO ()
    sizePreviewAndWindow = do
      let (previewWidth, previewHeight) =
            getPreviewWidthAndHeight inFileWidth inFileHeight
      GI.Gtk.widgetSetSizeRequest
        window
        (doubleToInt32 previewWidth)
        (-1)
      GI.Gtk.widgetSetSizeRequest
        mainPreviewBox
        (doubleToInt32 previewWidth)
        (doubleToInt32 previewHeight)
      resizeWindow window
      return ()
    handleChanges :: IO ()
    handleChanges = do
      (maybePlaybinDuration, maybePlaybinPosition)
                            <- getPlaybinDurationAndPosition guiComponents
      let playbinDuration   = fromMaybe (-1) maybePlaybinDuration
      let playbinPosition   = fromMaybe (-1) maybePlaybinPosition
      let startTimeInNano   = secondsToNanoseconds startTime
      let endTimeInNano     = secondsToNanoseconds endTime
      let outOfBounds       =         playbinDuration >  0
                                &&    playbinPosition >= 0
                                && (  playbinPosition < startTimeInNano
                                   || playbinPosition > endTimeInNano
                                   )
      let seekToStart       =     inFilePathChanged
                              ||  startTimeChanged
                              ||  endTimeChanged
                              ||  outOfBounds
      when inFilePathChanged $ do
        void $ GI.Gst.elementSetState playbinElement GI.Gst.StateNull
        Data.GI.Base.Properties.setObjectPropertyString
          playbinElement
          "uri"
          (Just $ pack $ "file://" ++ inFilePath)
        Data.GI.Base.Properties.setObjectPropertyDouble
          playbinElement
          "volume"
          0.0
        resizeWindow window
        return ()
      when seekToStart $ do
        seekPlaybinElement
          guiComponents
          Nothing
          Nothing
        return ()
    updateVideoPreviewAspectRatio :: IO ()
    updateVideoPreviewAspectRatio = do
      cropModeEnabled        <- GI.Gtk.getToggleButtonActive cropToggleButton
      textOverlayModeEnabled <- GI.Gtk.getToggleButtonActive textOverlaysToggleButton
      if cropModeEnabled || textOverlayModeEnabled
        then
          void $ Data.GI.Base.Properties.setObjectPropertyBool
            playbinElement
            "force-aspect-ratio"
            False
        else
          void $ Data.GI.Base.Properties.setObjectPropertyBool
            playbinElement
            "force-aspect-ratio"
            True
videoPreview _ = return ()

firstAndLastFramePreview
  ::  GR.GuiPreviewFunctionArgs
  ->  IO ()
firstAndLastFramePreview
  GR.GuiPreviewFunctionArgs
    { GR.guiComponents =
        GR.GuiComponents
          { GR.window
          , GR.imagesPreviewBox
          , GR.firstFrameImage
          , GR.lastFrameImage
          , GR.firstFramePreviewImageDrawingArea
          , GR.lastFramePreviewImageDrawingArea
          , GR.temporaryDirectory
          }
    , GR.inFilePath
    , GR.startTime
    , GR.endTime
    , GR.colorCount
    , GR.dither
    , GR.inFileWidth
    , GR.inFileHeight
    , GR.inFilePathChanged
    , GR.startTimeChanged
    , GR.endTimeChanged
    , GR.colorCountChanged
    , GR.ditherChanged
    }
  = do
  GI.Gtk.widgetShow imagesPreviewBox
  handleChanges
  redrawDrawingAreas
  where
    handleChanges :: IO ()
    handleChanges = do
      let firstAndLastFrameDirty          =     inFilePathChanged
                                            ||  startTimeChanged
                                            ||  colorCountChanged
                                            ||  ditherChanged
      let lastFrameDirty                  = not firstAndLastFrameDirty && endTimeChanged
      let (previewWidth, _)               = getPreviewWidthAndHeight inFileWidth inFileHeight
      let guiMakeFramePreviewFunctionArgs =
            GR.GuiMakeFramePreviewFunctionArgs
              { GR.inFilePath         = inFilePath
              , GR.startTime          = startTime
              , GR.endTime            = endTime
              , GR.colorCount         = colorCount
              , GR.dither             = dither
              , GR.previewWidth       = previewWidth / 2.0
              , GR.firstFrameImage    = firstFrameImage
              , GR.lastFrameImage     = lastFrameImage
              , GR.temporaryDirectory = temporaryDirectory
              , GR.window             = window
              }
      when firstAndLastFrameDirty $
        makeFirstAndLastFramePreview
          guiMakeFramePreviewFunctionArgs
      when lastFrameDirty $
        makeLastFramePreview
          guiMakeFramePreviewFunctionArgs
    redrawDrawingAreas :: IO ()
    redrawDrawingAreas = do
      GI.Gtk.widgetQueueDraw firstFramePreviewImageDrawingArea
      GI.Gtk.widgetQueueDraw lastFramePreviewImageDrawingArea

runPreviewOverlay :: GR.GuiComponents -> IO ()
runPreviewOverlay
  guiComponents@GR.GuiComponents
    { GR.maybeVideoPreviewWidget = (Just _)
    , GR.maybePlaybinElement     = (Just _)
    , GR.videoPreviewDrawingArea
    }
  = do
  _ <- GI.GLib.timeoutAdd
      GI.GLib.PRIORITY_DEFAULT
      1 $ do
        GI.Gtk.widgetQueueDraw videoPreviewDrawingArea
        return True
  onPreviewOverlayDraw
    guiComponents
    videoPreviewDrawingArea
    ForFramePreviewNone
    True
  return ()
runPreviewOverlay
  guiComponents@GR.GuiComponents
    { GR.firstFramePreviewImageDrawingArea
    , GR.lastFramePreviewImageDrawingArea
    }
    = do
    _ <- GI.GLib.timeoutAdd
        GI.GLib.PRIORITY_DEFAULT
        1 $ do
          GI.Gtk.widgetQueueDraw firstFramePreviewImageDrawingArea
          GI.Gtk.widgetQueueDraw lastFramePreviewImageDrawingArea
          return True
    onPreviewOverlayDraw
      guiComponents
      firstFramePreviewImageDrawingArea
      ForFramePreviewFirst
      False
    onPreviewOverlayDraw
      guiComponents
      lastFramePreviewImageDrawingArea
      ForFramePreviewLast
      False
    return ()

onPreviewOverlayDraw
  ::  GR.GuiComponents
  ->  GI.Gtk.DrawingArea
  ->  ForFramePreview
  ->  Bool
  ->  IO ()
onPreviewOverlayDraw
  guiComponents
  drawingArea
  forFramePreview
  bool
  =
  void $
    GI.Gtk.onWidgetDraw
      drawingArea $
        \ context -> do
          _ <- drawCropGrid     guiComponents drawingArea                 context
          _ <- drawTextOverlays guiComponents drawingArea forFramePreview context
          return bool

drawCropGrid
  ::  GR.GuiComponents
  ->  GI.Gtk.DrawingArea
  ->  GI.Cairo.Context
  ->  IO Bool
drawCropGrid
  GR.GuiComponents
    { GR.cropToggleButton
    , GR.leftCropSpinButton
    , GR.rightCropSpinButton
    , GR.topCropSpinButton
    , GR.bottomCropSpinButton
    , GR.guiInFilePropertiesRef
    }
  drawingArea
  context
  = do
  GR.GuiInFileProperties
    { GR.inFileWidth
    , GR.inFileHeight
    }               <- readIORef guiInFilePropertiesRef
  cropModeEnabled   <- GI.Gtk.getToggleButtonActive cropToggleButton
  drawingAreaWidth  <-
    int32ToDouble <$> GI.Gtk.widgetGetAllocatedWidth drawingArea
  drawingAreaHeight <-
    int32ToDouble <$> GI.Gtk.widgetGetAllocatedHeight drawingArea
  left   <- GI.Gtk.spinButtonGetValue leftCropSpinButton
  right  <- GI.Gtk.spinButtonGetValue rightCropSpinButton
  top    <- GI.Gtk.spinButtonGetValue topCropSpinButton
  bottom <- GI.Gtk.spinButtonGetValue bottomCropSpinButton
  when (cropModeEnabled && inFileWidth > 0.0 && inFileHeight > 0.0) $
    GiCairoCairoBridge.renderWithContext context $ do
      orangePatternPng <- liftIO $ getDataFileName "data/orange-pattern.png"
      orangeSurface    <- liftIO $ GRC.imageSurfaceCreateFromPNG orangePatternPng
      GRC.withPatternForSurface
        orangeSurface $ \ orangePattern -> do
          GRC.patternSetExtend orangePattern GRC.ExtendRepeat

          -- Left
          drawRectPattern
            orangePattern
            0.0
            0.0
            (left * drawingAreaWidth)
            drawingAreaHeight

          -- Right
          drawRectPattern
            orangePattern
            (drawingAreaWidth - right * drawingAreaWidth)
            0.0
            (right * drawingAreaWidth)
            drawingAreaHeight

          -- Top
          drawRectPattern
            orangePattern
            (left * drawingAreaWidth)
            0.0
            (drawingAreaWidth - right * drawingAreaWidth - left * drawingAreaWidth)
            (top * drawingAreaHeight)

          -- Bottom
          drawRectPattern
            orangePattern
            (left * drawingAreaWidth)
            (drawingAreaHeight - bottom * drawingAreaHeight)
            (drawingAreaWidth - right * drawingAreaWidth - left * drawingAreaWidth)
            (bottom * drawingAreaHeight)
  return False

drawTextOverlays
  ::  GR.GuiComponents
  ->  GI.Gtk.DrawingArea
  ->  ForFramePreview
  ->  GI.Cairo.Context
  ->  IO Bool
drawTextOverlays
  guiComponents@GR.GuiComponents
    { GR.startTimeSpinButton
    , GR.endTimeSpinButton
    , GR.textOverlaysToggleButton
    , GR.guiInFilePropertiesRef
    }
  drawingArea
  forFramePreview
  context
  = do
  GR.GuiInFileProperties
    { GR.inFileWidth
    , GR.inFileHeight
    , GR.inFileDuration
    } <- readIORef guiInFilePropertiesRef
  textOverlayModeEnabled               <- GI.Gtk.getToggleButtonActive textOverlaysToggleButton
  when (textOverlayModeEnabled && inFileWidth > 0.0 && inFileHeight > 0.0) $ do
    (previewDuration, previewPosition) <- getPreviewDurationAndPosition inFileDuration
    drawingAreaWidth                   <- int32ToDouble <$> GI.Gtk.widgetGetAllocatedWidth  drawingArea
    drawingAreaHeight                  <- int32ToDouble <$> GI.Gtk.widgetGetAllocatedHeight drawingArea
    GuiTextOverlays.updateTextOverlays False guiComponents
    textOverlaysData <- Prelude.reverse <$> GuiTextOverlays.getTextOverlaysData guiComponents
    GiCairoCairoBridge.renderWithContext context $
      mapM_
        ( renderTextOverlayData
            previewDuration
            previewPosition
            drawingAreaWidth
            drawingAreaHeight
        )
        textOverlaysData
  return False
  where
    renderTextOverlayData
      ::  Double
      ->  Double
      ->  Double
      ->  Double
      ->  GR.GuiTextOverlayData
      ->  GRC.Render ()
    renderTextOverlayData
      _previewDuration
      previewPosition
      drawingAreaWidth
      drawingAreaHeight
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
      GRC.save

      GRC.setLineWidth $ int32ToDouble textOverlayOutlineSize
      pangoLayout <- GRPC.createLayout textOverlayText
      liftIO $ GRPL.layoutSetAlignment pangoLayout GRPL.AlignCenter
      liftIO $ GRPL.layoutSetFontDescription pangoLayout textOverlayMaybeFontDesc
      (_, GRPL.PangoRectangle _x _y width height) <- liftIO $ GRPL.layoutGetExtents pangoLayout
      let alphaChannel =
            if      textOverlayStartTime <= previewPosition
                &&  textOverlayEndTime   >= previewPosition
              then 1.0
              else 0.0
      let x  = (textOverlayHorizontal + 0.5) * drawingAreaWidth
      let y  = (textOverlayVertical   + 0.5) * drawingAreaHeight
      let x' = x - (width  / 2.0)
      let y' = y - (height / 2.0)
      let r  = int32ToDouble textOverlayRotation * pi / 180.0

      GRC.translate x        y
      GRC.rotate    r
      GRC.translate (-1 * x) (-1 * y)
      GRC.translate x'       y'

      GRPC.layoutPath pangoLayout
      (oR, oG, oB) <- liftIO $ getRgb textOverlayOutlineColor
      GRC.setSourceRGBA oR oG oB alphaChannel
      GRC.stroke
      (fR, fG, fB) <- liftIO $ getRgb textOverlayFillColor
      GRC.setSourceRGBA fR fG fB alphaChannel
      GRPC.showLayout pangoLayout

      GRC.restore
    getRgb :: String -> IO (Double, Double, Double)
    getRgb string = do
      rgba <- GI.Gdk.newZeroRGBA
      _    <- GI.Gdk.rGBAParse    rgba (Data.Text.pack string)
      r    <- GI.Gdk.getRGBARed   rgba
      g    <- GI.Gdk.getRGBAGreen rgba
      b    <- GI.Gdk.getRGBABlue  rgba
      return (r, g, b)
    getPreviewDurationAndPosition :: Double -> IO (Double, Double)
    getPreviewDurationAndPosition inFileDuration = do
      (maybePlaybinDuration, maybePlaybinPosition)   <- getPlaybinDurationAndPosition guiComponents
      case (maybePlaybinDuration, maybePlaybinPosition) of
        (Just playbinDuration, Just playbinPosition) ->
          return
            ( nanosecondsToSeconds playbinDuration
            , nanosecondsToSeconds playbinPosition
            )
        _ ->
          case forFramePreview of
            ForFramePreviewFirst  -> do
              previewPosition     <- GI.Gtk.spinButtonGetValue startTimeSpinButton
              return (inFileDuration, previewPosition)
            ForFramePreviewLast   -> do
              endTime             <- GI.Gtk.spinButtonGetValue endTimeSpinButton
              let previewPosition = endTime
              return (inFileDuration, previewPosition)
            ForFramePreviewNone  -> return (0.0, -1.0)

runTimeSlicesWidget :: GR.GuiComponents -> IO ()
runTimeSlicesWidget
  guiComponents@GR.GuiComponents
    { GR.maybeVideoPreviewWidget
    , GR.maybePlaybinElement
    , GR.startTimeSpinButton
    , GR.endTimeSpinButton
    , GR.timeSlicesDrawingArea
    , GR.guiInFilePropertiesRef
    }
  = do
  when (isJust maybeVideoPreviewWidget && isJust maybePlaybinElement) $ do
    GI.Gtk.widgetSetTooltipText
      timeSlicesDrawingArea $
        Just "Click to change the video position."
    addOnMouseClickHandler
  void $
    GI.GLib.timeoutAdd
      GI.GLib.PRIORITY_DEFAULT
      1 $ do
        GI.Gtk.widgetQueueDraw timeSlicesDrawingArea
        return True
  void $
    GI.Gtk.onWidgetDraw
      timeSlicesDrawingArea $
        \ context -> do
          GR.GuiInFileProperties
            { GR.inFileDuration
            }                    <- readIORef guiInFilePropertiesRef
          when (inFileDuration > 0.0) $ do
            startTime            <- GI.Gtk.spinButtonGetValue startTimeSpinButton
            endTime              <- GI.Gtk.spinButtonGetValue endTimeSpinButton
            textOverlaysData     <- Prelude.reverse <$> GuiTextOverlays.getTextOverlaysData guiComponents
            drawingAreaWidth     <- int32ToDouble <$> GI.Gtk.widgetGetAllocatedWidth  timeSlicesDrawingArea
            drawingAreaHeight    <- int32ToDouble <$> GI.Gtk.widgetGetAllocatedHeight timeSlicesDrawingArea
            let durationTime     = endTime - startTime
            let startTime'       = startTime / inFileDuration
            let durationTime'    = durationTime / inFileDuration
            GiCairoCairoBridge.renderWithContext context $ do
              GRC.setSourceRGBA 0.1 0.1 0.1 1.0
              GRC.rectangle 0.0 0.0 drawingAreaWidth drawingAreaHeight
              GRC.fill

              grayPatternPng   <- liftIO $ getDataFileName "data/gray-pattern.png"
              purplePatternPng <- liftIO $ getDataFileName "data/purple-pattern.png"
              greenPatternPng  <- liftIO $ getDataFileName "data/green-pattern.png"

              graySurface      <- liftIO $ GRC.imageSurfaceCreateFromPNG grayPatternPng
              purpleSurface    <- liftIO $ GRC.imageSurfaceCreateFromPNG purplePatternPng
              greenSurface     <- liftIO $ GRC.imageSurfaceCreateFromPNG greenPatternPng

              GRC.withPatternForSurface
                graySurface $ \ grayPattern ->
                GRC.withPatternForSurface
                  purpleSurface $ \ purplePattern ->
                  GRC.withPatternForSurface
                    greenSurface $ \ greenPattern -> do
                    GRC.patternSetExtend grayPattern   GRC.ExtendRepeat
                    GRC.patternSetExtend purplePattern GRC.ExtendRepeat
                    GRC.patternSetExtend greenPattern  GRC.ExtendRepeat

                    let drawTextOverlay (s, e) = do
                          let d = e - s

                          drawRectPattern
                            greenPattern
                            (((s - startTime) / durationTime) * drawingAreaWidth)
                            0
                            ((d               / durationTime) * drawingAreaWidth)
                            (drawingAreaHeight / 2 )

                          drawRectPattern
                            greenPattern
                            ((s / inFileDuration) * drawingAreaWidth)
                            (drawingAreaHeight / 2)
                            ((d / inFileDuration) * drawingAreaWidth)
                            (drawingAreaHeight / 2)

                    let textOverlaysData' = filterEmptyTextOverlays textOverlaysData

                    -- Draw the background.
                    drawRectPattern
                      grayPattern
                      0.0
                      0.0
                      drawingAreaWidth
                      drawingAreaHeight

                    -- Draw the GIF slice in the top half.
                    drawRectPattern
                      purplePattern
                      0.0
                      0.0
                      drawingAreaWidth
                      (drawingAreaHeight / 2.0)

                    -- Draw the GIF slice in the bottom half.
                    drawRectPattern
                      purplePattern
                      (startTime'    * drawingAreaWidth)
                      (drawingAreaHeight / 2.0)
                      (durationTime' * drawingAreaWidth)
                      (drawingAreaHeight / 2.0)

                    -- Draw the text overlays with their controls hidden.
                    mapM_
                      drawTextOverlay
                      ( mergeTextOverlaysIntervals
                      $ filterControlsHiddenTextOverlays
                          textOverlaysData'
                      )

                    -- Draw the text overlays with their controls visible.
                    mapM_
                      drawTextOverlay
                      ( mergeTextOverlaysIntervals
                      $ filterControlsVisibleTextOverlays
                          textOverlaysData'
                      )

              (playbinDuration, playbinPosition) <-
                liftIO $ getPlaybinDurationAndPosition guiComponents
              case (playbinDuration, playbinPosition) of
                (Just playbinDuration', Just playbinPosition') -> do
                  let videoDuration' = nanosecondsToSeconds playbinDuration'
                  let videoPosition' = nanosecondsToSeconds playbinPosition'
                  when (videoDuration' > 0.0) $ do
                    when (durationTime > 0.0) $ do
                      -- Draw the video position line and clock in the top half.
                      let videoPositionLineX =
                            ((videoPosition' - startTime) / durationTime) * drawingAreaWidth - 1.0
                      GRC.rectangle
                        videoPositionLineX
                        0.0
                        2.0
                        (drawingAreaHeight / 2.0)
                      GRC.setSourceRGBA 1.0 1.0 1.0 1.0
                      GRC.fill

                      GRC.selectFontFace
                        ("monospace" :: String)
                        GRC.FontSlantNormal
                        GRC.FontWeightNormal
                      GRC.setFontSize 15.0
                      let videoPosition'' = printf "%.2f" videoPosition' :: String
                      textWidth  <- GRC.textExtentsWidth  <$> GRC.textExtents videoPosition''
                      textHeight <- GRC.textExtentsHeight <$> GRC.textExtents videoPosition''
                      let textX  = videoPositionLineX - (textWidth / 2.0)
                      let textX'
                            | textX <= 0                                = 0
                            | textX + textWidth + 1 >= drawingAreaWidth = drawingAreaWidth - textWidth - 1
                            | otherwise                                 = textX
                      let textY  = drawingAreaHeight / 4.0 - (textHeight / 2.0)
                      GRC.rectangle textX' textY (textWidth + 1.0) (textHeight + 1.0)
                      GRC.setSourceRGBA (48.0 / 255.0) (52/ 255.0) (58/ 255.0) 1.0
                      GRC.fill
                      GRC.setSourceRGBA 1.0 1.0 1.0 1.0
                      GRC.moveTo textX' (textY + textHeight)
                      GRC.showText videoPosition''

                    -- Draw the video position line in the bottom half.
                    GRC.rectangle
                      ((videoPosition' / videoDuration') * drawingAreaWidth - 1.0)
                      (drawingAreaHeight / 2.0)
                      2.0
                      (drawingAreaHeight / 2.0)
                    GRC.setSourceRGBA 1.0 1.0 1.0 1.0
                    GRC.fill
                _ -> return ()

              -- Dividing line.
              GRC.setSourceRGBA (48.0 / 255.0) (52/ 255.0) (58/ 255.0) 1.0
              GRC.rectangle 0.0 (drawingAreaHeight / 2.0 - 1.0) drawingAreaWidth 2.0
              GRC.fill
          return True
  where
    filterEmptyTextOverlays
      ::  [GR.GuiTextOverlayData]
      ->  [GR.GuiTextOverlayData]
    filterEmptyTextOverlays
      =
      Prelude.filter
        (not . Prelude.null . GR.textOverlayText)
    filterControlsVisibleTextOverlays
      ::  [GR.GuiTextOverlayData]
      ->  [GR.GuiTextOverlayData]
    filterControlsVisibleTextOverlays
      =
      Prelude.filter
        GR.textOverlayControlsVisible
    filterControlsHiddenTextOverlays
      ::  [GR.GuiTextOverlayData]
      ->  [GR.GuiTextOverlayData]
    filterControlsHiddenTextOverlays
      =
      Prelude.filter
        (not . GR.textOverlayControlsVisible)
    mergeTextOverlaysIntervals
      ::  [GR.GuiTextOverlayData]
      ->  [(Double, Double)]
    mergeTextOverlaysIntervals
      textOverlaysData
      =
      Prelude.foldl
        merge
        []
        sortedIntervals
      where
        merge
          ::  [(Double, Double)]
          ->  (Double, Double)
          ->  [(Double, Double)]
        merge
          (a@(s0, e0):xs)
           b@(s1, e1)
          =
          if e0 >= s1
            then (s0, Prelude.max e0 e1) : xs
            else b : a : xs
        merge
          []
          b
          =
          [b]
        sortedIntervals
          :: [(Double, Double)]
        sortedIntervals
          =
          Data.List.sortOn
            fst
            intervals
        intervals
          :: [(Double, Double)]
        intervals
          =
          Prelude.map
            (\ t -> (GR.textOverlayStartTime t, GR.textOverlayEndTime t))
            textOverlaysData
    addOnMouseClickHandler
      ::  IO ()
    addOnMouseClickHandler = do
      GI.Gtk.widgetAddEvents timeSlicesDrawingArea [GI.Gdk.EventMaskAllEventsMask]
      void $
        GI.Gtk.onWidgetButtonReleaseEvent
          timeSlicesDrawingArea $ \ eventButton -> do
            eventButtonNumber <- GI.Gdk.getEventButtonButton eventButton
            when (eventButtonNumber == 1) $ do
              x <- GI.Gdk.getEventButtonX eventButton
              y <- GI.Gdk.getEventButtonY eventButton
              (playbinDuration, _) <- liftIO $ getPlaybinDurationAndPosition guiComponents
              case (maybePlaybinElement, playbinDuration) of
                (Just playbinElement, Just playbinDuration') -> do
                  let videoDuration' = nanosecondsToSeconds playbinDuration'
                  drawingAreaWidth   <- int32ToDouble <$> GI.Gtk.widgetGetAllocatedWidth  timeSlicesDrawingArea
                  drawingAreaHeight  <- int32ToDouble <$> GI.Gtk.widgetGetAllocatedHeight timeSlicesDrawingArea
                  when (videoDuration' > 0.0 && drawingAreaWidth > 0.0) $ do
                    startTime         <- GI.Gtk.spinButtonGetValue startTimeSpinButton
                    endTime           <- GI.Gtk.spinButtonGetValue endTimeSpinButton
                    let durationTime  = endTime - startTime
                    let endTimeInNano = secondsToNanoseconds endTime
                    let pauseVideo    = void $ GI.Gst.elementSetState playbinElement GI.Gst.StatePaused
                    if y <= (drawingAreaHeight / 2.0)
                      then do
                        let a = x / drawingAreaWidth
                        let b = a * durationTime
                        let c = startTime + b
                        let seekToInNano = secondsToNanoseconds c
                        pauseVideo
                        seekPlaybinElement
                          guiComponents
                          (Just seekToInNano)
                          (Just endTimeInNano)
                      else do
                        let a = x / drawingAreaWidth
                        let b = a * videoDuration'
                        when (b >= startTime && b <= endTime) $ do
                          let seekToInNano = secondsToNanoseconds b
                          pauseVideo
                          seekPlaybinElement
                            guiComponents
                            (Just seekToInNano)
                            (Just endTimeInNano)
                _ -> return ()
            return True

drawRectPattern
  ::  GRC.Pattern
  ->  Double
  ->  Double
  ->  Double
  ->  Double
  ->  GRC.Render ()
drawRectPattern
  p
  x
  y
  w
  h
  = do
  GRC.setSource p
  GRC.rectangle x y w h
  GRC.fill

setupVideoPreviewPauseToggleButton :: GR.GuiComponents -> IO ()
setupVideoPreviewPauseToggleButton
  guiComponents@GR.GuiComponents
    { GR.maybeVideoPreviewWidget = (Just _)
    , GR.maybePlaybinElement     = (Just playbinElement)
    , GR.videoPreviewPauseToggleButton
    }
  = do
  GI.Gtk.widgetShow videoPreviewPauseToggleButton
  void $
    GI.Gtk.afterToggleButtonToggled
      videoPreviewPauseToggleButton $ do
        active          <- GI.Gtk.getToggleButtonActive videoPreviewPauseToggleButton
        (_, playing, _) <- GI.Gst.elementGetState playbinElement 0
        if active && playing == GI.Gst.StatePlaying
          then void $ GI.Gst.elementSetState playbinElement GI.Gst.StatePaused
          else playPlaybinElement guiComponents
        toggleToggleButtonLabel
          videoPreviewPauseToggleButton
          " Paused"
          " Pause"
          "Play preview?"
          "Pause preview?"
setupVideoPreviewPauseToggleButton
  GR.GuiComponents
    { GR.videoPreviewPauseToggleButton
    }
  = GI.Gtk.widgetHide videoPreviewPauseToggleButton

playPlaybinElement :: GR.GuiComponents -> IO ()
playPlaybinElement
  GR.GuiComponents
    { GR.maybeVideoPreviewWidget = (Just _)
    , GR.maybePlaybinElement     = (Just playbinElement)
    , GR.videoPreviewPauseToggleButton
    }
  = do
  pauseButtonActive  <- GI.Gtk.getToggleButtonActive videoPreviewPauseToggleButton
  (_, state, state') <- GI.Gst.elementGetState playbinElement 0
  when (  not pauseButtonActive
       && (state == GI.Gst.StatePaused || state' == GI.Gst.StatePaused)
       ) $
    void $ GI.Gst.elementSetState playbinElement GI.Gst.StatePlaying
playPlaybinElement _ = return ()

getPlaybinDurationAndPosition :: GR.GuiComponents -> IO (Maybe Int64, Maybe Int64)
getPlaybinDurationAndPosition
  GR.GuiComponents
    { GR.maybeVideoPreviewWidget = Just _videoPreviewWidget
    , GR.maybePlaybinElement     = Just playbinElement
    }
  = do
  (couldQueryDuration, playbinDuration) <-
    GI.Gst.elementQueryDuration playbinElement GI.Gst.FormatTime
  (couldQueryPosition, playbinPosition) <-
    GI.Gst.elementQueryPosition playbinElement GI.Gst.FormatTime
  if     couldQueryDuration
      && couldQueryPosition
      && playbinDuration >  0
      && playbinPosition >= 0
    then return (Just playbinDuration, Just playbinPosition)
    else return (Nothing, Nothing)
getPlaybinDurationAndPosition _ = return (Nothing, Nothing)

seekPlaybinElement
  ::  GR.GuiComponents
  ->  Maybe Int64
  ->  Maybe Int64
  ->  IO ()
seekPlaybinElement
  guiComponents@GR.GuiComponents
    { GR.startTimeSpinButton
    , GR.endTimeSpinButton
    , GR.maybeVideoPreviewWidget = Just _videoPreviewWidget
    , GR.maybePlaybinElement     = Just _playbinElement
    }
  Nothing
  Nothing
  = do
  let convert = secondsToNanoseconds
  startTime   <- convert <$> GI.Gtk.spinButtonGetValue startTimeSpinButton
  endTime     <- convert <$> GI.Gtk.spinButtonGetValue endTimeSpinButton
  seekPlaybinElement'
    guiComponents
    startTime
    endTime
seekPlaybinElement
  guiComponents@GR.GuiComponents
    { GR.maybeVideoPreviewWidget = Just _videoPreviewWidget
    , GR.maybePlaybinElement     = Just _playbinElement
    }
  (Just startTime)
  (Just endTime)
  =
  seekPlaybinElement'
    guiComponents
    startTime
    endTime
seekPlaybinElement _ _ _ = return ()

seekPlaybinElement'
  ::  GR.GuiComponents
  ->  Int64
  ->  Int64
  ->  IO ()
seekPlaybinElement'
  guiComponents@GR.GuiComponents
    { GR.maybeVideoPreviewWidget = Just _videoPreviewWidget
    , GR.maybePlaybinElement     = Just playbinElement
    }
  startTime
  endTime
  = do
  _ <- GI.Gst.elementSetState playbinElement GI.Gst.StatePaused
  void $
    GI.Gst.elementSeek
      playbinElement
      1.0
      GI.Gst.FormatTime
      [GI.Gst.SeekFlagsFlush, GI.Gst.SeekFlagsAccurate]
      GI.Gst.SeekTypeSet
      startTime
      GI.Gst.SeekTypeSet
        $ endTime + 1
  playPlaybinElement guiComponents
seekPlaybinElement' _ _ _ = return ()

resetVideoPreview
  ::  GR.GuiComponents
  ->  IO ()
resetVideoPreview
  GR.GuiComponents
    { GR.maybeVideoPreviewWidget = Just _videoPreviewWidget
    , GR.maybePlaybinElement     = Just playbinElement
    }
  = do
  void $ GI.Gst.elementSetState playbinElement GI.Gst.StateNull
  Data.GI.Base.Properties.setObjectPropertyString
    playbinElement
    "uri"
    (Just "")
  Data.GI.Base.Properties.setObjectPropertyDouble
    playbinElement
    "volume"
    0.0
resetVideoPreview _ = return ()

resetFirstAndLastFramePreview
  ::  GR.GuiComponents
  ->  IO ()
resetFirstAndLastFramePreview _ = return ()

resetWindow :: GR.GuiComponents -> IO ()
resetWindow
  GR.GuiComponents
    { GR.window
    , GR.mainPreviewBox
    , GR.imagesPreviewBox
    , GR.videoPreviewBox
    }
  = do
  GI.Gtk.widgetSetSizeRequest
    window
    (doubleToInt32 $ desiredPreviewSize + 300.0)
    (-1)
  GI.Gtk.widgetSetSizeRequest
    mainPreviewBox
    (doubleToInt32 desiredPreviewSize)
    (-1)
  GI.Gtk.widgetHide imagesPreviewBox
  GI.Gtk.widgetHide videoPreviewBox
  resizeWindow window

resizeWindow :: GI.Gtk.Window -> IO ()
resizeWindow window =
  void $ GI.Gtk.windowResize window 1 1

makeFirstAndLastFramePreview
  ::  GR.GuiMakeFramePreviewFunctionArgs
  ->  IO ()
makeFirstAndLastFramePreview
  guiMakeFramePreviewFunctionArgs@GR.GuiMakeFramePreviewFunctionArgs
    { GR.inFilePath
    , GR.startTime
    , GR.colorCount
    , GR.previewWidth
    , GR.dither
    , GR.firstFrameImage
    , GR.temporaryDirectory
    , GR.window
    }
  = do
  void $ forkIO $
    withTempDirectory
      temporaryDirectory
      framePreviewDirectoryName
        $ \ tmpDir -> do
          let outFilePath = tmpDir ++ [pathSeparator] ++ "gifcurry-first-frame-preview.gif"
          let inputValid  = not (Data.List.null inFilePath) && startTime >= 0.0
          void $
            setOrResetFramePrevew
              GR.GuiSetOrResetFramePrevewFunctionArgs
                { GR.inputValid   = inputValid
                , GR.inFilePath   = inFilePath
                , GR.outFilePath  = outFilePath
                , GR.time         = startTime
                , GR.colorCount   = colorCount
                , GR.previewWidth = previewWidth
                , GR.dither       = dither
                , GR.image        = firstFrameImage
                , GR.window       = window
                }
  makeLastFramePreview
    guiMakeFramePreviewFunctionArgs

makeLastFramePreview
  ::  GR.GuiMakeFramePreviewFunctionArgs
  ->  IO ()
makeLastFramePreview
  GR.GuiMakeFramePreviewFunctionArgs
    { GR.inFilePath
    , GR.endTime
    , GR.colorCount
    , GR.dither
    , GR.previewWidth
    , GR.lastFrameImage
    , GR.temporaryDirectory
    , GR.window
    }
  =
  void
    $ forkIO
      $ withTempDirectory
        temporaryDirectory
        framePreviewDirectoryName
          $ \ tmpDir -> do
            let outFilePath  = tmpDir ++ [pathSeparator] ++ "gifcurry-last-frame-preview.gif"
            let inputValid   = endTime > 0.0 && not (Data.List.null inFilePath)
            void
              $ setOrResetFramePrevew
                GR.GuiSetOrResetFramePrevewFunctionArgs
                  { GR.inputValid   = inputValid
                  , GR.inFilePath   = inFilePath
                  , GR.outFilePath  = outFilePath
                  , GR.time         = endTime
                  , GR.colorCount   = colorCount
                  , GR.dither       = dither
                  , GR.previewWidth = previewWidth
                  , GR.image        = lastFrameImage
                  , GR.window       = window
                  }

newtype InputFile    = InputFile    String
newtype OutputFile   = OutputFile   String
newtype StartTime    = StarTime     Double
newtype ColorCount   = ColorCount   Double
newtype PreviewWidth = PreviewWidth Double
newtype Dither       = Dither       Bool

setOrResetFramePrevew
  ::  GR.GuiSetOrResetFramePrevewFunctionArgs
  ->  IO ()
setOrResetFramePrevew
  GR.GuiSetOrResetFramePrevewFunctionArgs
    { GR.inputValid = False
    , GR.image
    , GR.window
    }
  =
  GtkMainSyncAsync.gtkMainAsync $ do
    resetImage image
    resizeWindow window
setOrResetFramePrevew
  GR.GuiSetOrResetFramePrevewFunctionArgs
    { GR.inputValid = True
    , GR.inFilePath
    , GR.outFilePath
    , GR.time
    , GR.colorCount
    , GR.dither
    , GR.previewWidth
    , GR.image
    , GR.window
    }
  = do
  result <-
    makeImagePreview
      (InputFile inFilePath)
      (OutputFile outFilePath)
      (StarTime time)
      (ColorCount colorCount)
      (PreviewWidth previewWidth)
      (Dither dither)
  case result of
    Left _ ->
      void $ updatePreviewFrame "" image False
    Right filePath ->
      void $ updatePreviewFrame filePath image True
  GtkMainSyncAsync.gtkMainAsync $ resizeWindow window

makeImagePreview
  ::  InputFile
  ->  OutputFile
  ->  StartTime
  ->  ColorCount
  ->  PreviewWidth
  ->  Dither
  ->  IO (Either IOError String)
makeImagePreview
  (InputFile inputFile)
  (OutputFile outputFile)
  (StarTime startTime)
  (ColorCount colorCount)
  (PreviewWidth previewWidth)
  (Dither dither)
  =
  Gifcurry.createGif
    $ Gifcurry.defaultGifParams
      { Gifcurry.inputFile    = inputFile
      , Gifcurry.outputFile   = outputFile
      , Gifcurry.saveAsVideo  = False
      , Gifcurry.startTime    = startTime
      , Gifcurry.endTime      = startTime + 0.001
      , Gifcurry.fps          = 15
      , Gifcurry.width        = round previewWidth :: Int
      , Gifcurry.colorCount   = round colorCount   :: Int
      , Gifcurry.dither       = dither
      }

getPreviewWidthAndHeight
  ::  Double
  ->  Double
  ->  (Double, Double)
getPreviewWidthAndHeight inFileWidth inFileHeight = (previewWidth, previewHeight)
  where
    widthRatio    :: Double
    widthRatio    = inFileWidth  / inFileHeight
    heightRatio   :: Double
    heightRatio   = inFileHeight / inFileWidth
    previewWidth  :: Double
    previewWidth  =
      if inFileWidth >= inFileHeight
        then desiredPreviewSize
        else (desiredPreviewSize / 2.0) * widthRatio
    previewHeight :: Double
    previewHeight =
      if inFileWidth >= inFileHeight
        then desiredPreviewSize * heightRatio
        else desiredPreviewSize / 2.0

updatePreviewFrame
  ::  String
  ->  GI.Gtk.Image
  ->  Bool
  ->  IO ()
updatePreviewFrame filePath image True =
  GtkMainSyncAsync.gtkMainSync (GI.Gtk.imageSetFromFile image (Just filePath))
updatePreviewFrame _ image False =
  GtkMainSyncAsync.gtkMainSync (resetImage image)

resetImage
  ::  GI.Gtk.Image
  ->  IO ()
resetImage image =
  GI.Gtk.imageSetFromIconName
    image
    (Just $ pack blankPreviewIcon)
    (enumToInt32 GI.Gtk.IconSizeButton)

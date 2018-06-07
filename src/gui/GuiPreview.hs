{-
  Gifcurry
  (C) 2018 David Lettier
  lettier.com
-}

{-# LANGUAGE
    OverloadedStrings
  , NamedFieldPuns
#-}

module GuiPreview where

import GHC.Float
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
  ( gif
  , GifParams(..)
  , Quality(QualityLow)
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
        , GR.maybePlaybinElement     = (Just _)
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
            preview guiComponents videoPreview
      videoPreviewOverlayChildBoxChildCount <-
        Data.List.length <$> GI.Gtk.containerGetChildren videoPreviewOverlayChildBox
      when (videoPreviewOverlayChildBoxChildCount <= 0) $ do
        _ <- GI.Gtk.boxPackStart videoPreviewOverlayChildBox videoPreviewWidget True True 0
        return ()
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
            preview guiComponents firstAndLastFramePreview
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
  ->  (  GR.GuiComponents
      -> GR.GuiPreviewState
      -> String
      -> Float
      -> Float
      -> Float
      -> Float
      -> IO ()
      )
  ->  IO Bool
preview
  guiComponents@GR.GuiComponents
    { GR.startTimeSpinButton
    , GR.durationTimeSpinButton
    , GR.guiPreviewStateRef
    , GR.inVideoPropertiesRef
    }
  f
  = do
  GR.InVideoProperties
    { GR.inVideoUri = inFilePath
    , GR.inVideoWidth
    , GR.inVideoHeight
    } <- readIORef inVideoPropertiesRef
  guiPreviewState <- readIORef guiPreviewStateRef
  startTime       <- double2Float <$> GI.Gtk.spinButtonGetValue startTimeSpinButton
  durationTime    <- double2Float <$> GI.Gtk.spinButtonGetValue durationTimeSpinButton
  let invalidInFilePath    = inFilePath    == ""
  let invalidStartTime     = startTime     < 0.0
  let invalidDurationTime  = durationTime  <= 0.0
  let invalidInVideoWidth  = inVideoWidth  <= 0.0
  let invalidInVideoHeight = inVideoHeight <= 0.0
  let inputInvalid         =
           invalidInFilePath
        || invalidStartTime
        || invalidDurationTime
        || invalidInVideoWidth
        || invalidInVideoHeight
  if not inputInvalid
    then
      f
        guiComponents
        guiPreviewState
        inFilePath
        startTime
        durationTime
        inVideoWidth
        inVideoHeight
    else
      resetWindow guiComponents
  atomicModifyIORef' guiPreviewStateRef $
    \ guiPreviewState' ->
      ( guiPreviewState'
          { GR.maybeInFilePath   = if invalidInFilePath   then Nothing else Just inFilePath
          , GR.maybeStartTime    = if invalidStartTime    then Nothing else Just startTime
          , GR.maybeDurationTime = if invalidDurationTime then Nothing else Just durationTime
          , GR.loopRunning = True
          }
      , ()
      )
  return True

videoPreview
  ::  GR.GuiComponents
  ->  GR.GuiPreviewState
  ->  String
  ->  Float
  ->  Float
  ->  Float
  ->  Float
  ->  IO ()
videoPreview
  guiComponents@GR.GuiComponents
    { GR.window
    , GR.cropToggleButton
    , GR.textOverlaysToggleButton
    , GR.mainPreviewBox
    , GR.videoPreviewBox
    , GR.maybePlaybinElement = (Just playbinElement)
    }
  GR.GuiPreviewState
    { GR.maybeInFilePath
    , GR.maybeStartTime
    }
  inFilePath
  startTime
  durationTime
  inVideoWidth
  inVideoHeight
  = do
  GI.Gtk.widgetShow videoPreviewBox
  sizePreviewAndWindow
  handleChanges
  updateVideoPreviewAspectRatio
  where
    sizePreviewAndWindow :: IO ()
    sizePreviewAndWindow = do
      let (previewWidth, previewHeight) =
            getPreviewWidthAndHeight inVideoWidth inVideoHeight
      GI.Gtk.widgetSetSizeRequest
        window
        (floatToInt32 previewWidth)
        (-1)
      GI.Gtk.widgetSetSizeRequest
        mainPreviewBox
        (floatToInt32 previewWidth)
        (floatToInt32 previewHeight)
      resizeWindow window
      return ()
    handleChanges :: IO ()
    handleChanges = do
      (playbinDuration, playbinPosition) <- getPlaybinDurationAndPosition guiComponents
      let videoDuration     = fromMaybe (-1) playbinDuration
      let videoPosition     = fromMaybe (-1) playbinPosition
      let endTime           = startTime + durationTime
      let startTimeInNano   = secondsToNanoseconds startTime
      let endTimeInNano     = secondsToNanoseconds endTime
      let inFilePathChanged = fromMaybe "" maybeInFilePath /= inFilePath
      let startTimeChanged  = fromMaybe (-1.0) maybeStartTime /= startTime
      let nearTheEnd        = (videoDuration - videoPosition) <= 500000
      let startOver         =
               videoDuration > 0
            && (  videoPosition  >= videoDuration
               || videoPosition  >= endTimeInNano
               || videoPosition  <  startTimeInNano
               || nearTheEnd
               )
      let seekToStart = startTimeChanged || inFilePathChanged || startOver
      when inFilePathChanged $ do
        _ <- GI.Gst.elementSetState playbinElement GI.Gst.StateNull
        Data.GI.Base.Properties.setObjectPropertyString
          playbinElement
          "uri"
          (Just $ pack $ "file://" ++ inFilePath)
        _ <- Data.GI.Base.Properties.setObjectPropertyDouble playbinElement "volume" 0.0
        resizeWindow window
        return ()
      when seekToStart $ do
        _ <- GI.Gst.elementSetState playbinElement GI.Gst.StatePaused
        _ <- GI.Gst.elementSeekSimple
          playbinElement
          GI.Gst.FormatTime
          [GI.Gst.SeekFlagsFlush]
          startTimeInNano
        playPlaybinElement guiComponents
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
videoPreview _ _ _ _ _ _ _ = return ()

firstAndLastFramePreview
  ::  GR.GuiComponents
  ->  GR.GuiPreviewState
  ->  String
  ->  Float
  ->  Float
  ->  Float
  ->  Float
  ->  IO ()
firstAndLastFramePreview
  GR.GuiComponents
    { GR.window
    , GR.imagesPreviewBox
    , GR.firstFrameImage
    , GR.lastFrameImage
    , GR.firstFramePreviewImageDrawingArea
    , GR.lastFramePreviewImageDrawingArea
    , GR.temporaryDirectory
    }
  GR.GuiPreviewState
    { GR.maybeInFilePath
    , GR.maybeStartTime
    , GR.maybeDurationTime
    }
  inFilePath
  startTime
  durationTime
  inVideoWidth
  inVideoHeight
  = do
  GI.Gtk.widgetShow imagesPreviewBox
  handleChanges
  redrawDrawingAreas
  where
    handleChanges :: IO ()
    handleChanges = do
      let inFilePathChanged      = fromMaybe "" maybeInFilePath /= inFilePath
      let startTimeChanged       = fromMaybe (-1.0) maybeStartTime /= startTime
      let durationTimeChanged    = fromMaybe (-1.0) maybeDurationTime /= durationTime
      let firstAndLastFrameDirty = inFilePathChanged || startTimeChanged
      let lastFrameDirty         = not firstAndLastFrameDirty && durationTimeChanged
      let (previewWidth, _)      = getPreviewWidthAndHeight inVideoWidth inVideoHeight
      when firstAndLastFrameDirty $
        makeFirstAndLastFramePreview
          inFilePath
          startTime
          durationTime
          (previewWidth / 2.0)
          firstFrameImage
          lastFrameImage
          temporaryDirectory
          window
      when lastFrameDirty $
        makeLastFramePreview
          inFilePath
          startTime
          durationTime
          (previewWidth / 2.0)
          lastFrameImage
          temporaryDirectory
          window
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

drawCropGrid :: GR.GuiComponents -> GI.Gtk.DrawingArea -> GI.Cairo.Context -> IO Bool
drawCropGrid
  GR.GuiComponents
    { GR.cropToggleButton
    , GR.leftCropSpinButton
    , GR.rightCropSpinButton
    , GR.topCropSpinButton
    , GR.bottomCropSpinButton
    , GR.inVideoPropertiesRef
    }
  drawingArea
  context
  = do
  GR.InVideoProperties
    { GR.inVideoWidth
    , GR.inVideoHeight
    }               <- readIORef inVideoPropertiesRef
  cropModeEnabled   <- GI.Gtk.getToggleButtonActive cropToggleButton
  drawingAreaWidth  <-
    int32ToDouble <$> GI.Gtk.widgetGetAllocatedWidth drawingArea
  drawingAreaHeight <-
    int32ToDouble <$> GI.Gtk.widgetGetAllocatedHeight drawingArea
  left   <- GI.Gtk.spinButtonGetValue leftCropSpinButton
  right  <- GI.Gtk.spinButtonGetValue rightCropSpinButton
  top    <- GI.Gtk.spinButtonGetValue topCropSpinButton
  bottom <- GI.Gtk.spinButtonGetValue bottomCropSpinButton
  when (cropModeEnabled && inVideoWidth > 0.0 && inVideoHeight > 0.0) $
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
            0.0
            0.0
            drawingAreaWidth
            (top * drawingAreaHeight)
          -- Bottom
          drawRectPattern
            orangePattern
            0.0
            (drawingAreaHeight - bottom * drawingAreaHeight)
            drawingAreaWidth
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
    , GR.durationTimeSpinButton
    , GR.textOverlaysToggleButton
    , GR.inVideoPropertiesRef
    }
  drawingArea
  forFramePreview
  context
  = do
  GR.InVideoProperties
    { GR.inVideoWidth
    , GR.inVideoHeight
    , GR.inVideoDuration
    } <- readIORef inVideoPropertiesRef
  textOverlayModeEnabled <- GI.Gtk.getToggleButtonActive textOverlaysToggleButton
  when (textOverlayModeEnabled && inVideoWidth > 0.0 && inVideoHeight > 0.0) $ do
    (videoDuration, videoPosition) <- getVideoDurationAndPosition (floatToDouble inVideoDuration)
    drawingAreaWidth  <- int32ToDouble <$> GI.Gtk.widgetGetAllocatedWidth  drawingArea
    drawingAreaHeight <- int32ToDouble <$> GI.Gtk.widgetGetAllocatedHeight drawingArea
    GuiTextOverlays.updateTextOverlays False guiComponents
    textOverlaysData <- GuiTextOverlays.getTextOverlaysData guiComponents
    GiCairoCairoBridge.renderWithContext context $
      mapM_
        (renderTextOverlayData videoDuration videoPosition drawingAreaWidth drawingAreaHeight)
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
      _videoDuration
      videoPosition
      drawingAreaWidth
      drawingAreaHeight
      GR.GuiTextOverlayData
        { GR.textOverlayText
        , GR.textOverlayLeft
        , GR.textOverlayTop
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
            if      textOverlayStartTime <= videoPosition
                &&  textOverlayEndTime   >= videoPosition
              then 1.0
              else 0.3
      let x  = (textOverlayLeft + 0.5) * drawingAreaWidth
      let y  = (textOverlayTop  + 0.5) * drawingAreaHeight
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
    getVideoDurationAndPosition :: Double -> IO (Double, Double)
    getVideoDurationAndPosition inVideoDuration = do
      (playbinDuration, playbinPosition) <- getPlaybinDurationAndPosition guiComponents
      case (playbinDuration, playbinPosition) of
        (Just videoDuration, Just videoPosition) ->
          return
            ( nanosecondsToSeconds videoDuration
            , nanosecondsToSeconds videoPosition
            )
        _ ->
          case forFramePreview of
            ForFramePreviewFirst -> do
              videoPosition      <- GI.Gtk.spinButtonGetValue startTimeSpinButton
              return (inVideoDuration, videoPosition)
            ForFramePreviewLast  -> do
              startTime          <- GI.Gtk.spinButtonGetValue startTimeSpinButton
              durationTime       <- GI.Gtk.spinButtonGetValue durationTimeSpinButton
              let videoPosition  = startTime + durationTime
              return (inVideoDuration, videoPosition)
            ForFramePreviewNone  -> return (0.0, -1.0)

runTimeSlicesWidget :: GR.GuiComponents -> IO ()
runTimeSlicesWidget
  guiComponents@GR.GuiComponents
    { GR.maybeVideoPreviewWidget
    , GR.maybePlaybinElement
    , GR.startTimeSpinButton
    , GR.durationTimeSpinButton
    , GR.timeSlicesDrawingArea
    , GR.inVideoPropertiesRef
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
          GR.InVideoProperties
            { GR.inVideoDuration
            }                    <- readIORef inVideoPropertiesRef
          when (inVideoDuration > 0.0) $ do
            startTime            <- GI.Gtk.spinButtonGetValue startTimeSpinButton
            durationTime         <- GI.Gtk.spinButtonGetValue durationTimeSpinButton
            textOverlaysData     <- GuiTextOverlays.getTextOverlaysData guiComponents
            drawingAreaWidth     <- int32ToDouble <$> GI.Gtk.widgetGetAllocatedWidth  timeSlicesDrawingArea
            drawingAreaHeight    <- int32ToDouble <$> GI.Gtk.widgetGetAllocatedHeight timeSlicesDrawingArea
            let inVideoDuration' = floatToDouble  inVideoDuration
            let startTime'       = startTime    / inVideoDuration'
            let durationTime'    = durationTime / inVideoDuration'
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

                    -- Background.
                    drawRectPattern
                      grayPattern
                      0.0
                      0.0
                      drawingAreaWidth
                      drawingAreaHeight

                    -- GIF time slice.
                    drawRectPattern
                      purplePattern
                      0.0
                      0.0
                      drawingAreaWidth
                      (drawingAreaHeight / 2.0)

                    -- Video duration.
                    drawRectPattern
                      purplePattern
                      (startTime' * drawingAreaWidth)
                      (drawingAreaHeight / 2.0)
                      (durationTime' * drawingAreaWidth)
                      (drawingAreaHeight / 2.0)

                    mapM_
                      (\ GR.GuiTextOverlayData
                          { GR.textOverlayText
                          , GR.textOverlayStartTime
                          , GR.textOverlayDurationTime
                          }
                        ->
                        unless (Data.List.null textOverlayText) $ do
                          -- GIF time slice.
                          drawRectPattern
                            greenPattern
                            (((textOverlayStartTime - startTime) / durationTime) * drawingAreaWidth)
                            0.0
                            ((textOverlayDurationTime / durationTime)            * drawingAreaWidth)
                            (drawingAreaHeight / 2.0)

                          -- Video duration.
                          drawRectPattern
                            greenPattern
                            ((textOverlayStartTime / inVideoDuration')    * drawingAreaWidth)
                            (drawingAreaHeight / 2.0)
                            ((textOverlayDurationTime / inVideoDuration') * drawingAreaWidth)
                            (drawingAreaHeight / 2.0)
                      )
                      textOverlaysData

              (playbinDuration, playbinPosition) <-
                liftIO $ getPlaybinDurationAndPosition guiComponents
              case (playbinDuration, playbinPosition) of
                (Just playbinDuration', Just playbinPosition') -> do
                  let videoDuration' = nanosecondsToSeconds playbinDuration'
                  let videoPosition' = nanosecondsToSeconds playbinPosition'
                  when (videoDuration' > 0.0) $ do
                    when (durationTime > 0.0) $ do
                      -- GIF time slice.
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
                      let textX = videoPositionLineX - (textWidth / 2.0)
                      let textY = drawingAreaHeight / 4.0 - (textHeight / 2.0)
                      GRC.rectangle textX textY (textWidth + 1.0) (textHeight + 1.0)
                      GRC.setSourceRGBA (48.0 / 255.0) (52/ 255.0) (58/ 255.0) 1.0
                      GRC.fill
                      GRC.setSourceRGBA 1.0 1.0 1.0 1.0
                      GRC.moveTo textX (textY + textHeight)
                      GRC.showText videoPosition''

                    -- Video duration.
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
    addOnMouseClickHandler :: IO ()
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
                    durationTime      <- GI.Gtk.spinButtonGetValue durationTimeSpinButton
                    _                 <- GI.Gst.elementSetState playbinElement GI.Gst.StatePaused
                    let seekPlaybinTo t =
                          void $
                            GI.Gst.elementSeekSimple
                              playbinElement
                              GI.Gst.FormatTime
                              [GI.Gst.SeekFlagsFlush]
                              t
                    if y <= (drawingAreaHeight / 2.0)
                      then do
                        let a = x / drawingAreaWidth
                        let b = a * durationTime
                        let c = startTime + b
                        let seekToInNano = secondsToNanoseconds $ doubleToFloat c
                        seekPlaybinTo seekToInNano
                      else do
                        let a = x / drawingAreaWidth
                        let b = a * videoDuration'
                        when (b >= startTime && b <= startTime + durationTime) $ do
                          let seekToInNano = secondsToNanoseconds $ doubleToFloat b
                          seekPlaybinTo seekToInNano
                    playPlaybinElement guiComponents
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
        (_, playing, _) <- GI.Gst.elementGetState playbinElement (-1)
        if active && playing == GI.Gst.StatePlaying
          then void $ GI.Gst.elementSetState playbinElement GI.Gst.StatePaused
          else playPlaybinElement guiComponents
        if active
          then GI.Gtk.setButtonLabel videoPreviewPauseToggleButton "Paused"
          else GI.Gtk.setButtonLabel videoPreviewPauseToggleButton "Pause"
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
  (_, state, state') <- GI.Gst.elementGetState playbinElement (-1)
  when ( not pauseButtonActive
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
    (floatToInt32 $ desiredPreviewSize + 300.0)
    (-1)
  GI.Gtk.widgetSetSizeRequest
    mainPreviewBox
    (floatToInt32 desiredPreviewSize)
    (-1)
  GI.Gtk.widgetHide imagesPreviewBox
  GI.Gtk.widgetHide videoPreviewBox
  resizeWindow window

resizeWindow :: GI.Gtk.Window -> IO ()
resizeWindow window =
  void $ GI.Gtk.windowResize window 1 1

makeFirstAndLastFramePreview
  ::  String
  ->  Float
  ->  Float
  ->  Float
  ->  GI.Gtk.Image
  ->  GI.Gtk.Image
  ->  System.FilePath.FilePath
  ->  GI.Gtk.Window
  ->  IO ()
makeFirstAndLastFramePreview
  inFilePath
  startTime
  durationTime
  previewWidth
  firstFrameImage
  lastFrameImage
  temporaryDirectory
  window
  = do
  void $ forkIO $
    withTempDirectory
      temporaryDirectory
      framePreviewDirectoryName $
        \ tmpDir -> do
          let outFilePath = tmpDir ++ "/gifcurry-first-frame-preview.gif"
          let inputValid  = not (Data.List.null inFilePath) && startTime >= 0.0
          void $
            setOrResetFramePrevew
              inputValid
              inFilePath
              outFilePath
              startTime
              previewWidth
              firstFrameImage
              window
  makeLastFramePreview
    inFilePath
    startTime
    durationTime
    previewWidth
    lastFrameImage
    temporaryDirectory
    window

makeLastFramePreview
  ::  String
  ->  Float
  ->  Float
  ->  Float
  ->  GI.Gtk.Image
  ->  System.FilePath.FilePath
  ->  GI.Gtk.Window
  ->  IO ()
makeLastFramePreview
  inFilePath
  startTime
  durationTime
  previewWidth
  lastFrameImage
  temporaryDirectory
  window
  =
  void $ forkIO $
    withTempDirectory
      temporaryDirectory
      framePreviewDirectoryName $
        \ tmpdir -> do
          let startTime'  = startTime + durationTime
          let outFilePath = tmpdir ++ "/gifcurry-last-frame-preview.gif"
          let inputValid  = startTime' > 0.0 && not (Data.List.null inFilePath)
          void $
            setOrResetFramePrevew
              inputValid
              inFilePath
              outFilePath
              startTime'
              previewWidth
              lastFrameImage
              window

setOrResetFramePrevew
  ::  Bool
  ->  String
  ->  String
  ->  Float
  ->  Float
  ->  GI.Gtk.Image
  ->  GI.Gtk.Window
  ->  IO ()
setOrResetFramePrevew False _ _ _ _ image window =
  GtkMainSyncAsync.gtkMainAsync $ do
    resetImage image
    resizeWindow window
setOrResetFramePrevew
  True
  inFilePath
  outFilePath
  time
  previewWidth
  image
  window
  = do
  result <- makeImagePreview inFilePath outFilePath time previewWidth
  case result of
    Left _ ->
      void $ updatePreviewFrame "" image False
    Right filePath ->
      void $ updatePreviewFrame filePath image True
  GtkMainSyncAsync.gtkMainAsync $ resizeWindow window

makeImagePreview
  ::  String
  ->  String
  ->  Float
  ->  Float
  ->  IO (Either IOError String)
makeImagePreview inputFile outputFile startTime previewWidth =
  Gifcurry.gif $
    Gifcurry.defaultGifParams
      { Gifcurry.inputFile    = inputFile
      , Gifcurry.outputFile   = outputFile
      , Gifcurry.saveAsVideo  = False
      , Gifcurry.startTime    = startTime
      , Gifcurry.durationTime = 0.001
      , Gifcurry.widthSize    = round previewWidth :: Int
      , Gifcurry.quality      = Gifcurry.QualityLow
      }

getPreviewWidthAndHeight :: Float -> Float -> (Float, Float)
getPreviewWidthAndHeight inVideoWidth inVideoHeight = (previewWidth, previewHeight)
  where
    widthRatio    :: Float
    widthRatio    = inVideoWidth  / inVideoHeight
    heightRatio   :: Float
    heightRatio   = inVideoHeight / inVideoWidth
    previewWidth  :: Float
    previewWidth  =
      if inVideoWidth >= inVideoHeight
        then desiredPreviewSize
        else (desiredPreviewSize / 2.0) * widthRatio
    previewHeight :: Float
    previewHeight =
      if inVideoWidth >= inVideoHeight
        then desiredPreviewSize * heightRatio
        else desiredPreviewSize / 2.0

updatePreviewFrame :: String -> GI.Gtk.Image -> Bool -> IO ()
updatePreviewFrame filePath image True =
  GtkMainSyncAsync.gtkMainSync (GI.Gtk.imageSetFromFile image (Just filePath))
updatePreviewFrame _ image False =
  GtkMainSyncAsync.gtkMainSync (resetImage image)

resetImage :: GI.Gtk.Image -> IO ()
resetImage image =
  GI.Gtk.imageSetFromIconName
    image
    (Just $ pack blankPreviewIcon)
    (enumToInt32 GI.Gtk.IconSizeButton)

secondsToNanoseconds :: Float -> Int64
secondsToNanoseconds s =
  fromIntegral (round (s * 1000000000.0) :: Integer) :: Int64

nanosecondsToSeconds :: Int64 -> Double
nanosecondsToSeconds s =
  int64ToDouble s * (1.0 / 1000000000.0)

desiredPreviewSize :: Float
desiredPreviewSize = 700.0

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
import Data.Int
import Data.Maybe
import Data.Text
import Data.List
import Data.Bits
import Data.IORef
import Data.GI.Base.Properties
import Control.Monad
import Control.Concurrent
import qualified GI.GLib
import qualified GI.Gtk
import qualified GI.Gst
import qualified GI.Cairo
import qualified GiCairoCairoBridge
import qualified Graphics.Rendering.Cairo as GRC

import qualified Gifcurry
  ( gif
  , GifParams(..)
  , defaultGifParams
  )
import qualified GtkMainSyncAsync (gtkMainSync, gtkMainAsync)
import qualified GuiRecords as GR
import GuiMisc

blankPreviewIcon :: String
blankPreviewIcon = "gtk-discard"

framePreviewDirectoryName :: String
framePreviewDirectoryName = "gifcurry-frame-previews"

buildVideoPreviewWidgetAndPlaybinElement :: IO (Maybe GI.Gtk.Widget, Maybe GI.Gst.Element)
buildVideoPreviewWidgetAndPlaybinElement = do
  maybeGtkSink <- GI.Gst.elementFactoryMake "gtksink" (Just "MultimediaPlayerGtkSink")
  maybeVideoPreviewWidget <-
    case maybeGtkSink of
      Nothing      -> return Nothing
      Just gtkSink ->
        Data.GI.Base.Properties.getObjectPropertyObject gtkSink "widget" GI.Gtk.Widget
  maybePlaybinElement <- GI.Gst.elementFactoryMake "playbin" (Just "MultimediaPlayerPlaybin")
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
runGuiPreview
  guiComponents@GR.GuiComponents
    { GR.maybeVideoPreviewWidget = (Just videoPreviewWidget)
    , GR.maybePlaybinElement = (Just _)
    , GR.mainPreviewBox
    , GR.videoPreviewBox
    , GR.videoPreviewOverlayChildBox
    , GR.videoPreviewDrawingArea
    }
  = do
  mainPreviewBoxChildCount <-
    Data.List.length <$> GI.Gtk.containerGetChildren mainPreviewBox
  when (mainPreviewBoxChildCount <= 0) $
    GI.Gtk.boxPackStart mainPreviewBox videoPreviewBox True True 0
  videoPreviewOverlayChildBoxChildCount <-
    Data.List.length <$> GI.Gtk.containerGetChildren videoPreviewOverlayChildBox
  when (videoPreviewOverlayChildBoxChildCount <= 0) $ do
    _ <- GI.Gtk.boxPackStart videoPreviewOverlayChildBox videoPreviewWidget True True 0
    _ <- GI.Gtk.onWidgetDraw
      videoPreviewDrawingArea
      (drawCropGrid guiComponents videoPreviewDrawingArea)
    return ()
  runPreviewLoopIfNotRunning
    guiComponents $
      preview guiComponents videoPreview
runGuiPreview
  guiComponents@GR.GuiComponents
    { GR.mainPreviewBox
    , GR.imagesPreviewBox
    , GR.firstFramePreviewImageDrawingArea
    , GR.lastFramePreviewImageDrawingArea
    }
  = do
  childrenCount <- Data.List.length <$> GI.Gtk.containerGetChildren mainPreviewBox
  when (childrenCount <= 0) $ do
    _ <- GI.Gtk.boxPackStart mainPreviewBox imagesPreviewBox True True 0
    _ <- GI.Gtk.onWidgetDraw
      firstFramePreviewImageDrawingArea
      (drawCropGrid guiComponents firstFramePreviewImageDrawingArea)
    _ <- GI.Gtk.onWidgetDraw
      lastFramePreviewImageDrawingArea
      (drawCropGrid guiComponents lastFramePreviewImageDrawingArea)
    return ()
  runPreviewLoopIfNotRunning
    guiComponents $
      preview guiComponents firstAndLastFramePreview

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
      readIORef guiPreviewStateRef >>=
        \ x -> return $ GR.loopRunning x

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
  let inputInvalid =
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
  atomicWriteIORef guiPreviewStateRef
    guiPreviewState
      { GR.maybeInFilePath   = if invalidInFilePath   then Nothing else Just inFilePath
      , GR.maybeStartTime    = if invalidStartTime    then Nothing else Just startTime
      , GR.maybeDurationTime = if invalidDurationTime then Nothing else Just durationTime
      , GR.loopRunning = True
      }
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
  GR.GuiComponents
    { GR.window
    , GR.cropToggleButton
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
  handleCropMode
  where
    sizePreviewAndWindow :: IO ()
    sizePreviewAndWindow = do
      let widthRatio  = inVideoWidth  / inVideoHeight
      let heightRatio = inVideoHeight / inVideoWidth
      let previewWidth =
            if inVideoWidth >= inVideoHeight
              then desiredPreviewSize
              else desiredPreviewSize * widthRatio
      let previewHeight =
            if inVideoWidth < inVideoHeight
              then desiredPreviewSize
              else desiredPreviewSize * heightRatio
      let previewWidth'  = floatToInt32 previewWidth
      let previewHeight' = floatToInt32 previewHeight
      GI.Gtk.widgetSetSizeRequest
        window
        previewWidth'
        (-1)
      GI.Gtk.widgetSetSizeRequest
        mainPreviewBox
        previewWidth'
        previewHeight'
      resizeWindow window
    handleChanges :: IO ()
    handleChanges = do
      (couldQueryDuration, videoDuration) <-
        GI.Gst.elementQueryDuration playbinElement GI.Gst.FormatTime
      (couldQueryPosition, videoPosition) <-
        GI.Gst.elementQueryPosition playbinElement GI.Gst.FormatTime
      let endTime = startTime + durationTime
      let startTimeInNano = secondsToNanoseconds startTime
      let endTimeInNano   = secondsToNanoseconds endTime
      let inFilePathChanged = fromMaybe "" maybeInFilePath /= inFilePath
      let startTimeChanged  = fromMaybe (-1.0) maybeStartTime /= startTime
      let startOver =
               (couldQueryDuration && couldQueryPosition && videoDuration > 0)
            && (  videoPosition >= videoDuration
               || videoPosition >= endTimeInNano
               || videoPosition < startTimeInNano
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
      when seekToStart $ do
        _ <- GI.Gst.elementSetState playbinElement GI.Gst.StatePaused
        _ <- GI.Gst.elementSeekSimple
          playbinElement
          GI.Gst.FormatTime
          [GI.Gst.SeekFlagsFlush]
          startTimeInNano
        _ <- GI.Gst.elementSetState playbinElement GI.Gst.StatePlaying
        return ()
    handleCropMode :: IO ()
    handleCropMode = do
      cropModeEnabled <- GI.Gtk.getToggleButtonActive cropToggleButton
      if cropModeEnabled
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
  _
  _
  = do
  GI.Gtk.widgetShow imagesPreviewBox
  handleChanges
  redrawDrawingAreas
  where
    handleChanges :: IO ()
    handleChanges = do
      let inFilePathChanged =
            fromMaybe "" maybeInFilePath /= inFilePath
      let startTimeChanged =
            fromMaybe (-1.0) maybeStartTime /= startTime
      let durationTimeChanged =
            fromMaybe (-1.0) maybeDurationTime /= durationTime
      let firstAndLastFrameDirty = inFilePathChanged || startTimeChanged
      let lastFrameDirty = not firstAndLastFrameDirty && durationTimeChanged
      when firstAndLastFrameDirty $
        makeFirstAndLastFramePreview
          inFilePath
          startTime
          durationTime
          firstFrameImage
          lastFrameImage
          temporaryDirectory
          window
      when lastFrameDirty $
        makeLastFramePreview
          inFilePath
          startTime
          durationTime
          lastFrameImage
          temporaryDirectory
          window
    redrawDrawingAreas :: IO ()
    redrawDrawingAreas = do
      GI.Gtk.widgetQueueDraw firstFramePreviewImageDrawingArea
      GI.Gtk.widgetQueueDraw lastFramePreviewImageDrawingArea

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
    } <- readIORef inVideoPropertiesRef
  cropModeEnabled <- GI.Gtk.getToggleButtonActive cropToggleButton
  drawingAreaWidth <-
    int32ToDouble <$> GI.Gtk.widgetGetAllocatedWidth drawingArea
  drawingAreaHeight <-
    int32ToDouble <$> GI.Gtk.widgetGetAllocatedHeight drawingArea
  left   <- (/ 100.0) <$> GI.Gtk.spinButtonGetValue leftCropSpinButton
  right  <- (/ 100.0) <$> GI.Gtk.spinButtonGetValue rightCropSpinButton
  top    <- (/ 100.0) <$> GI.Gtk.spinButtonGetValue topCropSpinButton
  bottom <- (/ 100.0) <$> GI.Gtk.spinButtonGetValue bottomCropSpinButton
  when (cropModeEnabled && inVideoWidth >= 0.0 && inVideoHeight >= 0.0) $
    GiCairoCairoBridge.renderWithContext context $ do
      GRC.setSourceRGBA 0.0 0.0 0.0 0.8
      GRC.setLineWidth 1.0
      -- Left
      GRC.rectangle 0.0 0.0 (left * drawingAreaWidth) drawingAreaHeight
      GRC.fill
      -- Right
      GRC.rectangle
        (drawingAreaWidth - right * drawingAreaWidth)
        0.0
        (right * drawingAreaWidth)
        drawingAreaHeight
      GRC.fill
      -- Top
      GRC.rectangle 0.0 0.0 drawingAreaWidth (top * drawingAreaHeight)
      GRC.fill
      -- Bottom
      GRC.rectangle
        0.0
        (drawingAreaHeight - bottom * drawingAreaHeight)
        drawingAreaWidth
        (bottom * drawingAreaHeight)
      GRC.fill
  return False

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
    900
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
  ->  GI.Gtk.Image
  ->  GI.Gtk.Image
  ->  System.FilePath.FilePath
  ->  GI.Gtk.Window
  ->  IO ()
makeFirstAndLastFramePreview
  inFilePath
  startTime
  durationTime
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
              firstFrameImage
              ""
              window
  makeLastFramePreview
    inFilePath
    startTime
    durationTime
    lastFrameImage
    temporaryDirectory
    window

makeLastFramePreview
  ::  String
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
              lastFrameImage
              ""
              window

setOrResetFramePrevew
  ::  Bool
  ->  String
  ->  String
  ->  Float
  ->  GI.Gtk.Image
  ->  String
  ->  GI.Gtk.Window
  ->  IO ()
setOrResetFramePrevew False _ _ _ image _ window =
  GtkMainSyncAsync.gtkMainAsync $ do
    resetImage image
    resizeWindow window
setOrResetFramePrevew
  True
  inFilePath
  outFilePath
  time
  image
  overlay
  window
  = do
  result <- makeImagePreview inFilePath outFilePath time overlay
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
  ->  String
  ->  IO (Either IOError String)
makeImagePreview inputFile outputFile startTime bottomText =
  Gifcurry.gif $
    Gifcurry.defaultGifParams
      { Gifcurry.inputFile      = inputFile
      , Gifcurry.outputFile     = outputFile
      , Gifcurry.saveAsVideo    = False
      , Gifcurry.startTime      = startTime
      , Gifcurry.durationTime   = 0.001
      , Gifcurry.widthSize      = 300
      , Gifcurry.qualityPercent = 50.0
      , Gifcurry.bottomText     = bottomText
      }

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

desiredPreviewSize :: Float
desiredPreviewSize = 600.0

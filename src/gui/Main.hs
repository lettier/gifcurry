{-
  Gifcurry
  (C) 2016 David Lettier
  lettier.com
-}

{-# LANGUAGE OverloadedStrings #-}

import System.Directory
import System.Process
import System.FilePath
import System.Info
import System.IO.Temp
import Text.Read
import Data.Int
import Data.Maybe
import Data.Either
import Data.Char
import Data.Text
import Data.List
import Control.Monad
import Control.Concurrent
import Data.GI.Base
import GI.GObject
import qualified GI.Gtk

import Paths_Gifcurry
import qualified Gifcurry (
      gif
    , GifParams(..)
    , defaultGifParams
    , gifParamsValid
    , getVideoDurationInSeconds
    , findOrCreateTemporaryDirectory
  )
import qualified GtkMainSyncAsync (gtkMainSync, gtkMainAsync)

main :: IO ()
main = do
  _ <- GI.Gtk.init Nothing

  builder <- buildBuilder

  window                  <- builderGetObject GI.Gtk.Window            builder "gifcurry-window"
  startTimeEntry          <- builderGetObject GI.Gtk.Entry             builder "start-time-text-entry"
  durationTimeEntry       <- builderGetObject GI.Gtk.Entry             builder "duration-text-entry"
  widthEntry              <- builderGetObject GI.Gtk.Entry             builder "width-text-entry"
  qualityEntry            <- builderGetObject GI.Gtk.Entry             builder "quality-text-entry"
  topTextEntry            <- builderGetObject GI.Gtk.Entry             builder "top-text-text-entry"
  bottomTextEntry         <- builderGetObject GI.Gtk.Entry             builder "bottom-text-text-entry"
  outputFileNameEntry     <- builderGetObject GI.Gtk.Entry             builder "output-file-name-text-entry"
  statusEntry             <- builderGetObject GI.Gtk.Entry             builder "status-text-entry"
  inputFileButton         <- builderGetObject GI.Gtk.FileChooserButton builder "input-file-button"
  outputFilePathButton    <- builderGetObject GI.Gtk.FileChooserButton builder "output-file-path-button"
  fontChooserButton       <- builderGetObject GI.Gtk.FontButton        builder "font-chooser-button"
  createButton            <- builderGetObject GI.Gtk.Button            builder "create-button"
  openButton              <- builderGetObject GI.Gtk.Button            builder "open-button"
  giphyButton             <- builderGetObject GI.Gtk.Button            builder "giphy-link-button"
  imgurButton             <- builderGetObject GI.Gtk.Button            builder "imgur-link-button"
  firstFrameImage         <- builderGetObject GI.Gtk.Image             builder "first-frame-image"
  lastFrameImage          <- builderGetObject GI.Gtk.Image             builder "last-frame-image"
  longGifGtkMessageDialog <- builderGetObject GI.Gtk.MessageDialog     builder "long-gif-message-dialog"
  yesGtkButton            <- builderGetObject GI.Gtk.Button            builder "yes-button"
  noGtkButton             <- builderGetObject GI.Gtk.Button            builder "no-button"
  aboutButton             <- builderGetObject GI.Gtk.Button            builder "about-button"
  aboutDialog             <- builderGetObject GI.Gtk.AboutDialog       builder "about-dialog"

  -- Bug in Glade does not allow setting the link button label.
  GI.Gtk.buttonSetLabel giphyButton "Giphy"
  GI.Gtk.buttonSetLabel imgurButton "Imgur"

  GI.Gtk.entrySetText qualityEntry "100"
  GI.Gtk.entrySetText widthEntry   "500"

  -- Glade does not allow us to use the response ID nicknames so we setup them up programmatically here.
  GI.Gtk.dialogAddActionWidget longGifGtkMessageDialog yesGtkButton (enumToInt32 GI.Gtk.ResponseTypeYes)
  GI.Gtk.dialogAddActionWidget longGifGtkMessageDialog noGtkButton  (enumToInt32 GI.Gtk.ResponseTypeNo)

  temporaryDirectory <- Gifcurry.findOrCreateTemporaryDirectory

  _ <- GI.Gtk.onFileChooserButtonFileSet inputFileButton $ do
    setStatusEntryReady statusEntry
    GI.Gtk.imageSetFromIconName firstFrameImage
      (Just $ pack blankPreviewIcon) (enumToInt32 GI.Gtk.IconSizeButton)
    GI.Gtk.imageSetFromIconName lastFrameImage
      (Just $ pack blankPreviewIcon) (enumToInt32 GI.Gtk.IconSizeButton)
    inputFilePathName <- inputFileButtonGetText inputFileButton
    maybeDuration <- Gifcurry.getVideoDurationInSeconds (
        Gifcurry.defaultGifParams { Gifcurry.inputFile = inputFilePathName }
      )
    let duration = fromMaybe 0.5 maybeDuration - 0.5
    let durationText = if duration == 0.0 then "" else pack (show duration)
    let startText = if duration == 0.0 then "" else "0.0"
    _ <- GI.Gtk.entrySetText startTimeEntry startText
    _ <- GI.Gtk.entrySetText durationTimeEntry durationText
    unless (Data.Text.null startText || Data.Text.null durationText) (
        makeFirstFramePreview
          inputFileButton
          startTimeEntry
          durationTimeEntry
          firstFrameImage
          lastFrameImage
          temporaryDirectory
      )

  _ <- GI.Gtk.onEditableChanged startTimeEntry $
    makeFirstFramePreview
      inputFileButton
      startTimeEntry
      durationTimeEntry
      firstFrameImage
      lastFrameImage
      temporaryDirectory

  _ <- GI.Gtk.onEditableChanged durationTimeEntry $
    makeLastFramePreview
      inputFileButton
      startTimeEntry
      durationTimeEntry
      lastFrameImage
      temporaryDirectory

  _ <- GI.Gtk.onWidgetButtonReleaseEvent createButton $ \ _ -> do
    inputFilePathName <- inputFileButtonGetText inputFileButton
    startTime         <- entryGetFloat startTimeEntry (-1.0)
    durationTime      <- entryGetFloat durationTimeEntry (-1.0)
    widthSize'        <- entryGetFloat widthEntry 0.0
    let widthSize     = truncate widthSize'
    qualityPercent    <- entryGetFloat qualityEntry (-1.0)
    fontChoice        <- GI.Gtk.fontButtonGetFontName fontChooserButton
    topText           <- GI.Gtk.entryGetText topTextEntry
    bottomText        <- GI.Gtk.entryGetText bottomTextEntry
    (_, outputFilePathName) <- assembleOutputFilePathName outputFilePathButton outputFileNameEntry
    let params = Gifcurry.defaultGifParams {
        Gifcurry.inputFile      = inputFilePathName
      , Gifcurry.outputFile     = outputFilePathName
      , Gifcurry.startTime      = startTime
      , Gifcurry.durationTime   = durationTime
      , Gifcurry.widthSize      = widthSize
      , Gifcurry.qualityPercent = qualityPercent
      , Gifcurry.fontChoice     = unpack fontChoice
      , Gifcurry.topText        = unpack topText
      , Gifcurry.bottomText     = unpack bottomText
    }
    paramsValid <- Gifcurry.gifParamsValid params
    setStatusEntryReady statusEntry
    if paramsValid
      then do
        longGifGtkMessageDialogResponse <- if durationTime >= durationTimeWarningLevel
                                              then GI.Gtk.dialogRun longGifGtkMessageDialog
                                              else return (enumToInt32 GI.Gtk.ResponseTypeYes)
        when (longGifGtkMessageDialogResponse == enumToInt32 GI.Gtk.ResponseTypeYes) $
          void $ forkOS $ do
            GtkMainSyncAsync.gtkMainAsync $ GI.Gtk.entrySetText statusEntry "One GIF coming up!"
            success <- (ioSuccess . Gifcurry.gif) params
            if not success
              then GtkMainSyncAsync.gtkMainAsync $ GI.Gtk.entrySetText statusEntry "Did not work. Check your settings."
              else do
                _ <- forkOS $ GtkMainSyncAsync.gtkMainAsync $ openGifCommand outputFilePathName
                GtkMainSyncAsync.gtkMainAsync $ setStatusEntryReady statusEntry
      else GI.Gtk.entrySetText statusEntry "Settings are wrong."
    return True

  _ <- GI.Gtk.onWidgetButtonReleaseEvent openButton $ \ _ -> do
    (_, outputFilePathName) <- assembleOutputFilePathName outputFilePathButton outputFileNameEntry
    fileExists <- doesFileExist outputFilePathName
    if fileExists
      then void $ forkIO $ GtkMainSyncAsync.gtkMainAsync $ openGifCommand outputFilePathName
      else GI.Gtk.entrySetText statusEntry "GIF does not exist. Check your settings."
    return True

  _ <- GI.Gtk.onWidgetButtonReleaseEvent aboutButton $ \ _ -> GI.Gtk.dialogRun aboutDialog >> return True

  _ <- GI.Gtk.onDialogResponse longGifGtkMessageDialog (\ _ -> GI.Gtk.widgetHide longGifGtkMessageDialog)
  _ <- GI.Gtk.onDialogResponse aboutDialog             (\ _ -> GI.Gtk.widgetHide aboutDialog)

  _ <- GI.Gtk.onWidgetDestroy window GI.Gtk.mainQuit

  GI.Gtk.widgetShowAll window
  GI.Gtk.main

buildBuilder :: IO GI.Gtk.Builder
buildBuilder = do
  gladeFile <- getDataFileName "data/gui.glade"
  GI.Gtk.builderNewFromFile (pack gladeFile)

builderGetObject ::
  (GI.GObject.GObject b, GI.Gtk.IsBuilder a) =>
  (Data.GI.Base.ManagedPtr b -> b) ->
  a ->
  Prelude.String ->
  IO b
builderGetObject objectTypeClass builder objectId =
  fromJust <$> GI.Gtk.builderGetObject builder (pack objectId) >>=
  GI.Gtk.unsafeCastTo objectTypeClass

inputFileButtonGetText :: GI.Gtk.FileChooserButton -> IO String
inputFileButtonGetText inputFileButton = do
  inputFilePathName <- fileChooserButtonGetText inputFileButton
  fileExist <- doesFileExist inputFilePathName
  if fileExist then return inputFilePathName else return ""

durationTimeWarningLevel :: Float
durationTimeWarningLevel = 10.0

entryGetFloat :: GI.Gtk.Entry -> Float -> IO Float
entryGetFloat e nothing = fmap (\ t -> fromMaybe nothing (readMaybe (unpack t) :: Maybe Float)) (GI.Gtk.entryGetText e)

assembleOutputFilePathName :: GI.Gtk.FileChooserButton -> GI.Gtk.Entry -> IO (String, String)
assembleOutputFilePathName outputFilePathButton outputFileNameEntry = do
  outputFilePath <- fileChooserButtonGetText outputFilePathButton
  outputFileName <- unpack . Data.Text.strip <$> GI.Gtk.entryGetText outputFileNameEntry
  let outputGifFileName = outputFileName ++ ".gif"
  let outputFilePathName =
        if (not . Data.List.null) outputFilePath
          then outputFilePath ++ [System.FilePath.pathSeparator] ++ outputGifFileName
          else outputGifFileName
  return (outputGifFileName, outputFilePathName)

fileChooserButtonGetText :: GI.Gtk.FileChooserButton -> IO String
fileChooserButtonGetText =
  fmap (Data.Text.unpack . Data.Text.strip . Data.Text.pack . fromMaybe "") . GI.Gtk.fileChooserGetFilename

openGifCommand :: String -> IO ()
openGifCommand outputFilePathName = do
  fileExists <- doesFileExist outputFilePathName
  when fileExists $ void (spawnCommand (command ++ "\"" ++ outputFilePathName ++ "\""))
  where
    command :: String
    command = if "linux" `Data.List.isInfixOf` fmap Data.Char.toLower System.Info.os
                then "xdg-open "
                else "open "

blankPreviewIcon :: String
blankPreviewIcon = "gtk-discard"

resetImage :: GI.Gtk.Image -> IO ()
resetImage image =
  GI.Gtk.imageSetFromIconName
    image
    (Just $ pack blankPreviewIcon)
    (enumToInt32 GI.Gtk.IconSizeButton)

makeGifPreview :: String -> String -> Float -> String -> IO (Either IOError String)
makeGifPreview inputFile outputFile startTime bottomText = Gifcurry.gif $ Gifcurry.defaultGifParams {
      Gifcurry.inputFile      = inputFile
    , Gifcurry.outputFile     = outputFile
    , Gifcurry.startTime      = startTime
    , Gifcurry.durationTime   = 0.001
    , Gifcurry.widthSize      = 200
    , Gifcurry.qualityPercent = 50.0
    , Gifcurry.bottomText     = bottomText
  }

ioSuccess :: IO (Either IOError String) -> IO Bool
ioSuccess = fmap isRight

framePreviewDirectoryName :: String
framePreviewDirectoryName = "gifcurry-frame-previews"

makeLastFramePreview ::
  GI.Gtk.FileChooserButton ->
  GI.Gtk.Entry ->
  GI.Gtk.Entry ->
  GI.Gtk.Image ->
  System.FilePath.FilePath ->
  IO ()
makeLastFramePreview
  inputFileButton
  startTimeEntry
  durationTimeEntry
  lastFrameImage
  temporaryDirectory
  =
  void $ forkIO $
    withTempDirectory temporaryDirectory framePreviewDirectoryName $ \ tmpdir -> do
      inputFilePathName           <- inputFileButtonGetText inputFileButton
      startTimeText               <- GI.Gtk.entryGetText startTimeEntry
      durationTimeText            <- GI.Gtk.entryGetText durationTimeEntry
      startTime'                  <- entryGetFloat startTimeEntry (-1.0)
      durationTime                <- entryGetFloat durationTimeEntry 0.001
      let startTimeTextValid      = (not . Data.List.null) (unpack startTimeText)
      let durationTimeTextValid   = (not . Data.List.null) (unpack durationTimeText)
      let startTime               = startTime' + durationTime
      let outputFilePathName      = tmpdir ++ "/gifcurry-last-frame-preview.gif"
      let doSet                   = startTimeTextValid &&
                                    durationTimeTextValid &&
                                    not (Data.List.null inputFilePathName) &&
                                    (startTime > 0.0)
      _ <- setOrResetFramePrevew doSet inputFilePathName outputFilePathName startTime lastFrameImage " LAST  FRAME "
      return()

makeFirstFramePreview ::
  GI.Gtk.FileChooserButton ->
  GI.Gtk.Entry ->
  GI.Gtk.Entry ->
  GI.Gtk.Image ->
  GI.Gtk.Image ->
  System.FilePath.FilePath ->
  IO ()
makeFirstFramePreview
  inputFileButton
  startTimeEntry
  durationTimeEntry
  firstFrameImage
  lastFrameImage
  temporaryDirectory
  =
  void $ forkIO $ do
    withTempDirectory temporaryDirectory framePreviewDirectoryName $ \ tmpDir -> do
      inputFilePathName       <- inputFileButtonGetText inputFileButton
      startTime               <- entryGetFloat startTimeEntry (-1.0)
      let outputFilePathName  = tmpDir ++ "/gifcurry-first-frame-preview.gif"
      let doSet               = not (Data.List.null inputFilePathName) && (startTime >= 0.0)
      _ <- setOrResetFramePrevew doSet inputFilePathName outputFilePathName startTime firstFrameImage " FIRST FRAME "
      return ()
    makeLastFramePreview
      inputFileButton
      startTimeEntry
      durationTimeEntry
      lastFrameImage
      temporaryDirectory

setOrResetFramePrevew :: Bool -> String -> String -> Float -> GI.Gtk.Image -> String -> IO ()
setOrResetFramePrevew False _ _ _ image _ = GtkMainSyncAsync.gtkMainAsync $ resetImage image
setOrResetFramePrevew True inputFilePathName outputFilePathName time image overlay = do
  success <- ioSuccess (makeGifPreview inputFilePathName outputFilePathName time overlay)
  _ <- updatePreviewFrame outputFilePathName image success
  return()

updatePreviewFrame :: String -> GI.Gtk.Image -> Bool -> IO ()
updatePreviewFrame filePathName image True  = GtkMainSyncAsync.gtkMainSync (GI.Gtk.imageSetFromFile image (Just filePathName))
updatePreviewFrame _            image False = GtkMainSyncAsync.gtkMainSync (resetImage image)

setStatusEntryReady :: GI.Gtk.Entry -> IO ()
setStatusEntryReady = flip GI.Gtk.entrySetText "Ready."

enumToInt32 :: (Enum a, Ord a) => a -> Int32
enumToInt32 enum = fromIntegral (fromEnum enum) :: Int32

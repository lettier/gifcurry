-- David Lettier (C) 2016. http://www.lettier.com/

import System.Directory
import System.Process
import System.Info
import System.IO.Temp
import Text.Read
import Data.Maybe
import Data.Either
import Data.Char
import Data.List
import Control.Monad
import Control.Concurrent
import Graphics.UI.Gtk

import Paths_Gifcurry
import qualified Gifcurry (gif, GifParams(..), defaultGifParams, gifParamsValid)

main :: IO ()
main = do
  _ <- initGUI

  builder <- buildBuilder

  window               <- loadWindow   builder "gifcurry_window"
  startTimeEntry       <- loadEntry    builder "start_time_text_entry"
  durationTimeEntry    <- loadEntry    builder "duration_text_entry"
  widthEntry           <- loadEntry    builder "width_text_entry"
  qualityEntry         <- loadEntry    builder "quality_text_entry"
  topTextEntry         <- loadEntry    builder "top_text_text_entry"
  bottomTextEntry      <- loadEntry    builder "bottom_text_text_entry"
  outputFileNameEntry  <- loadEntry    builder "output_file_name_text_entry"
  statusEntry          <- loadEntry    builder "status_text_entry"
  inputFileButton      <- loadFileChooserButton builder "input_file_button"
  outputFilePathButton <- loadFileChooserButton builder "output_file_path_button"
  fontChooserButton    <- loadFontChooserButton builder "font_chooser_button"
  createButton         <- loadButton   builder "create_button"
  openButton           <- loadButton   builder "open_button"
  giphyButton          <- loadButton   builder "giphy_link_button"
  imgurButton          <- loadButton   builder "imgur_link_button"
  firstFrameImage      <- loadImage    builder "first_frame_image"
  lastFrameImage       <- loadImage    builder "last_frame_image"

  -- Bug in Glade does not allow setting the link button label.
  buttonSetLabel giphyButton "Giphy"
  buttonSetLabel imgurButton "Imgur"

  entrySetText qualityEntry "100"
  entrySetText widthEntry   "500"

  _ <- inputFileButton `on` fileChooserButtonFileSet $ do
    imageSetFromIconName firstFrameImage blankPreviewIcon IconSizeButton
    imageSetFromIconName lastFrameImage  blankPreviewIcon IconSizeButton

  _ <- startTimeEntry `on` editableChanged $
    makeFirstFramePreview inputFileButton startTimeEntry durationTimeEntry firstFrameImage lastFrameImage

  _ <- durationTimeEntry `on` editableChanged $
    makeLastFramePreview inputFileButton startTimeEntry durationTimeEntry lastFrameImage

  _ <- createButton `on` buttonActivated $ do
    inputFilePathName <- inputFileButtonGetText inputFileButton
    startTime <- entryGetFloat startTimeEntry (-1.0)
    durationTime <- entryGetFloat durationTimeEntry (-1.0)
    widthSize' <- entryGetFloat widthEntry 0.0
    let widthSize = truncate widthSize'
    qualityPercent <- entryGetFloat qualityEntry (-1.0)
    fontChoice <- fontButtonGetFontName fontChooserButton
    topText <- entryGetText topTextEntry
    bottomText <- entryGetText bottomTextEntry
    (_, outputFilePathName) <- assembleOutputFilePathName outputFilePathButton outputFileNameEntry
    let params = Gifcurry.defaultGifParams {
        Gifcurry.inputFile = inputFilePathName
      , Gifcurry.outputFile = outputFilePathName
      , Gifcurry.startTime = startTime
      , Gifcurry.durationTime = durationTime
      , Gifcurry.widthSize = widthSize
      , Gifcurry.qualityPercent = qualityPercent
      , Gifcurry.fontChoice = fontChoice
      , Gifcurry.topText = topText
      , Gifcurry.bottomText = bottomText
    }
    paramsValid <- Gifcurry.gifParamsValid params
    if paramsValid
      then void $ forkOS $ do
        postGUIAsync $ entrySetText statusEntry "One GIF coming up!"
        success <- (ioSuccess . Gifcurry.gif) params
        if not success
          then postGUIAsync $ entrySetText statusEntry "Did not work. Check your settings."
          else do
            _ <- forkOS $ postGUIAsync $ openGifCommand outputFilePathName
            postGUIAsync $ entrySetText statusEntry "Ready."
      else entrySetText statusEntry "Settings are wrong."

  _ <- openButton `on` buttonActivated $ do
    (_, outputFilePathName) <- assembleOutputFilePathName outputFilePathButton outputFileNameEntry
    fileExists <- doesFileExist outputFilePathName
    if fileExists
      then void $ forkIO $ postGUIAsync $ openGifCommand outputFilePathName
      else entrySetText statusEntry "GIF does not exist. Check your settings."

  _ <- on window objectDestroy mainQuit

  widgetShowAll window
  mainGUI

loadWindow :: Builder -> (String -> IO Window)
loadWindow b = builderGetObject b castToWindow

loadEntry :: Builder -> (String -> IO Entry)
loadEntry b = builderGetObject b castToEntry

loadFileChooserButton :: Builder -> (String -> IO FileChooserButton)
loadFileChooserButton b = builderGetObject b castToFileChooserButton

loadFontChooserButton :: Builder -> (String -> IO FontButton)
loadFontChooserButton b = builderGetObject b castToFontButton

loadButton :: Builder -> (String -> IO Button)
loadButton b = builderGetObject b castToButton

loadImage :: Builder -> (String -> IO Image)
loadImage b = builderGetObject b castToImage

buildBuilder :: IO Builder
buildBuilder = do
  builder <- builderNew
  gladeFile <- getDataFileName "data/gui.glade"
  builderAddFromFile builder gladeFile
  return builder

inputFileButtonGetText :: FileChooserButton -> IO String
inputFileButtonGetText inputFileButton = do
  inputFileButtonText <- fileChooserGetFilename inputFileButton
  inputFilePathName <- case inputFileButtonText of
    Nothing -> return ""
    Just inputFilePathName -> return inputFilePathName
  fileExist <- doesFileExist inputFilePathName
  if fileExist then return inputFilePathName else return ""

entryGetFloat :: Entry -> Float -> IO Float
entryGetFloat e nothing = fmap (\ t -> fromMaybe nothing (readMaybe t :: Maybe Float)) (entryGetText e)

assembleOutputFilePathName :: FileChooserButton -> Entry -> IO (String, String)
assembleOutputFilePathName outputFilePathButton outputFileNameEntry = do
  outputFilePathText <- fileChooserGetFilename outputFilePathButton
  outputFilePath <- case outputFilePathText of
    Nothing -> return ""
    Just dir -> return dir
  outputFileName <- entryGetText outputFileNameEntry
  let outputGifFileName = outputFileName ++ ".gif"
  let outputFilePathName = outputFilePath ++ "/" ++ outputGifFileName
  return (outputGifFileName, outputFilePathName)

openGifCommand :: String -> IO ()
openGifCommand outputFilePathName = do
  fileExists <- doesFileExist outputFilePathName
  when fileExists $ void (spawnCommand (command ++ outputFilePathName))
  where
    command :: String
    command = if "linux" `isInfixOf` fmap toLower System.Info.os then "xdg-open " else "open "

blankPreviewIcon :: String
blankPreviewIcon = "gtk-discard"

resetImage :: Image -> IO ()
resetImage image = imageSetFromIconName image blankPreviewIcon IconSizeButton

makeGifPreview :: String -> String -> Float -> String -> String -> IO (Either IOError String)
makeGifPreview inputFile outputFile startTime fontChoice bottomText = Gifcurry.gif $ Gifcurry.defaultGifParams {
      Gifcurry.inputFile = inputFile
    , Gifcurry.outputFile = outputFile
    , Gifcurry.startTime = startTime
    , Gifcurry.durationTime = 0.001
    , Gifcurry.widthSize = 200
    , Gifcurry.qualityPercent = 50.0
    , Gifcurry.fontChoice = fontChoice
    , Gifcurry.bottomText = bottomText
  }

ioSuccess :: IO (Either IOError String) -> IO Bool
ioSuccess = fmap isRight

makeLastFramePreview :: FileChooserButton -> Entry -> Entry -> Image -> IO ()
makeLastFramePreview inputFileButton startTimeEntry durationTimeEntry lastFrameImage = do
  _ <- forkIO $
    withTempDirectory "." "previews" $ \tmpdir -> do
      inputFilePathName <- inputFileButtonGetText inputFileButton
      startTime <- entryGetFloat startTimeEntry (-1.0)
      durationTime <- entryGetFloat durationTimeEntry 0.001
      let startTime' = startTime + durationTime
      if not (null inputFilePathName) && (startTime' > 0.0) then do
        let outputFilePathName = tmpdir ++ "/end.gif"
        success <- ioSuccess $ makeGifPreview inputFilePathName outputFilePathName startTime' "default" " LAST FRAME  "
        if success
          then postGUISync $ imageSetFromFile lastFrameImage outputFilePathName
          else postGUISync $ resetImage lastFrameImage
      else postGUIAsync $ resetImage lastFrameImage
  return ()

makeFirstFramePreview :: FileChooserButton -> Entry -> Entry -> Image -> Image -> IO ()
makeFirstFramePreview inputFileButton startTimeEntry durationTimeEntry firstFrameImage lastFrameImage = do
  _ <- forkIO $ do
    withTempDirectory "." "previews" $ \tmpDir -> do
      inputFilePathName <- inputFileButtonGetText inputFileButton
      startTime <- entryGetFloat startTimeEntry (-1.0)
      if not (null inputFilePathName) && (startTime >= 0.0) then do
        let outputFilePathName = tmpDir ++ "/start.gif"
        success <- ioSuccess $ makeGifPreview inputFilePathName outputFilePathName startTime "default" " FIRST FRAME "
        if success
          then postGUISync $ imageSetFromFile firstFrameImage outputFilePathName
          else postGUISync $ resetImage firstFrameImage
      else postGUISync $ resetImage firstFrameImage
    makeLastFramePreview inputFileButton startTimeEntry durationTimeEntry lastFrameImage
  return ()

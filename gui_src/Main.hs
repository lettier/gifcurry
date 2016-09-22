-- David Lettier (C) 2016. http://www.lettier.com/

import System.Environment
import System.Directory
import System.Process
import System.Info
import System.IO.Temp
import Text.Read
import Data.Char
import Data.List
import Control.Monad
import Control.Concurrent
import Control.Exception
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Builder

import Paths_Gifcurry
import Gifcurry (gif, GifParams(..), defaultGifParams, gifParamsValid)

main :: IO ()
main = do
  initGUI

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
  inputFileButton      <- loadFcButton builder "input_file_button"
  outputFilePathButton <- loadFcButton builder "output_file_path_button"
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
  entrySetText widthEntry "500"

  inputFileButton `on` fileChooserButtonFileSet $ do
    imageSetFromIconName firstFrameImage "gtk-missing-image" IconSizeButton
    imageSetFromIconName lastFrameImage "gtk-missing-image" IconSizeButton

  startTimeEntry `on` editableChanged $
    makeFirstFramePreview inputFileButton startTimeEntry durationTimeEntry firstFrameImage lastFrameImage

  durationTimeEntry `on` editableChanged $
    makeLastFramePreview inputFileButton startTimeEntry durationTimeEntry lastFrameImage

  createButton `on` buttonActivated $ do
    inputFilePathName <- inputFileButtonGetText inputFileButton
    startTime <- entryGetFloat startTimeEntry (-1.0)
    durationTime <- entryGetFloat durationTimeEntry (-1.0)
    widthSize' <- entryGetFloat widthEntry 0.0
    let widthSize = truncate widthSize'
    qualityPercent <- entryGetFloat qualityEntry (-1.0)
    topText <- entryGetText topTextEntry
    bottomText <- entryGetText bottomTextEntry
    (outputGifFileName, outputFilePathName) <- assembleOutputFilePathName outputFilePathButton outputFileNameEntry
    let params = defaultGifParams {
        inputFile = inputFilePathName
      , outputFile = outputFilePathName
      , startTime = startTime
      , durationTime = durationTime
      , widthSize = widthSize
      , qualityPercent = qualityPercent
      , topText = topText
      , bottomText = bottomText
    }
    paramsValid <- gifParamsValid params

    if paramsValid
      then do
        forkOS $ do
          postGUIAsync $ entrySetText statusEntry "One GIF coming up!"
          success <- (ioSuccess . gif) params
          if not success
            then postGUIAsync $ entrySetText statusEntry "Did not work. Check your settings."
            else do
              forkOS $ openGifCommand outputFilePathName
              postGUIAsync $ entrySetText statusEntry "Ready."
        return ()
      else entrySetText statusEntry "Settings are wrong."
    return ()

  openButton `on` buttonActivated $ do
    (_, outputFilePathName) <- assembleOutputFilePathName outputFilePathButton outputFileNameEntry
    fileExists <- doesFileExist outputFilePathName
    if fileExists
      then do
        forkIO $ openGifCommand outputFilePathName
        return ()
      else entrySetText statusEntry "GIF does not exist. Check your settings."
    return ()

  on window objectDestroy mainQuit
  widgetShowAll window
  mainGUI

loadWindow :: Builder -> (String -> IO Window)
loadWindow b = builderGetObject b castToWindow

loadEntry :: Builder -> (String -> IO Entry)
loadEntry b = builderGetObject b castToEntry

loadFcButton :: Builder -> (String -> IO FileChooserButton)
loadFcButton b = builderGetObject b castToFileChooserButton

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
entryGetFloat e nothing = do
  text <- entryGetText e
  case readMaybe text :: Maybe Float of
    Nothing -> return nothing
    Just x -> return x

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
  when fileExists $ do
    spawnCommand $ command ++ outputFilePathName
    return ()
  where command = if "linux" `isInfixOf` fmap toLower System.Info.os then "xdg-open " else "open "

resetImage :: Image -> IO ()
resetImage image = imageSetFromIconName image "gtk-missing-image" IconSizeButton

makeGifPreview :: String -> String -> Float -> String -> IO (Either IOError String)
makeGifPreview inputFile outputFile startTime bottomText = gif $ defaultGifParams {
      inputFile = inputFile
    , outputFile = outputFile
    , startTime = startTime
    , durationTime = 0.001
    , widthSize = 200
    , qualityPercent = 50.0
    , bottomText = bottomText
  }

ioSuccess :: IO (Either IOError String) -> IO Bool
ioSuccess r = do
  result <- r
  case result of
    Left err  -> return False
    Right val -> return True

makeLastFramePreview :: FileChooserButton -> Entry -> Entry -> Image -> IO ()
makeLastFramePreview inputFileButton startTimeEntry durationTimeEntry lastFrameImage = do
  forkIO $
    withTempDirectory "." "previews" $ \tmpdir -> do
      inputFilePathName <- inputFileButtonGetText inputFileButton
      startTime <- entryGetFloat startTimeEntry (-1.0)
      durationTime <- entryGetFloat durationTimeEntry 0.001
      let startTime' = startTime + durationTime
      if not (null inputFilePathName) && (startTime' > 0.0) then do
        let outputFilePathName = tmpdir ++ "/end.gif"
        success <- ioSuccess $ makeGifPreview inputFilePathName outputFilePathName startTime' " LAST FRAME  "
        if success
          then postGUISync $ imageSetFromFile lastFrameImage outputFilePathName
          else postGUISync $ resetImage lastFrameImage
      else postGUIAsync $ resetImage lastFrameImage
  return ()

makeFirstFramePreview :: FileChooserButton -> Entry -> Entry -> Image -> Image -> IO ()
makeFirstFramePreview inputFileButton startTimeEntry durationTimeEntry firstFrameImage lastFrameImage = do
  forkIO $ do
    withTempDirectory "." "previews" $ \tmpDir -> do
      inputFilePathName <- inputFileButtonGetText inputFileButton
      startTime <- entryGetFloat startTimeEntry (-1.0)
      if not (null inputFilePathName) && (startTime >= 0.0) then do
        let outputFilePathName = tmpDir ++ "/start.gif"
        success <- ioSuccess $ makeGifPreview inputFilePathName outputFilePathName startTime " FIRST FRAME "
        if success
          then postGUISync $ imageSetFromFile firstFrameImage outputFilePathName
          else postGUISync $ resetImage firstFrameImage
      else postGUISync $ resetImage firstFrameImage
    makeLastFramePreview inputFileButton startTimeEntry durationTimeEntry lastFrameImage
  return ()

-- David Lettier (C) 2016. http://www.lettier.com/

import System.Environment
import System.Directory
import System.Process
import System.Info
import System.IO.Temp
import Control.Monad
import Data.Char
import Data.List
import Control.Concurrent
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Builder

import Paths_Gifcurry
import Gifcurry (gif)

main = do
  initGUI

  builder <- build_builder

  window                  <- load_window builder "gifcurry_window"
  start_time_entry        <- load_entry builder "start_time_text_entry"
  duration_entry          <- load_entry builder "duration_text_entry"
  width_entry             <- load_entry builder "width_text_entry"
  quality_entry           <- load_entry builder "quality_text_entry"
  top_text_entry          <- load_entry builder "top_text_text_entry"
  bottom_text_entry       <- load_entry builder "bottom_text_text_entry"
  output_file_name_entry  <- load_entry builder "output_file_name_text_entry"
  status_entry            <- load_entry builder "status_text_entry"
  input_file_button       <- load_fc_button builder "input_file_button"
  output_file_path_button <- load_fc_button builder "output_file_path_button"
  create_button           <- load_button builder "create_button"
  open_button             <- load_button builder "open_button"
  giphy_button            <- load_button builder "giphy_link_button"
  imgur_button            <- load_button builder "imgur_link_button"
  first_frame_image       <- load_image builder "first_frame_image"
  last_frame_image        <- load_image builder "last_frame_image"

  -- Bug in Glade does not allow setting the link button label.
  buttonSetLabel giphy_button "Giphy"
  buttonSetLabel imgur_button "Imgur"

  entrySetText quality_entry "100"
  entrySetText width_entry "500"

  input_file_button `on` fileChooserButtonFileSet $ do
    imageSetFromIconName first_frame_image "gtk-missing-image" IconSizeButton
    imageSetFromIconName last_frame_image "gtk-missing-image" IconSizeButton

  start_time_entry `on` editableChanged $
    make_first_frame_preview input_file_button start_time_entry duration_entry first_frame_image last_frame_image

  duration_entry `on` editableChanged $
    make_last_frame_preview input_file_button start_time_entry duration_entry last_frame_image

  create_button `on` buttonActivated $ do
    input_file_text <- fileChooserGetFilename input_file_button
    input_file_path_name <- case input_file_text of
      Nothing -> return ""
      Just file_path_name -> return file_path_name

    start_time <- entryGetText start_time_entry
    duration <- entryGetText duration_entry

    width <- entryGetText width_entry
    quality <- entryGetText quality_entry

    top_text <- entryGetText top_text_entry
    bottom_text <- entryGetText bottom_text_entry

    (output_gif_file_name, output_file_path_name) <- assemble_output_file_path_name output_file_path_button output_file_name_entry

    if ((length input_file_path_name) > 0 && (length output_file_path_name) > 5 && (length output_gif_file_name) > 4)
      then do
        forkIO $ do
          entrySetText status_entry "One GIF coming up!"
          result <- gif [
            input_file_path_name,
            output_file_path_name,
            start_time,
            duration,
            width,
            quality,
            top_text,
            bottom_text ]
          if result == 1
            then entrySetText status_entry "Did not work. Check your settings."
            else entrySetText status_entry "Ready."
          forkIO $ do
            open_gif output_file_path_name
            return ()
          return ()
        return ()
      else entrySetText status_entry "File paths are wrong. Check your settings."

    return ()

  open_button `on` buttonActivated $ do
    (_, output_file_path_name) <- assemble_output_file_path_name output_file_path_button output_file_name_entry

    fileExists <- doesFileExist output_file_path_name
    if fileExists
      then do
        forkIO $ do
          open_gif output_file_path_name
          return ()
        return ()
      else entrySetText status_entry "GIF does not exist. Check your settings."

    return ()

  on window objectDestroy mainQuit
  widgetShowAll window
  mainGUI

load_window b = builderGetObject b castToWindow

load_entry b = builderGetObject b castToEntry

load_fc_button b = builderGetObject b castToFileChooserButton

load_button b = builderGetObject b castToButton

load_image b = builderGetObject b castToImage

build_builder = do
  builder <- builderNew
  glade_file <- getDataFileName "data/gui.glade"
  builderAddFromFile builder glade_file

  return builder

assemble_output_file_path_name output_file_path_button output_file_name_entry = do
  output_file_path_text <- fileChooserGetFilename output_file_path_button
  output_file_path <- case output_file_path_text of
    Nothing -> return ""
    Just dir -> return dir

  output_file_name <- entryGetText output_file_name_entry
  let output_gif_file_name = output_file_name ++ ".gif"
  let output_file_path_name = output_file_path ++ "/" ++ output_gif_file_name

  return (output_gif_file_name, output_file_path_name)

open_gif output_file_path_name = spawnCommand $ command ++ output_file_path_name
  where command = if "linux" `isInfixOf` (fmap toLower System.Info.os) then "xdg-open " else "open "

reset_image i = imageSetFromIconName i "gtk-missing-image" IconSizeButton

make_gif_preview ifpn ofpn st btxt = gif [
  ifpn,
  ofpn,
  st,
  "0.001",
  "200",
  "50",
  "",
  btxt ]

make_last_frame_preview ifb ste de lfi = do
  forkIO $
    withTempDirectory "." "previews" $ \tmpDir -> do
      input_file_text <- fileChooserGetFilename ifb
      input_file_path_name <- case input_file_text of
        Nothing -> return ""
        Just file_path_name -> return file_path_name
      start_time_text <- case (return $ entryGetText ste) of
        Nothing -> return ""
        Just start_time_text -> start_time_text
      duration_text <- case (return $ entryGetText de) of
        Nothing -> return ""
        Just duration_text -> duration_text
      if ((not (null input_file_path_name)) && (not (null start_time_text)) && (not (null duration_text))) then do
        let output_file_path_name = tmpDir ++ "/end.gif"
        let start_time_flt = read start_time_text :: Float
        let duration_flt = read duration_text :: Float
        let start_time = show $ start_time_flt + duration_flt
        result <- make_gif_preview input_file_path_name output_file_path_name start_time " LAST FRAME "
        if (result == 0)
          then imageSetFromFile lfi output_file_path_name
        else reset_image lfi
      else reset_image lfi
  return ()

make_first_frame_preview ifb ste de ffi lfi = do
  forkIO $ do
    withTempDirectory "." "previews" $ \tmpDir -> do
      input_file_text <- fileChooserGetFilename ifb
      input_file_path_name <- case input_file_text of
        Nothing -> return ""
        Just file_path_name -> return file_path_name
      start_time <- case (return $ entryGetText ste) of
        Nothing -> return ""
        Just start_time -> start_time
      if ((not (null input_file_path_name)) && (not (null start_time))) then do
        let output_file_path_name = tmpDir ++ "/start.gif"
        result <- make_gif_preview input_file_path_name output_file_path_name start_time " FIRST FRAME "
        if (result == 0)
          then imageSetFromFile ffi output_file_path_name
        else reset_image ffi
      else reset_image ffi
    make_last_frame_preview ifb ste de lfi
  return ()

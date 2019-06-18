{-
  Gifcurry
  (C) 2017 David Lettier
  lettier.com
-}

{-# LANGUAGE
    NamedFieldPuns
  , DuplicateRecordFields
#-}

module GuiKeyboard where

import Control.Monad
import Data.IORef
import Data.Word
import qualified GI.Gdk
import qualified GI.Gtk

import qualified GuiRecords as GR
import qualified GuiPreview
import GuiMisc

addKeyboardEventHandler
  ::  GR.GuiComponents
  ->  IO ()
addKeyboardEventHandler
  guiComponents@GR.GuiComponents
    { GR.window
    }
  =
  void
    $ GI.Gtk.onWidgetKeyPressEvent window
      $ keyboardEventHandler guiComponents

keyboardEventHandler
  ::  GR.GuiComponents
  ->  GI.Gdk.EventKey
  ->  IO Bool
keyboardEventHandler
  guiComponents@GR.GuiComponents
    {
    }
  eventKey
  = do
  keyValue        <- GI.Gdk.getEventKeyKeyval eventKey
  let isSeekLeft  = isSeekLeftKey  keyValue
  let isSeekRight = isSeekRightKey keyValue
  let isSeek      = isSeekLeft || isSeekRight
  when isSeek $ handleSeekKeys guiComponents isSeekLeft
  return False

handleSeekKeys
  ::  GR.GuiComponents
  ->  Bool
  ->  IO ()
handleSeekKeys
  guiComponents@GR.GuiComponents
    { GR.startTimeSpinButton
    , GR.endTimeSpinButton
    , GR.videoPreviewPauseToggleButton
    , GR.guiInFilePropertiesRef
    , GR.maybeVideoPreviewWidget = (Just _)
    , GR.maybePlaybinElement = (Just _)
    }
  isSeekLeft
  = do
  (maybePlaybinDuration, maybePlaybinPosition)
    <- GuiPreview.getPlaybinDurationAndPosition guiComponents
  case (maybePlaybinDuration, maybePlaybinPosition) of
    (Just _, Just playbinPosition) -> do
      void
        $ GI.Gtk.setToggleButtonActive videoPreviewPauseToggleButton True
      startTime <-
            secondsToNanoseconds
        <$> GI.Gtk.spinButtonGetValue startTimeSpinButton
      endTime <-
            secondsToNanoseconds
        <$> GI.Gtk.spinButtonGetValue endTimeSpinButton
      GR.GuiInFileProperties
        { GR.inFileFps
        } <- readIORef guiInFilePropertiesRef
      let fps        = if inFileFps <= 0 then 1 else inFileFps
      let inc        = doubleToInt64 $ (1 / fps) * nanosecondsInASecond
      let seekTo     = if isSeekLeft then playbinPosition - inc else playbinPosition + inc
      let seekTo'
            | seekTo >= endTime   = endTime
            | seekTo <= startTime = startTime
            | otherwise           = seekTo
      GuiPreview.seekPlaybinElement
        guiComponents
        (Just seekTo')
        (Just endTime)
    _ -> return ()
handleSeekKeys _ _ = return ()

isSeekLeftKey
  ::  Word32
  ->  Bool
isSeekLeftKey GI.Gdk.KEY_less = True
isSeekLeftKey _               = False

isSeekRightKey
  ::  Word32
  ->  Bool
isSeekRightKey GI.Gdk.KEY_greater = True
isSeekRightKey _                  = False

{-
  Gifcurry
  (C) 2018 David Lettier
  lettier.com
-}

{-# LANGUAGE OverloadedStrings #-}

module GuiMisc where

import System.Exit
import System.FilePath
import System.Process
import System.Directory
import Control.Exception
import Control.Monad
import Text.Read
import Data.Int
import Data.Word
import Data.Maybe
import Data.Char
import Data.Text
import Data.List
import Data.GI.Base.Overloading
import qualified GI.Gtk
import qualified GI.GLib

import qualified GtkMainSyncAsync (gtkMainAsync)
import qualified GuiStyle

enumToInt32 :: (Enum a, Ord a) => a -> Int32
enumToInt32 = fromIntegral . fromEnum

floatToInt32 :: Float -> Int32
floatToInt32 f = enumToInt32 (round f :: Int)

floatToDouble :: Float -> Double
floatToDouble = realToFrac

doubleToFloat :: Double -> Float
doubleToFloat = realToFrac

doubleToInt :: Double -> Int
doubleToInt = truncate

doubleToInt32 :: Double -> Int32
doubleToInt32 d = enumToInt32 (round d :: Int)

doubleToInt64 :: Double -> Int64
doubleToInt64 d = fromIntegral (round d :: Int)

int32ToDouble :: Int32 -> Double
int32ToDouble = fromIntegral

int32ToFloat :: Int32 -> Float
int32ToFloat = fromIntegral

int32ToInt :: Int32 -> Int
int32ToInt = fromIntegral

intToInt32 :: Int -> Int32
intToInt32 = fromIntegral

int64ToDouble :: Int64 -> Double
int64ToDouble = fromIntegral

entryGetMaybeFloat
  ::  GI.Gtk.Entry
  ->  IO (Maybe Float)
entryGetMaybeFloat entry = do
  text <- Data.Text.strip <$> GI.Gtk.entryGetText entry
  let containsPeriod = isJust $ Data.Text.find (== '.') text
  let string = Data.Text.unpack text
  let maybeFloat = readMaybe (string ++ if containsPeriod then "0" else "") :: Maybe Float
  return maybeFloat

entryGetFloat
  ::  GI.Gtk.Entry
  ->  Float
  ->  IO Float
entryGetFloat entry nothing = do
  maybeFloat <- entryGetMaybeFloat entry
  return $ fromMaybe nothing maybeFloat

entryGetMaybeInt
  ::  GI.Gtk.Entry
  ->  IO (Maybe Int)
entryGetMaybeInt entry = do
  text <- Data.Text.strip <$> GI.Gtk.entryGetText entry
  let string = Data.Text.unpack text
  let maybeInt = readMaybe string :: Maybe Int
  return maybeInt

fileChooserGetFilePath
  ::  ( GI.GLib.GObject a
      , Data.GI.Base.Overloading.IsDescendantOf GI.Gtk.FileChooser a
      )
  =>  a
  ->  IO String
fileChooserGetFilePath =
  fmap
    (Data.Text.unpack . Data.Text.strip . Data.Text.pack . fromMaybe "")
  . GI.Gtk.fileChooserGetFilename

fileChooserGetExistingFilePath
  ::  ( GI.GLib.GObject a
      , Data.GI.Base.Overloading.IsDescendantOf GI.Gtk.FileChooser a
      )
  =>  a
  ->  IO (Maybe String)
fileChooserGetExistingFilePath fileChooser = do
  result    <- fileChooserGetFilePath fileChooser
  fileExist <- doesFileExist result
  return $
    if fileExist
      then Just result
      else Nothing

updateStatusLabelAsync :: GI.Gtk.Label -> Word32 -> Text -> IO ()
updateStatusLabelAsync statusLabel milliseconds message =
  void $ GI.GLib.timeoutAdd
    GI.GLib.PRIORITY_DEFAULT
    milliseconds $ do
      GtkMainSyncAsync.gtkMainAsync
        $ GI.Gtk.labelSetText statusLabel message
      return False

hasFileExtension
  ::  String
  ->  String
  ->  Bool
hasFileExtension
  filePath
  extension
  =
      stringToLower extension
  ==  ( Data.Text.unpack
      . Data.Text.toLower
      . Data.Text.pack
      . takeExtension
      ) filePath

safeDivide
  ::  (Fractional a, Eq a)
  =>  a
  ->  a
  ->  Maybe a
safeDivide n d = if d == 0.0 then Nothing else Just $ n / d

clamp
  ::  (Fractional a, Eq a, Ord a)
  =>  a
  ->  a
  ->  a
  ->  a
clamp min' max' v
  | v <= min'  = min'
  | v >= max'  = max'
  | otherwise  = v

safeRunProcessGetOutput
  ::  String
  ->  [String]
  ->  IO (System.Exit.ExitCode, String, String)
safeRunProcessGetOutput processName args =
  catch readProcess' catchError
  where
    readProcess'
      ::  IO (System.Exit.ExitCode, String, String)
    readProcess' =
      readProcessWithExitCode
        processName
        args
        ""
    catchError
      ::  Control.Exception.IOException
      ->  IO (System.Exit.ExitCode, String, String)
    catchError e = do
      putStrLn $ "[ERROR] " ++ show e
      return (ExitFailure 1, "", "")

stringToLower :: String -> String
stringToLower = Prelude.map Data.Char.toLower

hasText :: Text -> String -> Bool
hasText needle haystack =
  Data.Text.isInfixOf needle $
    Data.Text.toLower $
      Data.Text.pack haystack

listElementsEqual :: Eq a => [a] -> Bool
listElementsEqual (x:xs) = Data.List.all (== x) xs
listElementsEqual []     = False

truncatePastDigit :: RealFrac a => a -> Int -> a
truncatePastDigit frac num = fromIntegral int / trunc
  where
    int :: Int
    int = floor (frac * trunc)
    trunc :: Fractional b => b
    trunc = 10.0^num

nanosecondsInASecond
  :: Num a => a
nanosecondsInASecond = 1000000000

secondsToNanoseconds :: Double -> Int64
secondsToNanoseconds s =
  fromIntegral (round (s * nanosecondsInASecond) :: Integer) :: Int64

nanosecondsToSeconds :: Int64 -> Double
nanosecondsToSeconds s =
  int64ToDouble s * (1.0 / nanosecondsInASecond)

toggleToggleButtonLabel
  ::  GI.Gtk.ToggleButton
  ->  Text
  ->  Text
  ->  Text
  ->  Text
  ->  IO ()
toggleToggleButtonLabel
  toggleButton
  activeLabel
  inactiveLabel
  activeTooltip
  inactiveTooltip
  = do
  active <-
    GI.Gtk.getToggleButtonActive
      toggleButton
  GI.Gtk.setButtonLabel
    toggleButton
    (if active then activeLabel else inactiveLabel)
  GI.Gtk.widgetSetTooltipText
    toggleButton
    (Just $ if active then activeTooltip else inactiveTooltip)
  let toggleClass =
        if active
          then GuiStyle.widgetAddStyleClass
          else GuiStyle.widgetRemoveStyleClass
  toggleClass
    toggleButton
    "gifcurry-font-weight-bold"

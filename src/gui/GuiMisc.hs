{-
  Gifcurry
  (C) 2018 David Lettier
  lettier.com
-}

{-# LANGUAGE OverloadedStrings #-}

module GuiMisc where

import System.Directory
import Text.Read
import Data.Int
import Data.Maybe
import Data.Text
import qualified GI.Gtk

enumToInt32 :: (Enum a, Ord a) => a -> Int32
enumToInt32 enum = fromIntegral (fromEnum enum) :: Int32

floatToInt32 :: Float -> Int32
floatToInt32 f = enumToInt32 (round f :: Int)

int32ToDouble :: Int32 -> Double
int32ToDouble = fromIntegral

int32ToFloat :: Int32 -> Float
int32ToFloat = fromIntegral

entryGetMaybeFloat :: GI.Gtk.Entry -> IO (Maybe Float)
entryGetMaybeFloat entry = do
  text <- Data.Text.strip <$> GI.Gtk.entryGetText entry
  let containsPeriod = isJust $ Data.Text.find (== '.') text
  let string = Data.Text.unpack text
  let maybeFloat = readMaybe (string ++ if containsPeriod then "0" else "") :: Maybe Float
  return maybeFloat

entryGetFloat :: GI.Gtk.Entry -> Float -> IO Float
entryGetFloat entry nothing = do
  maybeFloat <- entryGetMaybeFloat entry
  return $ fromMaybe nothing maybeFloat

entryGetMaybeInt :: GI.Gtk.Entry -> IO (Maybe Int)
entryGetMaybeInt entry = do
  text <- Data.Text.strip <$> GI.Gtk.entryGetText entry
  let string = Data.Text.unpack text
  let maybeInt = readMaybe string :: Maybe Int
  return maybeInt

fileChooserButtonGetString :: GI.Gtk.FileChooserButton -> IO String
fileChooserButtonGetString =
  fmap
      (Data.Text.unpack . Data.Text.strip . Data.Text.pack . fromMaybe "")
    . GI.Gtk.fileChooserGetFilename

fileChooserButtonGetFilePath :: GI.Gtk.FileChooserButton -> IO String
fileChooserButtonGetFilePath fileChooserButton = do
  result <- fileChooserButtonGetString fileChooserButton
  fileExist <- doesFileExist result
  return $
    if fileExist
      then result
      else ""

safeDivide :: (Fractional a, Eq a) => a -> a -> Maybe a
safeDivide n d = if d == 0.0 then Nothing else Just $ n / d

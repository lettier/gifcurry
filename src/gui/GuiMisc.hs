{-
  Gifcurry
  (C) 2018 David Lettier
  lettier.com
-}

{-# LANGUAGE OverloadedStrings #-}

module GuiMisc where

import System.Exit
import System.Process
import System.Directory
import Control.Exception
import Text.Read
import Data.Int
import Data.Maybe
import Data.Char
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

fileChooserGetString :: GI.Gtk.IsFileChooser a => a -> IO String
fileChooserGetString =
  fmap
    (Data.Text.unpack . Data.Text.strip . Data.Text.pack . fromMaybe "")
  . GI.Gtk.fileChooserGetFilename

fileChooserGetFilePath :: GI.Gtk.IsFileChooser a => a -> IO String
fileChooserGetFilePath fileChooser = do
  result <- fileChooserGetString fileChooser
  fileExist <- doesFileExist result
  return $
    if fileExist
      then result
      else ""

safeDivide :: (Fractional a, Eq a) => a -> a -> Maybe a
safeDivide n d = if d == 0.0 then Nothing else Just $ n / d

clamp :: (Fractional a, Eq a, Ord a) => a -> a -> a -> a
clamp min max v
  | v <= min  = min
  | v >= max  = max
  | otherwise = v

safeRunProcessGetOutput :: String -> [String] -> IO (System.Exit.ExitCode, String, String)
safeRunProcessGetOutput processName args =
  catch readProcess' catchError
  where
    readProcess' :: IO (System.Exit.ExitCode, String, String)
    readProcess' =
      readProcessWithExitCode
        processName
        args
        ""
    catchError :: Control.Exception.IOException -> IO (System.Exit.ExitCode, String, String)
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

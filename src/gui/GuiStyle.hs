{-
  Gifcurry
  (C) 2018 David Lettier
  lettier.com
-}

{-# LANGUAGE OverloadedStrings #-}

module GuiStyle where

import Prelude
import Control.Monad
import Data.Word
import Data.Text
import qualified Data.ByteString.Char8
import qualified Data.ByteString
import qualified GI.Gdk
import qualified GI.Gtk

import Paths_Gifcurry
import qualified GuiRecords as GR

cssPriority :: Word32
cssPriority =
  fromIntegral GI.Gtk.STYLE_PROVIDER_PRIORITY_USER :: Word32

applyCss :: GR.GuiComponents -> IO ()
applyCss
  _
  = do
  maybeScreen         <- GI.Gdk.screenGetDefault
  provider            <- GI.Gtk.cssProviderNew
  styleFile           <- getDataFileName "data/style.css"
  styleFile318        <- getDataFileName "data/style-3-18.css"
  styleFile320        <- getDataFileName "data/style-3-20.css"
  isGtkVersionGte310  <- isGtkVersionGte 3 10
  isGtkVersionGte318  <- isGtkVersionGte 3 18
  isGtkVersionGte320  <- isGtkVersionGte 3 20
  case ( maybeScreen
       , isGtkVersionGte310
       , isGtkVersionGte318
       , isGtkVersionGte320
       )
    of
    (Just screen, True, False, False) -> do
      GI.Gtk.cssProviderLoadFromPath provider (Data.Text.pack styleFile)
      GI.Gtk.styleContextAddProviderForScreen
        screen
        provider
        cssPriority
    (Just screen, True, True, False) -> do
      styleFileContents          <- Data.ByteString.readFile styleFile
      styleFileContents318       <- Data.ByteString.readFile styleFile318
      let totalStyleFileContents =
            Data.ByteString.concat
              [styleFileContents, styleFileContents318]
      GI.Gtk.cssProviderLoadFromData provider totalStyleFileContents
      GI.Gtk.styleContextAddProviderForScreen
        screen
        provider
        cssPriority
    (Just screen, True, True, True) -> do
      styleFileContents          <- Data.ByteString.readFile styleFile
      styleFileContents318       <- Data.ByteString.readFile styleFile318
      styleFileContents320       <- Data.ByteString.readFile styleFile320
      let totalStyleFileContents =
            Data.ByteString.concat
              [styleFileContents, styleFileContents318, styleFileContents320]
      GI.Gtk.cssProviderLoadFromData provider totalStyleFileContents
      GI.Gtk.styleContextAddProviderForScreen
        screen
        provider
        cssPriority
    _ -> return ()

styleWidget :: GI.Gtk.IsWidget a => String -> a -> IO ()
styleWidget style widget = do
  provider <- GI.Gtk.cssProviderGetDefault
  GI.Gtk.cssProviderLoadFromData provider $ Data.ByteString.Char8.pack style
  styleContext <- GI.Gtk.widgetGetStyleContext widget
  void $
    GI.Gtk.styleContextAddProvider
      styleContext
      provider
      cssPriority

widgetAddStyleClass :: GI.Gtk.IsWidget a => a -> Text -> IO ()
widgetAddStyleClass widget styleClass = do
  styleContext <- GI.Gtk.widgetGetStyleContext widget
  GI.Gtk.styleContextAddClass styleContext styleClass

isGtkVersionGte :: Word32 -> Word32 -> IO Bool
isGtkVersionGte major minor = do
  major' <- GI.Gtk.getMajorVersion
  minor' <- GI.Gtk.getMinorVersion
  return (major' >= major && minor' >= minor)

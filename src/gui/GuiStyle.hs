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
  case maybeScreen of
    Just screen -> do
      GI.Gtk.cssProviderLoadFromPath provider (Data.Text.pack styleFile)
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

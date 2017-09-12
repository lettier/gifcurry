{-
  Gifcurry
  (C) 2017 David Lettier
  lettier.com

  Based on the following works.

    - https://github.com/gtk2hs/gtk2hs/blob/master/gtk/Graphics/UI/Gtk/General/General.chs#L177
    - https://github.com/gtk2hs/gtk2hs/blob/master/gtk/Graphics/UI/Gtk/General/General.chs#L188

  Modified to work with haskell-gi (https://github.com/haskell-gi).

  Original license:

  GIMP Toolkit (GTK) General

  Author : Axel Simon, Manuel M. T. Chakravarty

  Created: 8 December 1998

  Copyright (C) 2000..2005 Axel Simon, Manuel M. T. Chakravarty

  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 2.1 of the License, or (at your option) any later version.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Lesser General Public License for more details.
-}

{-# LANGUAGE OverloadedStrings #-}

module GtkMainSyncAsync (
      gtkMainSync
    , gtkMainAsync
  ) where

import Control.Monad
import Control.Concurrent
import GI.GLib hiding (String, IOError)

gtkMainSync :: IO a -> IO a
gtkMainSync callback =
  newEmptyMVar >>=
  gtkMainSyncSetupCallBack callback >>=
  takeMVar

gtkMainAsync :: IO () -> IO ()
gtkMainAsync callback = gtkMainAddCallback (callback >> return False)

gtkMainSyncSetupCallBack :: IO a -> MVar a -> IO (MVar a)
gtkMainSyncSetupCallBack callback mVar =
  gtkMainAddCallback (gtkMainSyncBuildCallBack callback mVar) >> return mVar

gtkMainSyncBuildCallBack :: IO a -> MVar a -> IO Bool
gtkMainSyncBuildCallBack callback mVar = callback >>= putMVar mVar >> return False

gtkMainAddCallback :: GI.GLib.SourceFunc -> IO ()
gtkMainAddCallback callback = void $ GI.GLib.idleAdd GI.GLib.PRIORITY_DEFAULT callback

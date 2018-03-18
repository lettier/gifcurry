#!/usr/bin/env bash

# Gifcurry
# (C) 2018 David Lettier
# lettier.com

/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
brew update
xcode-select --install
brew install \
  wget \
  git \
  libffi \
  libsvg \
  librsvg \
  libav \
  libogg \
  libvorbis \
  pkg-config \
  gobject-introspection \
  cairo \
  gdk-pixbuf \
  gsettings-desktop-schemas \
  gtk+3 \
  gtk-mac-integration \
  gnome-icon-theme \
  openh264 \
  theora \
  ffmpeg \
  imagemagick \
  ghostscript \
  gstreamer \
  gst-libav \
  gst-plugins-base \
  gst-plugins-good
brew install --with-gtk+3 gst-plugins-bad
wget -qO- https://get.haskellstack.org/ | sh -s - -f
git clone https://github.com/lettier/gifcurry.git
cd gifcurry/
LIBFFIPKGCONFIG=`find /usr/local/Cellar -path '*libffi*' -type d -name 'pkgconfig' 2>/dev/null | tr '\n' ':' | sed 's/:$//'`
export PKG_CONFIG_PATH=$PKG_CONFIG_PATH:$LIBFFIPKGCONFIG
stack setup
stack install alex happy
stack install gtk2hs-buildtools
stack install hsc2hs
stack install
ln -s $HOME/.local/bin/gifcurry_cli $HOME/Desktop/gifcurry_cli
ln -s $HOME/.local/bin/gifcurry_gui $HOME/Desktop/gifcurry_gui

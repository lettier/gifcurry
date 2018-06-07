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
  libvpx \
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
  imagemagick \
  ghostscript \
  gstreamer
brew install ffmpeg --with-libvpx
brew install gst-plugins-base --with-libogg --with-libvorbis --with-theora
brew install gst-plugins-good --with-gtk+3
brew install gst-plugins-bad --with-gtk+3
brew install gst-libav
mkdir -p $HOME/.magick
cd $HOME/Downloads/
wget http://www.imagemagick.org/Usage/scripts/imagick_type_gen -O imagemagick_type_gen.pl
chmod +x imagemagick_type_gen.pl
$HOME/Downloads/imagemagick_type_gen.pl > $HOME/.magick/type.xml
git clone https://github.com/lettier/gifcurry.git
cd gifcurry/
git pull
git reset --hard origin/master
git pull
LIBFFIPKGCONFIG=`find /usr/local/Cellar -path '*libffi*' -type d -name 'pkgconfig' 2>/dev/null | tr '\n' ':' | sed 's/:$//'`
export PKG_CONFIG_PATH=$PKG_CONFIG_PATH:$LIBFFIPKGCONFIG
wget -qO- https://get.haskellstack.org/ | sh -s - -f
stack setup
stack clean
stack install alex happy
stack install gtk2hs-buildtools
stack install hsc2hs
stack install
rm -f $HOME/Desktop/gifcurry_cli
rm -f $HOME/Desktop/gifcurry_gui
ln -s $HOME/.local/bin/gifcurry_cli $HOME/Desktop/gifcurry_cli
ln -s $HOME/.local/bin/gifcurry_gui $HOME/Desktop/gifcurry_gui

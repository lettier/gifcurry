![Gifcurry](logo.png)

# Gifcurry

## UI

![UI](ui.gif)

## Sample GIF

![Caminandes: Gran Dillama - Blender Foundation](sample.gif)  
Credit: [Caminandes: Gran Dillama - Blender Foundation](http://www.caminandes.com/)

## Description

Create animated GIFs, overlaid with optional text, from video files.

## CLI Usage

```bash
$ gifcurry_cli -i inputFile -o outputFile -s startTime -d durationTime -w widthSize -q qualityPercent -t topText -b bottomText
```

## CLI Example

```Bash
~/gifcurry ❯❯❯ ./gifcurry_cli -i ./02_gran_dillama_1080p.mp4 -o ./out.gif -s 32 -d 8 -w 500 -q 100 -t 'What is' -b 'Gifcurry?'
 _____ _  __                           
|  __ (_)/ _|                          
| |  \/_| |_ ___ _   _ _ __ _ __ _   _ 
| | __| |  _/ __| | | | '__| '__| | | |
| |_\ \ | || (__| |_| | |  | |  | |_| |
 \____/_|_| \___|\__,_|_|  |_|   \__, |
                                  __/ |
                                 |___/ 

Gifcurry (C) 2016 David Lettier. http://www.lettier.com/

Input file: ./02_gran_dillama_1080p.mp4
Output file: ./out.gif
Start second: 32
Duration: 8 seconds
GIF width: 500px
Quality: 100.0%
Top text: What is
Bottom text: Gifcurry?

Writing temporary frames to... ./frames3617
Writing your GIF to... ./out.gif
Done.
```

## Dependencies

* [Haskell](https://www.haskell.org/platform/)
  * [alex](https://hackage.haskell.org/package/alex)
  * [happy](https://hackage.haskell.org/package/happy)
  * [gtk2hs-buildtools](https://hackage.haskell.org/package/gtk2hs-buildtools)
  * [cmdargs](https://hackage.haskell.org/package/cmdargs)
  * [System.IO.Temp (temporary)](https://hackage.haskell.org/package/temporary)
  * [Graphics.UI.Gtk (gtk)](https://hackage.haskell.org/package/gtk3)
  * [System.Directory (directory)](https://hackage.haskell.org/package/directory)
* [FFmpeg](https://www.ffmpeg.org/download.html)
* [ImageMagick](http://www.imagemagick.org/script/download.php)
* [GTK+](http://www.gtk.org/download/index.php)

## Install & Run

### Ubuntu/Mint

```bash
# Install FFmpeg & ImageMagick
sudo add-apt-repository ppa:kirillshkrogalev/ffmpeg-next
sudo apt-get update
sudo apt-get install ffmpeg imagemagick
# Find the latest release at https://github.com/lettier/gifcurry/releases
wget https://github.com/lettier/gifcurry/releases/download/*/gifcurry-linux*.tar.gz
tar xvfz gifcurry-linux*.tar.gz
cd gifcurry-linux*/bin
./gifcurry_gui
./gifcurry_cli
```

### Arch Linux

```bash
# Install FFmpeg and ImageMagick
# Install yaourt (https://archlinux.fr/yaourt-en)
# AUR package: https://aur.archlinux.org/packages/gifcurry/
yaourt -S gifcurry
gifcurry_gui
gifcurry_cli
```

### Mac OS X El Capitan

#### Prebuilt

```bash
# If you don't have Homebrew
/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
brew install ffmpeg
brew install imagemagick
brew install ghostscript
brew install wget
brew install gnome-icon-theme
# Find the latest release at https://github.com/lettier/gifcurry/releases
wget https://github.com/lettier/gifcurry/releases/download/*/gifcurry-macosx*.tar.gz
tar xvfz gifcurry-macosx*.tar.gz
cd gifcurry-macosx*/bin
./gifcurry_gui
./gifcurry_cli
```

#### Build

```bash
# If you don't have Homebrew
/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
brew update
brew install xcode
brew install git
brew install ghc
sudo chown -R $(whoami):admin /usr/local/bin
brew link ghc
brew install cabal-install
brew install pkg-config
brew install glib
brew install gtk+3
brew install cairo
brew install pango
brew install ffmpeg
brew install imagemagick
brew install ghostscript
brew install gnome-icon-theme
git clone git@github.com:lettier/gifcurry.git
cd gifcurry
cabal sandbox init
cabal update
cabal configure
cabal install alex happy -j
cabal install gtk2hs-buildtools -j
cabal install gifcurry -j
cd .cabal-sandbox/bin/
./gifcurry_gui
./gifcurry_cli
```

### Hackage

```bash
# Install ghc and cabal-install
# Install ffmpeg and imagemagick
cabal update
cabal configure
cabal install alex happy -j
cabal install gtk2hs-buildtools -j
cabal install gifcurry -j
cd ~/.cabal/bin
./gifcurry_gui
./gifcurry_cli
```

### Github

```bash
# Install ghc and cabal-install
# Install ffmpeg and imagemagick
git clone git@github.com:lettier/gifcurry.git
cd gifcurry/
cabal sandbox init
cabal update
cabal configure
cabal install alex happy -j
cabal install gtk2hs-buildtools -j
cabal install -j
./.cabal-sandbox/bin/gifcurry_gui
./.cabal-sandbox/bin/gifcurry_cli
```

## License

For license information, see [LICENSE](LICENSE).

_(C) 2016 David Lettier._  
http://www.lettier.com/

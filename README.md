![Gifcurry](http://i.imgur.com/jhU7puN.png)

# Gifcurry

Create animated GIFs, optionally overlaid with text, from video files.

## Screenshots

![GUI](https://i.imgur.com/U8xnK82.gif)

## Sample GIF

![Caminandes 3: Llamigos (2016) - Blender Foundation](https://i.imgur.com/FJW2gBc.gif)  
Credit: [Caminandes 3: Llamigos (2016) - Blender Foundation](http://www.caminandes.com/)

## CLI Usage

```bash
gifcurry_cli [OPTIONS]

Common flags:
  -i --inputfile=FILE      The input video file path and name.
  -o --outputfile=ITEM     The output GIF file path and name.
  -s --starttime=NUM       The start time (in seconds) for the first frame.
  -d --durationtime=NUM    How long the GIF lasts (in seconds) from the start
                           time.
  -w --widthsize=INT       How wide the GIF needs to be. Height will scale to
                           match.
  -q --qualitypercent=NUM  Ranges from 0.0 to 100.0.
  -f --fontchoice=ITEM     Choose your desired font for the top and bottom
                           text.
  -t --toptext=ITEM        The text you wish to add to the top of the GIF.
  -b --bottomtext=ITEM     The text you wish to add to the bottom of the GIF.
  -? --help                Display help message
  -V --version             Print version information
     --numeric-version     Print just the version number
```

## CLI Example

```Bash
~/gifcurry ❯❯❯ ./gifcurry_cli \
  -i ./03_caminandes_llamigos_1080p.mp4 \
  -o ./out.gif \
  -s 42 \
  -d 4 \
  -w 600 \
  -q 100 \
  -f 'Roboto Condensed Bold Italic' \
  -t 'Download' \
  -b 'Gifcurry'

 _____ _  __                           
|  __ (_)/ _|                          
| |  \/_| |_ ___ _   _ _ __ _ __ _   _ 
| | __| |  _/ __| | | | '__| '__| | | |
| |_\ \ | || (__| |_| | |  | |  | |_| |
 \____/_|_| \___|\__,_|_|  |_|   \__, |
                                  __/ |
                                 |___/ 

Gifcurry 2.3.0.0
(C) 2016 David Lettier
lettier.com

----------------------------------------

Input file: ./03_caminandes_llamigos_1080p.mp4
Output file: ./out.gif
Start second: 42.000
Duration: 4.000 seconds
GIF width: 600px
Quality: 100.0%
Font Choice: Roboto Condensed Bold Italic
Top text: Download
Bottom text: Gifcurry

Writing temporary frames to... ./frames13465
Font matched: Roboto-Condensed-Bold-Italic
Writing your GIF to... ./out.gif
Done.
```

## Dependencies

### Run

* [GTK+](http://www.gtk.org/download/index.php)
* [FFmpeg](https://www.ffmpeg.org/download.html)
* [ImageMagick](http://www.imagemagick.org/script/download.php)

### Build

* [GObject Introspection](https://wiki.gnome.org/action/show/Projects/GObjectIntrospection)
* [Haskell](https://www.haskell.org/platform/)

## Install & Run

### Linux

#### [AppImage (GUI Only)](https://appimage.github.io/gifcurry/)

```bash
# Install GTK+ (https://www.gtk.org/download/index.php)
# Install FFmpeg (https://www.ffmpeg.org/download.html)
# Install ImageMagick (https://www.imagemagick.org/script/download.php)
# Install GNU Wget (https://www.gnu.org/software/wget/) or download the AppImage with your browser
# Find the latest release at https://github.com/lettier/gifcurry/releases
# * equals the version number
# Download gifcurry-*-x86_64.AppImage
# Right click on the AppImage and allow the file to be executed as a program
wget https://github.com/lettier/gifcurry/releases/download/*/gifcurry-*-x86_64.AppImage
chmod a+x gifcurry-*-x86_64.AppImage
./gifcurry-*-x86_64.AppImage
# If you would like to install the AppImage, you can run the following
cd
wget "https://raw.githubusercontent.com/lettier/gifcurry/master/packaging/linux/gifcurry-gui-install.sh" -O "gifcurry-gui-install.sh"
chmod +x "gifcurry-gui-install.sh"
./gifcurry-gui-install.sh
```

#### Debian/Ubuntu/Mint

```bash
# Install GTK+ (https://www.gtk.org/download/index.php)
# Install FFmpeg (https://www.ffmpeg.org/download.html)
# Install ImageMagick (https://www.imagemagick.org/script/download.php)
sudo add-apt-repository ppa:jonathonf/ffmpeg-3
sudo apt-get update
sudo apt-get install ffmpeg imagemagick
# Install GNU Wget (https://www.gnu.org/software/wget/)
# Find the latest release at https://github.com/lettier/gifcurry/releases
# * equals the version number
wget https://github.com/lettier/gifcurry/releases/download/*/gifcurry-linux-*.tar.gz
tar -xvfz gifcurry-linux*.tar.gz
cd gifcurry-linux-*/bin
./gifcurry_cli -?
./gifcurry_gui
```

#### [Arch/Manjaro/Antergos](https://aur.archlinux.org/packages/gifcurry/)

```bash
cd
# Install Git
sudo pacman -S git
# Install Gifcurry from AUR
mkdir -p build_gifcurry
cd build_gifcurry
git clone https://aur.archlinux.org/gifcurry.git
cd gifcurry
makepkg -sic
cd
rm -rf build_gifcurry
gifcurry_cli -?
gifcurry_gui
```

### Mac

```bash
# Install Homebrew
/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
brew update
brew install xcode
brew install git
git clone git@github.com:lettier/gifcurry.git
cd gifcurry/
brew cask install haskell-platform
brew install pkg-config gobject-introspection cairo gdk-pixbuf gsettings-desktop-schemas \
  gtk+3 gtk-mac-integration ffmpeg imagemagick ghostscript gnome-icon-theme
stack setup
stack install hsc2hs
stack install
export PATH=$PATH:"$HOME/.local/bin/"
gifcurry_cli -?
gifcurry_gui
```

### [Hackage](https://hackage.haskell.org/package/Gifcurry)

#### Stack

```bash
# Install GTK+ (https://www.gtk.org/download/index.php)
# Install GObject Introspection (https://wiki.gnome.org/action/show/Projects/GObjectIntrospection)
# Install GNU Wget (https://www.gnu.org/software/wget/)
# Install FFmpeg (https://www.ffmpeg.org/download.html)
# Install ImageMagick (https://www.imagemagick.org/script/download.php)
# Install Haskell (https://www.haskell.org/platform/)
# Find the latest version number at https://hackage.haskell.org/package/Gifcurry
# * equals the version number
wget https://hackage.haskell.org/package/Gifcurry-*/Gifcurry-*.tar.gz
tar -xvzf Gifcurry-*.tar.gz
cd Gifcurry-*
stack setup
stack install
stack exec -- gifcurry_cli -?
stack exec -- gifcurry_gui
```

#### Cabal

```bash
# Install GTK+ (https://www.gtk.org/download/index.php)
# Install GObject Introspection (https://wiki.gnome.org/action/show/Projects/GObjectIntrospection)
# Install FFmpeg (https://www.ffmpeg.org/download.html)
# Install ImageMagick (https://www.imagemagick.org/script/download.php)
# Install Haskell (https://www.haskell.org/platform/)
cabal update
cabal install alex happy -j
cabal install gifcurry -j
cd ~/.cabal/bin
./gifcurry_cli -?
./gifcurry_gui
```

### [Github](https://github.com/lettier/gifcurry)

```bash
# Install GTK+ (https://www.gtk.org/download/index.php)
# Install GObject Introspection (https://wiki.gnome.org/action/show/Projects/GObjectIntrospection)
# Install GNU Make (https://www.gnu.org/software/make/)
# Install FFmpeg (https://www.ffmpeg.org/download.html)
# Install ImageMagick (https://www.imagemagick.org/script/download.php)
# Install Haskell (https://www.haskell.org/platform/)
# Install Git (https://git-scm.com/downloads)
git clone https://github.com/lettier/gifcurry.git
cd gifcurry/
make
make run_cli CLI_ARGS='-?'
make run_gui
```

## License

For license information, see [LICENSE](LICENSE).

## Copyright

_(C) 2016 David Lettier_  
[lettier.com](http://www.lettier.com/)

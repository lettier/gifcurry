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
gifcurry_cli \
  -i inputFile \
  -o outputFile \
  -s startTime \
  -d durationTime \
  -w widthSize \
  -q qualityPercent \
  -f fontChoice \
  -t topText \
  -b bottomText
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

Gifcurry 2.2.0.0
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

* [Haskell](https://www.haskell.org/platform/)
* [FFmpeg](https://www.ffmpeg.org/download.html)
* [ImageMagick](http://www.imagemagick.org/script/download.php)
* [GTK+](http://www.gtk.org/download/index.php)

## Install & Run

### Linux

#### AppImage (GUI Only)

```bash
# Install FFmpeg (https://www.ffmpeg.org/download.html)
# Install ImageMagick (https://www.imagemagick.org/script/download.php)
# Install GNU Wget (https://www.gnu.org/software/wget/) or download the AppImage with your browser
# Find the latest release at https://github.com/lettier/gifcurry/releases
# * equals the version number
# Download gifcurry-linux-app-image-*
# Right click on the AppImage and allow the file to be executed as a program
wget https://github.com/lettier/gifcurry/releases/download/*/gifcurry-linux-app-image-*
chmod a+x gifcurry-linux-app-image-*
./gifcurry-linux-app-image-*
# If you would like to install the AppImage, you can run the following
cd
wget "https://raw.githubusercontent.com/lettier/gifcurry/master/packaging/linux/gifcurry-gui-install.sh" -O "gifcurry-gui-install.sh"
chmod +x "gifcurry-gui-install.sh"
./gifcurry-gui-install.sh
```

#### Ubuntu/Mint

```bash
# Install FFmpeg (https://www.ffmpeg.org/download.html)
# Install ImageMagick (https://www.imagemagick.org/script/download.php)
sudo add-apt-repository ppa:kirillshkrogalev/ffmpeg-next
sudo apt-get update
sudo apt-get install ffmpeg imagemagick
# Install GNU Wget (https://www.gnu.org/software/wget/)
# Find the latest release at https://github.com/lettier/gifcurry/releases
# * equals the version number
wget https://github.com/lettier/gifcurry/releases/download/*/gifcurry-linux-*.tar.gz
tar -xvfz gifcurry-linux*.tar.gz
cd gifcurry-linux-*/bin
./gifcurry_gui
./gifcurry_cli -?
```

#### Arch/Manjaro

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
gifcurry_gui
gifcurry_cli -?
```

### Mac

#### Binaries

```bash
# If you do not have Homebrew
/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
brew install ffmpeg
brew install imagemagick
brew install ghostscript
brew install wget
brew install gnome-icon-theme
# Find the latest release at https://github.com/lettier/gifcurry/releases
# * equals the version number
wget https://github.com/lettier/gifcurry/releases/download/*/gifcurry-macosx-*.tar.gz
tar xvfz gifcurry-macosx-*.tar.gz
cd gifcurry-macosx-*/bin
./gifcurry_gui
./gifcurry_cli -?
```

#### Compile

```bash
# If you do not have Homebrew
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
mkdir gifcurry
cd gifcurry
cabal sandbox init
cabal update
cabal install alex happy -j
cabal install gifcurry -j
cd .cabal-sandbox/bin/
./gifcurry_gui
./gifcurry_cli -?
```

### Hackage

#### Stack

```bash
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
stack exec -- gifcurry_gui
stack exec -- gifcurry_cli -?
```

#### Cabal

```bash
# Install FFmpeg (https://www.ffmpeg.org/download.html)
# Install ImageMagick (https://www.imagemagick.org/script/download.php)
# Install Haskell (https://www.haskell.org/platform/)
cabal update
cabal install alex happy -j
cabal install gifcurry -j
cd ~/.cabal/bin
./gifcurry_gui
./gifcurry_cli -?
```

### Github

```bash
# Install GNU Make (https://www.gnu.org/software/make/)
# Install FFmpeg (https://www.ffmpeg.org/download.html)
# Install ImageMagick (https://www.imagemagick.org/script/download.php)
# Install Haskell (https://www.haskell.org/platform/)
git clone git@github.com:lettier/gifcurry.git
cd gifcurry/
make
make run_gui
make run_cli CLI_ARGS='-?'
```

## License

For license information, see [LICENSE](LICENSE).

## Copyright

_(C) 2016 David Lettier_  
[lettier.com](http://www.lettier.com/)

![Gifcurry](https://i.imgur.com/KFZyEqq.png)

# Tell me about Gifcurry. :smiley:

Gifcurry is the open-source, Haskell-built video editor for GIF makers.
Load a video, make some edits, and save it as a GIF—Gifcurry makes your life easy! :sunglasses:
Most video formats should work, so go wild.
And since it's made with Haskell, you know it's good. :100:

For the command line averse, there is a GUI. :computer_mouse:
Die-hard terminal aficionado? Gifcurry has you covered with its CLI. :desktop_computer:
And for the Haskell programmers out there, there is also a library :books: API.

Gifcurry can save your creation as a GIF or as a video. :vhs:
So if you hate :angry: GIFs with a passion—no problem!
Just select "Save as a Video" and do your part to rid :wastebasket: the world of GIFs.

Enjoy memes? :trollface: Great! Gifcurry can add text all over your GIF.
You can change the font, size, color, position, outline, rotation, and the timing.
Create the next viral meme with Gifcurry.

Did you know Gifcurry slices...and dices? :fork_and_knife:
You can crop :scissors: from the left, the right, the top, and/or the bottom.
With Gifcurry, you can slice up some tasty GIFs.

Is Gifcurry another Electron app? No way! Gifcurry is 100% #ElectronFree.
No need to download more RAM :ram:, Gifcurry is light as a feather.
Run :running: it all day, run it all year—you'll never notice.

"So...Gifcurry is just FFmpeg and ImageMagick?"—nope.
Gifcurry hides all the goofy details so you can concentrate on what matters—the almighty :raised_hands: GIF.

Become a GIF master :muscle: with Gifcurry!

## What do I need Gifcurry for? :thinking:

:ballot_box_with_check: Want to show off that new UI feature in a pull request? Gifcurry.  
:ballot_box_with_check: Your template doesn't allow video in the hero image? Gifcurry.  
:ballot_box_with_check: No GIF of your favorite movie scene? Gifcurry.  
:ballot_box_with_check: Need a custom animated emoji for Slack? Gifcurry.  
:ballot_box_with_check: Can't find the perfect GIF for that reply-all email? Gifcurry.  
:ballot_box_with_check: Your README needs a GIF? Gifcurry.  
:ballot_box_with_check: That presentation slide could use some animation? Gifcurry.  
:ballot_box_with_check: Video doesn't auto play on iOS? Gifcurry.  
:ballot_box_with_check: Time to promote your indie game? Gifcurry.  
:ballot_box_with_check: Need to add subtitles to a video? Gifcurry.  

Gifcurry comes in handy :wrench: for all sorts of scenarios.

## What does the GUI look :eyes: like?

![Gifcurry GUI](https://i.imgur.com/SJ8zovM.gif)

## How do I use the command line interface (CLI)? :desktop_computer:

```text
gifcurry_cli [OPTIONS]

FILE IO:
  -i --input-file=FILE   The input video file path.
  -o --output-file=FILE  The output GIF file path.
  -m --save-as-video     If present, saves the GIF as a video.
TIME:
  -s --start-time=NUM    The start time (in seconds) for the first frame.
  -e --end-time=NUM      The end time (in seconds) for the last frame.
OUTPUT FILE SIZE:
  -w --width=INT         How wide the output needs to be. Height will scale
                         to match.
  -f --fps=INT           How many frames per second the output should have.
  -c --color-count=INT   How many colors are used in the output.
  -d --dither            If present, uses dither.
CROP:
  -L --left-crop=NUM     The amount you wish to crop from the left.
  -R --right-crop=NUM    The amount you wish to crop from the right.
  -T --top-crop=NUM      The amount you wish to crop from the top.
  -B --bottom-crop=NUM   The amount you wish to crop from the bottom.
TEXT:
  -t --text-file=FILE    Either a text overlays YAML or SRT subtitles file
                         path.
  -? --help              Display help message
  -V --version           Print version information

Visit https://github.com/lettier/gifcurry for more information.
```

## Got a CLI example?

```text
gifcurry_cli \
  -i ~/Videos/video.webm \
  -o ~/tmp/test \
  -s 150 \
  -e 151 \
  -t ~/tmp/text-overlays.yaml \
  -w 800 \
  -f 15 \
  -c 100 \
  -d
  -L 0.1 \
  -R 0.1 \
  -T 0.1 \
  -B 0.1 \
  -m

         ▄▄▄▄▄▄▄▄                                                                             
     ▄▄████    ▀▀███▄                                                                         
      ████▀   ▄    ▀███           ▄     ▐██▌   ▄███▄                                          
  ▄   ▐███   ████   ▀███      ▄███▀▀██        ███                                             
 ▐█▌   ██   ▐███     ████    ███        ▐██  █████▌ ▄█████ ▐██▌  ██▌  ██▄██▌ ██▄██▌ ██▌   ███ 
 ███   ▐▌   ███      ▐███▌   ███  ████▌ ▐██   ██▌  ███     ▐██▌  ██▌  ███▀   ███▀   ▐██  ███  
 ████      ███▀  ▐█   ███▌   ███    ██▌ ▐██   ██▌  ███     ▐██▌  ██▌  ██▌    ██▌     ██▌▐██   
 ▐███▄    ▐██▌   ██    ██     ███▄▄▄██▌ ▐██   ██▌   ███▄▄█  ███▄███▌  ██▌    ██▌      ████▌   
  ▀███   ▀███   ▐███   ▀        ▀▀▀▀▀    ▀▀   ▀▀      ▀▀▀     ▀▀▀     ▀▀     ▀▀        ███    
    ███▄   ▀    ████▌                                                                ███▀     
      ▀███▄▄   █████▀                                                                         
          ▀▀▀▀▀▀▀                                                                             


Gifcurry 6.0.1.0
(C) 2016 David Lettier
lettier.com

[INFO] Here are your settings.

  - FILE IO:
    - Input File:    /home/Videos/video.webm
    - Output File:   /home/tmp/test.webm
    - Save As Video: Yes
  - TIME:
    - Start Second: 150.000
    - End   Second: 151.000
  - OUTPUT FILE SIZE:
    - Width:       800px
    - FPS:         15
    - Color Count: 100
    - Dither:      True
  - TEXT:
    - Text: This is a test.
      - Font:
        - Family:  Sans
        - Size:    30
        - Style:   Normal
        - Stretch: Normal
        - Weight:  800
      - Time:
        - Start Second: 150.000
        - End   Second: 151.000
      - Translation:
        - Origin: NorthWest
        - X:      0.0
        - Y:      0.0
      - Rotation:
        - Degrees: 0
      - Outline:
        - Size: 10
        - Color: rgb(1,100,10)
      - Fill:
        - Color: rgb(255,255,0)
  - CROP:
    - Left:   0.100
    - Right:  0.100
    - Top:    0.100
    - Bottom: 0.100

[INFO] Writing the temporary frames to: /home/.cache/gifcurry/gifcurry-frames30450
[INFO] Adding text.
[INFO] Converting the frames to the specified color count.
[INFO] Saving your video to: /home/tmp/test.webm
[INFO] All done.
```

## What is the format for the text overlays YAML file? :1234:

Here's an example file:

```yaml
- text: This is an example.
  fontFamily:   Sans
  fontStyle:    Oblique
  fontStretch:  Normal
  fontWeight:   30
  fontSize:     800
  origin:       NorthWest
  xTranslation: 0.0
  yTranslation: 0.0
  rotation:     20
  startTime:    5.000
  endTime:      8.000
  outlineSize:  7
  outlineColor: rgba(1,100,10,1.0)
  fillColor:    rgba(255,255,0,0.0)
- text: This is another example.
  fontFamily:   Serif
  fontStyle:    Italic
  fontStretch:  Condensed
  fontWeight:   30
  fontSize:     800
  origin:       Center
  xTranslation: 10.0
  yTranslation: 11.0
  rotation:     20
  startTime:    5.000
  endTime:      8.000
  outlineSize:  7
  outlineColor: rgb(1,11,100)
  fillColor:    rgba(255,0,0,0.0)
```

`fontStyle` can be:

- `Any`
- `Italic`
- `Normal`
- `Oblique`

`fontStretch` can be:

- `Any`
- `Condensed`
- `Expanded`
- `ExtraCondensed`
- `ExtraExpanded`
- `Normal`
- `SemiCondensed`
- `SemiExpanded`
- `UltraCondensed`
- `UltraExpanded`

`origin` can be:

- `North`
- `NorthEast`
- `East`
- `SouthEast`
- `South`
- `SouthWest`
- `West`
- `NorthWest`

`outlineSize` ranges from `0` to `10`.

`outlineColor` and `fillColor` can either be `rgba(red,green,blue,alpha)` or `rgb(red,green,blue)`
where `red`, `green`, and `blue` range from `0` to `255` and `alpha` ranges from `0.0` to `1.0`.

## How do I get a copy of Gifcurry? :floppy_disk:

Gifcurry works on Linux, Mac, and most likely Windows.
Make sure you have FFmpeg, GStreamer, ImageMagick, and GTK+ installed on your machine.
To find the latest version of Gifcurry, head over to the
[releases page](https://github.com/lettier/gifcurry/releases).

### I use Linux. :penguin:

If you use Linux then the easiest way to grab a copy of Gifcurry is by downloading the
[AppImage](https://github.com/lettier/gifcurry/releases/download/6.0.1.0/gifcurry-6.0.1.0-x86_64.AppImage).
After you download the
[AppImage](https://github.com/lettier/gifcurry/releases/download/6.0.1.0/gifcurry-6.0.1.0-x86_64.AppImage),
right click on it, select permissions, and check the box near execute.
With that out of the way—you're all set—just double click on the AppImage
and the GUI will fire right up.

You can also download and install the
[AppImage](https://github.com/lettier/gifcurry/releases/download/6.0.1.0/gifcurry-6.0.1.0-x86_64.AppImage)
using the handy
[AppImage install script](https://raw.githubusercontent.com/lettier/gifcurry/master/packaging/linux/app-image/gifcurry-app-image-install.sh)
(right click and save link as).
Download the script, right click on it, select permissions, check the box near execute, and double click on it.
You should now see Gifcurry listed alongside your other installed programs.

If you want the CLI then download the
[prebuilt version](https://github.com/lettier/gifcurry/releases/download/6.0.1.0/gifcurry-linux-6.0.1.0.tar.gz)
for Linux, extract it, open up your terminal,
`cd` to the bin folder, and then run `gifcurry_cli -?`.
As an added bonus, inside the bin directory is the GUI version
too so now you have both.

#### I use Arch/Manjaro/Antergos/pacman. :ghost:

If you'd rather install Gifcurry via pacman then copy the following into your terminal.

```bash
cd
sudo pacman -S git ffmpeg imagemagick gstreamer gst-plugins-base-libs gst-plugins-base gst-plugins-good gst-plugins-bad gst-libav
cd "$HOME/Downloads"
git clone https://aur.archlinux.org/gifcurry.git
cd gifcurry
makepkg -sic
cd "$HOME/Downloads"
rm -rf gifcurry
cd
gifcurry_cli -?
gifcurry_gui
```

#### I use Ubuntu/Mint/Debian/Deepin/snap. :cyclone:

Gifcurry is available as a snap from [Snapcraft](https://snapcraft.io/).
If you don't already have `snap`, go ahead and install it using the command `sudo apt install snapd`.

You can install the
[Gifcurry snap](https://snapcraft.io/gifcurry)
right from your browser or via the command line.
For the command line route, paste the following into your terminal.

```bash
snap install gifcurry
sudo snap connect gifcurry:removable-media
gifcurry
```

The
[Gifcurry snap](https://snapcraft.io/gifcurry)
only comes with the GUI.
If you want the CLI, download the
[prebuilt version](https://github.com/lettier/gifcurry/releases/download/6.0.1.0/gifcurry-linux-6.0.1.0.tar.gz)
for Linux.

### I use Mac. :apple:

Mac users can download the
[Mac install script](https://raw.githubusercontent.com/lettier/gifcurry/master/packaging/mac/gifcurry-mac-install-script.command)
by holding down control, clicking the link, selecting "Save Link As...", selecting where Downloads, and clicking save.
To run the script, hold down the command key and press the space bar.
Now type terminal and hit enter. After the terminal comes up, copy and paste the following.

```bash
cd ~/Downloads
chmod +x gifcurry-mac-install-script.command
./gifcurry-mac-install-script.command
```

After running the install script, a shortcut to both the Gifcurry GUI and CLI will be on your desktop.

### I'm a Haskell developer. :hammer:

If you develop Haskell programs then the easiest way to build Gifcurry is with
[Haskell Stack](https://docs.haskellstack.org/en/stable/README/).
Copy the following into your terminal.

```bash
git clone https://github.com/lettier/gifcurry.git
cd gifcurry
stack setup
stack install alex happy
stack install gtk2hs-buildtools
stack install hsc2hs
stack install
$HOME/.local/bin/gifcurry_cli -?
$HOME/.local/bin/gifcurry_gui
```

## What dependencies does Gifcurry use? :thinking:

### To run Gifcurry. :running:

* [GTK+ >= 3.10](http://www.gtk.org/download/index.php)
* [FFmpeg >= 2.8.15](https://www.ffmpeg.org/download.html)
* [GStreamer >= 1.0](https://gstreamer.freedesktop.org/download/)
    * [GStreamer Plugins](https://gstreamer.freedesktop.org/modules/)
* [ImageMagick >= 6](http://www.imagemagick.org/script/download.php)

### To build Gifcurry. :construction_worker:

* [GObject Introspection](https://wiki.gnome.org/action/show/Projects/GObjectIntrospection)
* [Haskell Stack](https://docs.haskellstack.org/en/stable/README/)

## What is the license? :scroll:

For license information, see [LICENSE](LICENSE).

## Who wrote Gifcurry? :copyright:

(C) 2016 David Lettier  
[lettier.com](http://www.lettier.com/)

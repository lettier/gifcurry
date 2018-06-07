![Gifcurry](https://i.imgur.com/9pS8Ibp.png)

# Tell me about Gifcurry.

Gifcurry is your only open source video-to-GIF maker built with Haskell.
Load a video, make some edits, and save it as a GIF—it's that easy.
Most video formats should work so go wild.
And since it's made with Haskell, you know it's good.

For the command line averse, there is a GUI.
Die-hard terminal aficionado? Gifcurry has you covered with its CLI.
And for the Haskell programmers out there, there is also a library API.

Gifcurry can save your creation as a GIF or as a video.
So if you hate GIFs with a passion—no problem!
Just select "Save as a Video" and do your part to rid the world of GIFs.

Enjoy memes? Great! Gifcurry can add text all over your GIF.
You can change the font, size, color, position, outline, rotation, and the timing.
Create the next viral meme with Gifcurry.

Did you know Gifcurry slices...and dices?
You can crop from the left, the right, the top, and/or the bottom.
With Gifcurry, you can slice up some tasty GIFs.

Is Gifcurry another Electron app? No way! Gifcurry is 100% #ElectronFree.
No need to download more RAM, Gifcurry is light as a feather.
Run it all day, run it all year—you'll never notice.

"So...Gifcurry is just FFmpeg and ImageMagick?"—nope.
Gifcurry hides all the goofy details so you can concentrate on what matters—the almighty GIF.
Making GIFs with Gifcurry is fun so try it out!

## What do I need Gifcurry for?

Want to show off that new UI feature in a pull request? Gifcurry.  
Your template doesn't allow video in the hero image? Gifcurry.  
No GIF of your favorite movie scene? Gifcurry.  
Need a custom animated emoji for Slack? Gifcurry.  
Can't find the perfect GIF for that reply-all email? Gifcurry.  
Your README needs a GIF? Gifcurry.  
That presentation slide could use some animation? Gifcurry.  
Video doesn't auto play on iOS? Gifcurry.  

Gifcurry comes in handy for all sorts of scenarios.

## What does the GUI look like?

![Gifcurry GUI](https://i.imgur.com/IhB50O1.gif)


## How do I use the command line interface (CLI)?

```text
gifcurry_cli [OPTIONS]

FILE IO:
  -i --input-file=FILE          The input video file path and name.
  -o --output-file=FILE         The output GIF file path and name.
  -m --save-as-video            If present, saves the GIF as a video.
TIME:
  -s --start-time=NUM           The start time (in seconds) for the first
                                frame.
  -d --duration-time=NUM        How long the GIF lasts (in seconds) from the
                                start time.
OUTPUT FILE SIZE:
  -w --width-size=INT           How wide the GIF needs to be. Height will
                                scale to match.
  -q --quality=ITEM             Controls how many colors are used and the
                                frame rate.
                                The options are High, Medium, and Low.
CROP:
  -L --left-crop=NUM            The amount you wish to crop from the left.
  -R --right-crop=NUM           The amount you wish to crop from the right.
  -T --top-crop=NUM             The amount you wish to crop from the top.
  -B --bottom-crop=NUM          The amount you wish to crop from the bottom.
TEXT:
  -t --text-overlays-file=FILE  The text overlays YAML file path and name.
                                The format is:
                                - text:         ...
                                  fontFamily:   ...
                                  fontStyle:    ...
                                  fontStretch:  ...
                                  fontWeight:   ...
                                  fontSize:     ...
                                  origin:       ...
                                  xTranslation: ...
                                  yTranslation: ...
                                  rotation:     ...
                                  startTime:    ...
                                  durationTime: ...
                                  outlineSize:  ...
                                  outlineColor: ...
                                  fillColor:    ...
                                - text:         ...
                                ...


  -? --help                     Display help message
  -V --version                  Print version information

Visit https://github.com/lettier/gifcurry for more information.
```

## Got a CLI example?

```text
gifcurry_cli \
  -i ~/Videos/video.webm \
  -o ~/tmp/test \
  -s 150 \
  -d 1 \
  -t ~/tmp/text-overlays.yaml \
  -w 800 \
  -q High \
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


Gifcurry 4.0.0.0
(C) 2016 David Lettier
lettier.com

[INFO] Here are your settings.

  - FILE IO:
    - Input File: /home/Videos/video.webm
    - Output File: /home/tmp/test.webm
    - Save As Video: Yes
  - TIME:
    - Start Second: 150.000
    - Duration Time: 1.000 seconds
  - OUTPUT FILE SIZE:
    - Width Size: 800px
    - Quality: High
  - TEXT:
    - Text: This is a test.
      - Font:
        - Family: Sans
        - Size: 30
        - Style: Normal
        - Stretch: Normal
        - Weight: 800
      - Time:
        - Start: 150.000 seconds
        - Duration: 20.000 seconds
      - Translation:
        - Origin: NorthWest
        - X: 0.0
        - Y: 0.0
      - Rotation:
        - Degrees: 0
      - Outline:
        - Size: 10
        - Color: rgb(1,100,10)
      - Fill:
        - Color: rgb(255,255,0)
  - CROP:
    - Left: 0.100
    - Right: 0.100
    - Top: 0.100
    - Bottom: 0.100

[INFO] Writing the temporary frames to: /home/.cache/gifcurry/gifcurry-frames30450
[INFO] Adding text...
[INFO] Saving your video to: /home/tmp/test.webm
[INFO] All done.
```

## How do I get a copy of Gifcurry?

Gifcurry works on Linux, Mac, and most likely Windows.
Make sure you have FFmpeg, GStreamer, ImageMagick, and GTK+ installed on your machine.
To find the latest version of Gifcurry, head over to the
[releases page](https://github.com/lettier/gifcurry/releases).

### I use Linux.

If you use Linux then the easiest way to grab a copy of Gifcurry is by downloading the
[AppImage](https://github.com/lettier/gifcurry/releases/download/4.0.0.0/gifcurry-4.0.0.0-x86_64.AppImage).
After you download the
[AppImage](https://github.com/lettier/gifcurry/releases/download/4.0.0.0/gifcurry-4.0.0.0-x86_64.AppImage),
right click on it, select permissions, and check the box near execute.
With that out of the way—you're all set—just double click on the AppImage
and the GUI will fire right up.

You can also download and install the
[AppImage](https://github.com/lettier/gifcurry/releases/download/4.0.0.0/gifcurry-4.0.0.0-x86_64.AppImage)
using the handy
[AppImage install script](https://raw.githubusercontent.com/lettier/gifcurry/master/packaging/linux/app-image/gifcurry-app-image-install.sh)
(right click and save link as).
Download the script, right click on it, select permissions, check the box near execute, and double click on it.
You should now see Gifcurry listed alongside your other installed programs.

If you want the CLI then download the
[prebuilt version](https://github.com/lettier/gifcurry/releases/download/4.0.0.0/gifcurry-linux-4.0.0.0.tar.gz)
for Linux, extract it, open up your terminal,
`cd` to the bin folder, and then run `gifcurry_cli -?`.
As an added bonus, inside the bin directory is the GUI version
too so now you have both.

#### I use Arch/Manjaro/Antergos/pacman.

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

#### I use Ubuntu/Mint/Debian/Deepin/snap.

Gifcurry is available as a snap from [Snapcraft](https://snapcraft.io/).
If you don't already have `snap`, go ahead and install it using the command `sudo apt install snapd`.

You can install the
[Gifcurry snap](https://snapcraft.io/gifcurry)
right from your browser or via the command line.
For the command line route, paste the following into your terminal.

```bash
snap install gifcurry
sudo snap connect gifcurry:mount-observe
sudo snap connect gifcurry:removable-media
sudo snap connect gifcurry:raw-usb
gifcurry
```

The
[Gifcurry snap](https://snapcraft.io/gifcurry)
only comes with the GUI.
If you want the CLI, download the
[prebuilt version](https://github.com/lettier/gifcurry/releases/download/4.0.0.0/gifcurry-linux-4.0.0.0.tar.gz)
for Linux.

### I use Mac.

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

### I'm a Haskell developer.

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

## What dependencies does Gifcurry use?

### To run Gifcurry.

* [GTK+ >= 3.10](http://www.gtk.org/download/index.php)
* [FFmpeg >= 3](https://www.ffmpeg.org/download.html)
* [GStreamer >= 1.0](https://gstreamer.freedesktop.org/download/)
    * [GStreamer Plugins](https://gstreamer.freedesktop.org/modules/)
* [ImageMagick >= 6](http://www.imagemagick.org/script/download.php)

### To build Gifcurry.

* [GObject Introspection](https://wiki.gnome.org/action/show/Projects/GObjectIntrospection)
* [Haskell Stack](https://docs.haskellstack.org/en/stable/README/)

## What is the license?

For license information, see [LICENSE](LICENSE).

## Who wrote Gifcurry?

(C) 2016 David Lettier  
[lettier.com](http://www.lettier.com/)

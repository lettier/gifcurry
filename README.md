![Gifcurry](https://i.imgur.com/1omeH3m.png)

# Tell me about Gifcurry.

Gifcurry is your only open source video to GIF maker built with Haskell.
Load a video, make some edits, and save it as a GIF—it's that easy.
Most video formats should work so go wild.
And since it's made with Haskell, you know it's good.

For the command line averse, there is a GUI.
Die-hard terminal aficionado? Gifcurry has you covered with its CLI.
And for the Haskell programmers out there, there is also a library API.

Gifcurry can save your creation as a GIF or as a video.
So if you hate GIFs with a passion—no problem!
Just select "save as video" and do your part to rid the world of GIFs.

Enjoy memes? Great! Gifcurry can add text to the top and/or the bottom of your GIF.
Just type in some text for the top or type in some text for the bottom or type in
some pithy text for both the top and bottom—Gifcurry don't care.
Oh and you can select the font too so you're never too far from Comic Sans.

Gifcurry caters to the power user with its crop tool.
You can crop from the left, the right, the top, and/or the bottom.
With Gifcurry, you can cut out anything you don't want.

Is Gifcurry another Electron app? No way! Gifcurry is 100% #electronfree.
No need to download more RAM, Gifcurry is light as a feather.
Run it all day, run it all year—you'll never notice.

"So...Gifcurry is just FFmpeg and ImageMagick." Nope.
Gifcurry hides all the goofy details so you can concentrate on what matters—the almighty GIF.
Making GIFs with Gifcurry is fun so try it out!

## What do I need Gifcurry for?

Need to show off that new UI feature in a pull request? Gifcurry.  
Your template doesn't allow video in the hero image? Gifcurry.  
No GIF of your favorite movie scene? Gifcurry.  
Need a custom animated emoji for Slack? Gifcurry.  
Have an idea of the perfect GIF to close out that email? Gifcurry.  
Your README needs a GIF? Gifcurry.  
Video doesn't auto play on iOS? Gifcurry.  

Gifcurry comes in handy for all sorts of scenarios.

## What does the GUI look like?

![Gifcurry GUI](https://i.imgur.com/dVpQfHq.gif)

## Got any sample GIFs?

![GIF](https://i.imgur.com/alxcMli.gif)
![GIF](https://i.imgur.com/FUjIBm2.gif)

## How do I use the command line interface (CLI)?

```bash
gifcurry_cli [OPTIONS]

FILE IO:
  -i --input-file=FILE      The input video file path and name.
  -o --output-file=FILE     The output GIF file path and name.
  -m --save-as-video        If present, saves the GIF as a video.
TIME:
  -s --start-time=NUM       The start time (in seconds) for the first frame.
  -d --duration-time=NUM    How long the GIF lasts (in seconds) from the
                            start time.
OUTPUT FILE SIZE:
  -w --width-size=INT       How wide the GIF needs to be. Height will scale
                            to match.
  -q --quality-percent=NUM  From 1 (very low quality) to 100 (the best
                            quality). Controls how many colors are used and how
                            many frames per second there are.
TEXT:
  -f --font-choice=TEXT     Choose your desired font for the top and bottom
                            text.
  -t --top-text=TEXT        The text you wish to add to the top of the GIF.
  -b --bottom-text=TEXT     The text you wish to add to the bottom of the
                            GIF.
CROP:
  -L --left-crop=NUM        The amount you wish to crop from the left.
  -R --right-crop=NUM       The amount you wish to crop from the right.
  -T --top-crop=NUM         The amount you wish to crop from the top.
  -B --bottom-crop=NUM      The amount you wish to crop from the bottom.
INFO:
  -? --help                 Display help message
  -V --version              Print version information
```

## Got a CLI example?

```text
gifcurry_cli \
-i ~/Videos/video.webm -o ~/tmp/test -m \
-L 25 -R 25 -T 25 -B 25 \
-s 149.11 -d 1 \
-f 'fira sans' -t 'Top Text' -b 'Bottom Text'

         ppDPPPDbDDpp                                                                                    
      pDPPPP       )DPDp                      )                                                          
       PPPPP   )pp    DPPp          ppppp    PPP    pDbDD                                                
   p   )PPP    PPPD    PPPD      pDPDPPPDP         PPP                                                   
  bP    DPP   pPPP     )PPPb    (PPP         PPP )PPPPPP  pDPPPDb PPP   PPb  PPbpDPP PPbpPP ·DPb   pPD   
 (PPb   )D   (PPD       bPPP    PPP   DDDDD  PPP   PPP   PPb      PPP   PPb  PPPP    PPPP    (PP  pPPC   
 (PPPp       PPP    b   )PPP    DPPp    PPP  PPP   PPP  (PPb      PPP   PPb  PPP     PPP      DPb PPP    
  PPPb      DPPP   pPp   DPb     DPDp   PPP  PPP   PPP   DPPp  p  PPP  pPPb  PPP     PPP       PPpPP     
  )PPPp   (DPPP   )PPb    b       (PPDDPPP   PPP   PPP    (PDDDPC  PDDP PPC  PPP     PPP       )DPPP     
   )DPPp   )DD    DPPPb                                                                        pbPP      
     )DPbp       (PPPPPb                                                                      PPC        
        SPDbDppppPPDPC                                                                                   

Gifcurry 3.0.0.2
(C) 2016 David Lettier
lettier.com

[INFO] Here are your settings.

  - FILE IO:
    - Input File: /home/Videos/video.webm
    - Output File: /home/tmp/test.webm
    - Save As Video: Yes
  - TIME:
    - Start Second: 149.110
    - Duration Time: 1.000 seconds
  - OUTPUT FILE SIZE:
    - Width Size: 500px
    - Quality Percent: 100.0%
  - TEXT:
    - Font Choice: fira sans
    - Top Text: Top Text
    - Bottom Text: Bottom Text
  - CROP:
    - Left Crop: 25.000
    - Right crop: 25.000
    - Top Crop: 25.000
    - Bottom Crop: 25.000

[INFO] Writing the temporary frames to: /home/.cache/gifcurry/gifcurry-frames17389
[INFO] Your font choice matched to "Fira-Sans".
[INFO] Saving your GIF to: /home/.cache/gifcurry/gifcurry-frames17389/finished-result.gif
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
[AppImage](https://github.com/lettier/gifcurry/releases/download/3.0.0.2/gifcurry-3.0.0.2-x86_64.AppImage).
After you download the
[AppImage](https://github.com/lettier/gifcurry/releases/download/3.0.0.2/gifcurry-3.0.0.2-x86_64.AppImage),
right click on it, select permissions, and check the box near execute.
With that out of the way—you're all set—just double click on the AppImage
and the GUI will fire right up.

You can also download and install the
[AppImage](https://github.com/lettier/gifcurry/releases/download/3.0.0.2/gifcurry-3.0.0.2-x86_64.AppImage)
using the handy
[AppImage install script](https://raw.githubusercontent.com/lettier/gifcurry/master/packaging/linux/app-image/gifcurry-app-image-install.sh)
(right click and save link as).
Download the script, right click on it, select permissions, check the box near execute, and double click on it.
You should now see Gifcurry listed alongside your other installed programs.

If you want the CLI then download the
[prebuilt version](https://github.com/lettier/gifcurry/releases/download/3.0.0.2/gifcurry-linux-3.0.0.2.tar.gz)
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
[prebuilt version](https://github.com/lettier/gifcurry/releases/download/3.0.0.2/gifcurry-linux-3.0.0.2.tar.gz)
for Linux.

### I use Mac.

Mac users can download and run the
[Mac install script](https://raw.githubusercontent.com/lettier/gifcurry/master/packaging/mac/gifcurry-mac-install-script.command)
(hold down control, click the link, and select "Save Link As...").
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
* [ImageMagick >= 6](http://www.imagemagick.org/script/download.php)

### To build Gifcurry.

* [GObject Introspection](https://wiki.gnome.org/action/show/Projects/GObjectIntrospection)
* [Haskell Stack](https://docs.haskellstack.org/en/stable/README/)

## What is the license?

For license information, see [LICENSE](LICENSE).

## Who wrote Gifcurry?

(C) 2016 David Lettier  
[lettier.com](http://www.lettier.com/)

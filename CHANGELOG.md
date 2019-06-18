# [Gifcurry](https://github.com/lettier/gifcurry)

## Changelog

### 6.0.0.0

#### Added

- Features
  - Dithering
  - SRT subtitles file parsing
  - Open text as text overlays
  - Save text overlays to file
  - Raise text overlay
  - Lower text overlay
  - Clone text overlay
- GUI
  - Dither toggle button
  - Dither icon
  - Remove all button
  - Open text button
  - Clone text button
  - Save text button
  - Open text dialog
  - Save text dialog
  - Keyboard controls
    - - seek left
    - + seek right
  - Keyboard documentation to the about dialog
  - `gifcurry-button-dangerous` CSS class
  - Crop icons
    - Left
    - Right
    - top
    - Bottom
  - Text overlay raise button
  - Text overlay lower button
  - `GuiRecords`
    - `GuiPreviewState`
      - `maybeDither`
    - `GuiPreviewFunctionArgs`
      - `dither`
      - `ditherChanged`
    - `GuiMakeFramePreviewFunctionArgs`
      - `dither`
    - `GuiSetOrResetFramePrevewFunctionArgs`
      - `dither`
- CLI
  - `CliArgs`
    - `dither`
- Lib
  - Interface
    - `saveTextOverlaysToFile`
    - `getRgba`
    - `convertFileToTextOverlays`
    - `parseVersionNumber`
  - `GifParams`
    - `dither`

#### Changed

- Features
  - Duration time to end time
- GUI
  - Time slices clock stays within the draw area
  - The video no longer pauses when clicking outside of the start and end times in the times slices widget
  - Merges all overlapping text overlay time slices into a single draw call for text overlays with hidden controls
  - Text overlay pen-icon to t-icon
  - Text overlay text entry signal from `afterWidgetKeyReleaseEvent` to `afterEditableChanged`
  - Text overlay left and top to text overlay horizontal and vertical
  - Text overlay spin button position icons
  - Text overlay remove button CSS class
  - Tooltips
    - `First Frame` to `Last Frame`
    - `Last Frame` to `First Frame`
  - Button labels
    - `Save as a GIF` to `GIF`
    - `Save as a Video` to `Video`
    - `File Size` to `Size`
    - `Save & Open` to `File`
  - Crop spin button icons
  - Crop rectangles no longer overlap
  - Moved `text-decoration-color` to `style-3-18.css` for Ubuntu 14.04
- CLI
  - Can now process either a text overlays YAML file or a SRT subtitles file
  - `--text-overlays-file` to `--text-file`
- Lib
  - Only uses `exact:1` if FFmpeg > 2
  - `gifParamsValid` to `validateGifParams`
  - `gif` to `createGif`
  - `getPlayableMetadata`
    - `gifParams` to `inputFile`
  - `getOutputFileWithExtension` only adds the required file extension if the output file does not already have it

#### Removed

- GUI
  - Text overlay index number from text overlay toggle button label

### 5.0.0.0

#### Added

- FPS control
- Color count control
- The ability to load a GIF instead of just video files
- Converts input GIFs to videos and caches the result
- Tooltips to the main toggle buttons
- Additional processing step of reducing each frame's colors if color count < 256 when saving to video
- `PlayableMetadata`
- Color count controls fuzz
- Color count controls output video quality
- Color count preview to first and last frame preview mode
- Additional logging
- `DuplicateRecordFields` to the GUI (GHC 8 only)
- GUI preview resets
- File size icon
- Color count icon
- FPS icon

#### Changed

- `getVideoDurationInSeconds` and `getVideoWidthAndHeight` to `getPlayableMetadata`
- The width size is the size of the output after cropping instead of before
- Uses the longer duration time instead of the shorter stream time
- Switched from floats and doubles to just doubles
- Adjust start and duration time steps based on file duration
- Start and duration time spin buttons now show three decimal places
- GUI preview function arguments to records
- Fix crash when trying to load a directory
- File icon
- Start icon
- End icon

#### Removed

- The quality setting
- Label selection focus on the main toggle buttons
- `qualityFromString`
- `getVideoAverageFrameRateInSeconds`
- `qualityAndFrameRateToGifSettings`
- `qualityAndFrameRateToDelay`
- `qualityAndFrameRateToFrameRate`
- `safeFrameRate`
- `defaultFrameRate`

### 4.0.0.0

#### Added

- Multiple dynamic text overlays
- Text overlay YAML file option `-t` to CLI
- Text fill and outline color configuration
- Text start and duration time configuration
- Text origin, x translation, and y translation configuration
- Text overlay preview to GUI
- Text left and top placement entries to GUI
- Text rotation configuration
- Text outline size configuration
- Outline and fill color selectors to GUI
- Pattern to GUI crop preview
- `textOverlayOriginFromString` to library API
- `qualityFromString` to library API
- `TextOverlays` to library API
- `TextOverlay` to library API
- `TextOverlayOrigin` to library API
- `Quality` to library API
- Text overlay validation
- Time slices and video position display custom widget
- Video position clock
- Pause button for video preview
- A complete theme
- An icon set

#### Changed

- Quality percent to quality nominal
- CLI Logo
- CLI help information
- GUI shows only file selection, info, and status on start up
- GUI crop preview color
- GUI preview size
- GUI icon size
- GUI first and last frame preview draw area to match the image size
- GUI takes the video URI from the inVideoPropertiesRef instead of the inFileChooserDialog during save
- Save as video bypasses GIF creation and goes straight to video creation
- Video output configuration

#### Removed

- CLI Icon

### 3.0.0.2

#### Added

- Styling to the about dialog
- Added label to the file chooser dialog
- Custom icon for the about button
- System capabilities check
- UI feedback based on system capabilities check

#### Changed

- About dialog logo
- Window icon
- Level bars to progress bars due to minimum fill size bug
- Do not annotate if top and bottom text is empty

#### Removed

-

### 3.0.0.1

#### Added

- Added project website under docs

#### Changed

- Changed docs to haddock
- Increased base optimization with fuzz and map
- Increased default frame rate to 15
- Set tighter bounds on number of colors
- Set tighter bounds on quality percent clamp
- Query stream for duration and then container for duration
- Fixed empty file name issue

#### Removed

-

### 3.0.0.0

#### Added

- Added a video preview using GStreamer
- Added start time and end time indicators to UI
- Added a save as video option to both the GUI and CLI
- Added a crop mode to the UI
- Added crop parameters to the CLI
- Added the following to the library interface
    - `getOutputFileWithExtension`
    - `getVideoWidthAndHeight`
    - `defaultFontChoice`
- Added snap distribution

#### Changed

- Reconfigured the UI to show the preview off to the right along with the start and duration controls
- Changed the image first and last frame preview to be the fallback if GStreamer errors
- Changed text entries to spin buttons for better error checking
    - The spin buttons provide better feedback about the min and max settings
- Moved the GUI preview code into a separate module
- Locked the GHC version to avoid build issues concerning haskell-gi and GHC 8.2.X
- Changed the logo and icon

#### Removed

-

### 2.3.0.0

#### Added

- Support for GTK 3.10
- Logo to about dialog
- `findOrCreateTemporaryDirectory` to Gifcurry library API

#### Changed

- Switched icon from ICO to PNG
- Use XdgCache location for cross compatible way of storing temporary files

#### Removed

- ICO files

### 2.2.0.0

#### Added

- Loading a video now populates the start and duration fields
    - Duration is the length of the entire video in seconds
- Confirmation dialog if duration is >= 10 seconds
- `getVideoDurationInSeconds` to the library public API
- Error checking
- File path compatibility
- About dialog
- GNU Make targets for Arch Linux
- `stack.yaml`
- CHANGELOG.md

#### Changed

- Icon file
- Switched from gtk2hs to haskell-gi
- Switched from Cabal to Haskell stack
- Code clean up
- README install and run instructions
- README graphics
- GUI widget IDs
- Fixed the ImageMagick null font issue
    - ImageMagick assumes `~/.magick/type.xml` exists
    - Instead of `default`, it searches for the first match to `sans` if no font is specified
- Temporary file directory names where frames and GIFs are built

#### Removed

- Old icon from cabal file
- Altered icon from GUI file

### 2.1.1.0

#### Added

-

#### Changed

- Fixed cabal file
- Altered optimization
- Updated to work with GHC 8.0.2
- Updated README to work with Hackage

#### Removed

-

### 2.1.0.0

#### Added

- Font selection capabilities
- Font selection to the GUI
- A fontChoice parameter to the CLI
- A makefile

#### Changed

- Updated the logo
- Updated the icon
- Fixed font scaling
- Updated dependencies for macOS Sierra
- Updated README
- Reorganized project structure
- Fixed compiler warnings

#### Removed

-

### 2.0.0.2

#### Added

-

#### Changed

- Fixed a bug where the first and last frame previews were not updating

#### Removed

-

### 2.0.0.1

#### Added

- `postGUIAsync`

#### Changed

- Bumped dependencies up (mainly GHC 8.0)
- [Char] to String

#### Removed

-

### 2.0.0.0

#### Added

- Type signatures
- More error checking to the GUI

#### Changed

- Refactored code
- Made CLI more flexible concerning input options

#### Removed

-

### 0.1.1.0

#### Added

- First and last frame preview
- Further file exists error checking

#### Changed

- Disabled GUI re-sizing
- Simplified GUI

#### Removed

-

### 0.1.0.6

#### Added

-

#### Changed

- Updated GIF open method to work for Mac OS X

#### Removed

-

### 0.1.0.5

#### Added

-

#### Changed

- Downgraded GTK requirement from 3.16 to 3.10

#### Removed

-

### 0.1.0.4

#### Added

-

#### Changed

-

#### Removed

-

### 0.1.0.3

#### Added

-

#### Changed

-

#### Removed

-

### 0.1.0.2

#### Added

-

#### Changed

-

#### Removed

-

### 0.1.0.1

#### Added

-

#### Changed

-

#### Removed

-

### 0.1.0.0

#### Added

-

#### Changed

-

#### Removed

-

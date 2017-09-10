# [Gifcurry](https://github.com/lettier/gifcurry)

## Changelog

### Version

2.2.0.0

#### Downstreams

- [Hackage](https://hackage.haskell.org/package/Gifcurry-2.2.0.0)
- [Docker](https://hub.docker.com/r/lettier/gifcurry/)
- [Arch Linux AUR](https://aur.archlinux.org/packages/gifcurry)

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

-------------------------------------------------------------------------------

### Version

2.1.1.0

#### Downstreams

- [Hackage](https://hackage.haskell.org/package/Gifcurry-2.1.1.0)

#### Added

-

#### Changed

- Fixed cabal file
- Altered optimization
- Updated to work with GHC 8.0.2
- Updated README to work with Hackage

#### Removed

-

-------------------------------------------------------------------------------

### Version

2.1.0.0

#### Downstreams

- [Hackage](https://hackage.haskell.org/package/Gifcurry-2.1.0.0)

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

-------------------------------------------------------------------------------

### Version

2.0.0.2

#### Downstreams

- [Hackage](https://hackage.haskell.org/package/Gifcurry-2.0.0.2)

#### Added

-

#### Changed

- Fixed a bug where the first and last frame previews were not updating

#### Removed

-

-------------------------------------------------------------------------------

### Version

2.0.0.1

#### Downstreams

- [Hackage](https://hackage.haskell.org/package/Gifcurry-2.0.0.1)

#### Added

- `postGUIAsync`

#### Changed

- Bumped dependencies up (mainly GHC 8.0)
- [Char] to String

#### Removed

-

-------------------------------------------------------------------------------

### Version

2.0.0.0

#### Downstreams

- [Hackage](https://hackage.haskell.org/package/Gifcurry-2.0.0.0)

#### Added

- Type signatures
- More error checking to the GUI

#### Changed

- Refactored code
- Made CLI more flexible concerning input options

#### Removed

-

-------------------------------------------------------------------------------

### Version

0.1.1.0

#### Downstreams

- [Hackage](https://hackage.haskell.org/package/Gifcurry-0.1.1.0)

#### Added

- First and last frame preview
- Further file exists error checking

#### Changed

- Disabled GUI re-sizing
- Simplified GUI

#### Removed

-

-------------------------------------------------------------------------------

### Version

0.1.0.6

#### Downstreams

-

#### Added

-

#### Changed

- Updated GIF open method to work for Mac OS X

#### Removed

-

-------------------------------------------------------------------------------

### Version

0.1.0.5

#### Downstreams

- [Hackage](https://hackage.haskell.org/package/Gifcurry-0.1.0.5)

#### Added

-

#### Changed

- Downgraded GTK requirement from 3.16 to 3.10

#### Removed

-

-------------------------------------------------------------------------------

### Version

0.1.0.4

#### Downstreams

- [Hackage](https://hackage.haskell.org/package/Gifcurry-0.1.0.4)

#### Added

-

#### Changed

-

#### Removed

-

-------------------------------------------------------------------------------

### Version

0.1.0.3

#### Downstreams

- [Hackage](https://hackage.haskell.org/package/Gifcurry-0.1.0.3)

#### Added

-

#### Changed

-

#### Removed

-

-------------------------------------------------------------------------------

### Version

0.1.0.2

#### Downstreams

- [Hackage](https://hackage.haskell.org/package/Gifcurry-0.1.0.2)

#### Added

-

#### Changed

-

#### Removed

-

-------------------------------------------------------------------------------

### Version

0.1.0.1

#### Downstreams

- [Hackage](https://hackage.haskell.org/package/Gifcurry-0.1.0.1)

#### Added

-

#### Changed

-

#### Removed

-

-------------------------------------------------------------------------------

### Version

0.1.0.0

#### Downstreams

- [Hackage](https://hackage.haskell.org/package/Gifcurry-0.1.0.0)

#### Added

-

#### Changed

-

#### Removed

-

-------------------------------------------------------------------------------

## License

See [LICENSE](LICENSE).

## Copyright

(C) 2017 David Lettier  
[lettier.com](http://www.lettier.com/)

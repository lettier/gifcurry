{-
  Gifcurry
  (C) 2016 David Lettier
  lettier.com
-}

{-# LANGUAGE
    OverloadedStrings
  , NamedFieldPuns
#-}

-- | Produces GIFs using FFmpeg and ImageMagick.
-- The main function is 'gif'.
module Gifcurry
  ( GifParams(..)
  , PlayableMetadata(..)
  , TextOverlay(..)
  , TextOverlayOrigin(..)
  , versionNumber
  , findOrCreateTemporaryDirectory
  , defaultTextOverlay
  , defaultGifParams
  , validateGifParams
  , getPlayableMetadata
  , getOutputFileWithExtension
  , textOverlayOriginFromString
  , saveTextOverlaysToFile
  , getRgba
  , convertFileToTextOverlays
  , parseVersionNumber
  , createGif
  )
where

import System.Process
import System.IO.Temp
import System.Directory
import System.FilePath
import qualified System.FilePath.Find as SFF
import Control.Exception
import Control.Monad
import Control.Applicative ((<|>))
import Text.Read
import Text.ParserCombinators.ReadP
import Text.Printf
import Data.List (sortBy)
import Data.Maybe
import Data.Char
import Data.Text
import Data.Either
import Data.Yaml
import qualified Data.ByteString.Char8 as DBC

-- | The data type record required by 'gif'.
data GifParams =
  GifParams
    { inputFile    :: String
    , outputFile   :: String
    , saveAsVideo  :: Bool
    , startTime    :: Double
    , endTime      :: Double
    , width        :: Int
    , fps          :: Int
    , colorCount   :: Int
    , dither       :: Bool
    , textOverlays :: [TextOverlay]
    , leftCrop     :: Double
    , rightCrop    :: Double
    , topCrop      :: Double
    , bottomCrop   :: Double
    }
  deriving (Show, Read)

-- | The data type that holds the needed information to render text on top of the GIF.
data TextOverlay =
  TextOverlay
    { textOverlayText         :: String
    , textOverlayFontFamily   :: String
    , textOverlayFontStyle    :: String
    , textOverlayFontStretch  :: String
    , textOverlayFontWeight   :: Int
    , textOverlayFontSize     :: Int
    , textOverlayOrigin       :: TextOverlayOrigin
    , textOverlayXTranslation :: Double
    , textOverlayYTranslation :: Double
    , textOverlayRotation     :: Int
    , textOverlayStartTime    :: Double
    , textOverlayEndTime      :: Double
    , textOverlayOutlineSize  :: Int
    , textOverlayOutlineColor :: String
    , textOverlayFillColor    :: String
    }
  deriving (Show, Read)

-- | The data type that holds the probed results of a playable media file.
data PlayableMetadata =
  PlayableMetadata
    { playableMetadataFormats  :: [String]
    , playableMetadataDuration :: Double
    , playableMetadataFps      :: Double
    , playableMetadataWidth    :: Double
    , playableMetadataHeight   :: Double
    }

-- | The starting point for a text overlay.
data TextOverlayOrigin =
    TextOverlayOriginNorthWest
  | TextOverlayOriginNorth
  | TextOverlayOriginNorthEast
  | TextOverlayOriginWest
  | TextOverlayOriginCenter
  | TextOverlayOriginEast
  | TextOverlayOriginSouthWest
  | TextOverlayOriginSouth
  | TextOverlayOriginSouthEast
  deriving (Read, Eq)

instance FromJSON TextOverlay where
  parseJSON =
    withObject
      "TextOverlay"
      $ \ obj -> do
      text         <- obj .:  "text"
      fontFamily   <- obj .:? "fontFamily"   .!= "Sans"
      fontStyle    <- obj .:? "fontStyle"    .!= "Normal"
      fontStretch  <- obj .:? "fontStretch"  .!= "Normal"
      fontWeight   <- obj .:? "fontWeight"   .!= 400
      fontSize     <- obj .:? "fontSize"     .!= 30
      origin       <- obj .:? "origin"       .!= "Center"
      xTranslation <- obj .:? "xTranslation" .!= 0.0
      yTranslation <- obj .:? "yTranslation" .!= 0.0
      rotation     <- obj .:? "rotation"     .!= 0
      startTime    <- obj .:  "startTime"
      endTime      <- obj .:  "endTime"
      outlineSize  <- obj .:? "outlineSize"  .!= textOverlayOutlineSizeMax
      outlineColor <- obj .:? "outlineColor" .!= "rgba(0,0,0,1.0)"
      fillColor    <- obj .:? "fillColor"    .!= "rgba(255,255,255,1.0)"

      let fontStyle'
            | stringsEqual fontStyle "Any"                  = fontStyle
            | stringsEqual fontStyle "Italic"               = fontStyle
            | stringsEqual fontStyle "Normal"               = fontStyle
            | stringsEqual fontStyle "Oblique"              = fontStyle
            | otherwise                                     = "Normal"
      let fontStretch'
            | stringsEqual fontStretch "Any"                = fontStretch
            | stringsEqual fontStretch "Condensed"          = fontStretch
            | stringsEqual fontStretch "Expanded"           = fontStretch
            | stringsEqual fontStretch "ExtraCondensed"     = fontStretch
            | stringsEqual fontStretch "ExtraExpanded"      = fontStretch
            | stringsEqual fontStretch "Normal"             = fontStretch
            | stringsEqual fontStretch "SemiCondensed"      = fontStretch
            | stringsEqual fontStretch "SemiExpanded"       = fontStretch
            | stringsEqual fontStretch "UltraCondensed"     = fontStretch
            | stringsEqual fontStretch "UltraExpanded"      = fontStretch
            | otherwise                                     = "Normal"
      let fontWeight'
            | fontWeight < 1                                = 1
            | otherwise                                     = fontWeight
      let fontSize'
            | fontSize < 1                                  = 1
            | otherwise                                     = fontSize
      let origin'                                           = fromMaybe TextOverlayOriginCenter
                                                                $ textOverlayOriginFromString origin
      let xTranslation'
            |   origin' == TextOverlayOriginNorthWest
            &&  xTranslation >=  0.0 && xTranslation <= 1.0 = xTranslation
            |   origin' == TextOverlayOriginNorth
            &&  xTranslation >= -0.5 && xTranslation <= 0.5 = xTranslation
            |   origin' == TextOverlayOriginNorthEast
            &&  xTranslation >= -1.0 && xTranslation <= 0.0 = xTranslation
            |   origin' == TextOverlayOriginEast
            &&  xTranslation >= -1.0 && xTranslation <= 0.0 = xTranslation
            |   origin' == TextOverlayOriginSouthEast
            &&  xTranslation >= -1.0 && xTranslation <= 0.0 = xTranslation
            |   origin' == TextOverlayOriginSouth
            &&  xTranslation >= -0.5 && xTranslation <= 0.5 = xTranslation
            |   origin' == TextOverlayOriginSouthWest
            &&  xTranslation >=  0.0 && xTranslation <= 1.0 = xTranslation
            |   origin' == TextOverlayOriginWest
            &&  xTranslation >=  0.0 && xTranslation <= 1.0 = xTranslation
            |   origin' == TextOverlayOriginCenter
            &&  xTranslation >= -0.5 && xTranslation <= 0.5 = xTranslation
            |   otherwise                                   = 0
      let yTranslation'
            |   origin' == TextOverlayOriginNorthWest
            &&  yTranslation >=  0.0 && yTranslation <= 1.0 = yTranslation
            |   origin' == TextOverlayOriginNorth
            &&  yTranslation >=  0.0 && yTranslation <= 1.0 = yTranslation
            |   origin' == TextOverlayOriginNorthEast
            &&  yTranslation >=  0.0 && yTranslation <= 1.0 = yTranslation
            |   origin' == TextOverlayOriginEast
            &&  yTranslation >= -0.5 && yTranslation <= 0.5 = yTranslation
            |   origin' == TextOverlayOriginSouthEast
            &&  yTranslation >= -1.0 && yTranslation <= 0.0 = yTranslation
            |   origin' == TextOverlayOriginSouth
            &&  yTranslation >= -1.0 && yTranslation <= 0.0 = yTranslation
            |   origin' == TextOverlayOriginSouthWest
            &&  yTranslation >= -1.0 && yTranslation <= 0.0 = yTranslation
            |   origin' == TextOverlayOriginWest
            &&  yTranslation >= -0.5 && yTranslation <= 0.5 = yTranslation
            |   origin' == TextOverlayOriginCenter
            &&  yTranslation >= -0.5 && yTranslation <= 0.5 = yTranslation
            |   otherwise                                   = 0
      let rotation'
            | rotation < 0                                  = 360 - mod (abs rotation) 360
            | rotation > 360                                = mod rotation 360
            | otherwise                                     = rotation
      let startTime'
            | startTime < 0                                 = 0
            | otherwise                                     = startTime
      let endTime'
            | endTime < 0                                   = 0
            | otherwise                                     = endTime
      let startTime''
            | startTime' > endTime'                         = endTime'
            | otherwise                                     = startTime'
      let outlineSize'
            | outlineSize < 0                               = 0
            | outlineSize > textOverlayOutlineSizeMax       = textOverlayOutlineSizeMax
            | otherwise                                     = outlineSize
      let outlineColor'                                     = safeRgbaString outlineColor (  0,   0,   0, 1.0)
      let fillColor'                                        = safeRgbaString fillColor    (255, 255, 255, 1.0)

      return
        TextOverlay
          { textOverlayText         = text
          , textOverlayFontFamily   = fontFamily
          , textOverlayFontStyle    = fontStyle'
          , textOverlayFontStretch  = fontStretch'
          , textOverlayFontWeight   = fontWeight'
          , textOverlayFontSize     = fontSize'
          , textOverlayOrigin       = origin'
          , textOverlayXTranslation = xTranslation'
          , textOverlayYTranslation = yTranslation'
          , textOverlayRotation     = rotation'
          , textOverlayStartTime    = startTime''
          , textOverlayEndTime      = endTime'
          , textOverlayOutlineSize  = outlineSize'
          , textOverlayOutlineColor = outlineColor'
          , textOverlayFillColor    = fillColor'
          }

instance ToJSON TextOverlay where
  toJSON
    TextOverlay
      { textOverlayText
      , textOverlayFontFamily
      , textOverlayFontStyle
      , textOverlayFontStretch
      , textOverlayFontWeight
      , textOverlayFontSize
      , textOverlayOrigin
      , textOverlayXTranslation
      , textOverlayYTranslation
      , textOverlayRotation
      , textOverlayStartTime
      , textOverlayEndTime
      , textOverlayOutlineSize
      , textOverlayOutlineColor
      , textOverlayFillColor
      }
    =
    object
      [ "text"         .= textOverlayText
      , "fontFamily"   .= textOverlayFontFamily
      , "fontStyle"    .= textOverlayFontStyle
      , "fontStretch"  .= textOverlayFontStretch
      , "fontWeight"   .= textOverlayFontWeight
      , "fontSize"     .= textOverlayFontSize
      , "origin"       .= show textOverlayOrigin
      , "xTranslation" .= textOverlayXTranslation
      , "yTranslation" .= textOverlayYTranslation
      , "rotation"     .= textOverlayRotation
      , "startTime"    .= textOverlayStartTime
      , "endTime"      .= textOverlayEndTime
      , "outlineSize"  .= textOverlayOutlineSize
      , "outlineColor" .= textOverlayOutlineColor
      , "fillColor"    .= textOverlayFillColor
      ]

instance Show TextOverlayOrigin where
  show TextOverlayOriginNorthWest = "NorthWest"
  show TextOverlayOriginNorth     = "North"
  show TextOverlayOriginNorthEast = "NorthEast"
  show TextOverlayOriginWest      = "West"
  show TextOverlayOriginCenter    = "Center"
  show TextOverlayOriginEast      = "East"
  show TextOverlayOriginSouthWest = "SouthWest"
  show TextOverlayOriginSouth     = "South"
  show TextOverlayOriginSouthEast = "SouthEast"

data Timestamp =
  Timestamp
    { hours        :: Int
    , minutes      :: Int
    , seconds      :: Int
    , milliseconds :: Int
    }
  deriving (Show, Read)

data SrtSubtitleCoordinates =
  SrtSubtitleCoordinates
    { x1 :: Int
    , x2 :: Int
    , y1 :: Int
    , y2 :: Int
    }
  deriving (Show, Read)

data SrtSubtitle =
  SrtSubtitle
    { srtSubtitleIndex       :: Int
    , srtSubtitleStart       :: Timestamp
    , srtSubtitleEnd         :: Timestamp
    , srtSubtitleCoordinates :: Maybe SrtSubtitleCoordinates
    , srtSubtitleText        :: [String]
    }
  deriving (Show, Read)

-- | The version number.
versionNumber :: String
versionNumber = "6.0.0.0"

-- | Specifies the default field values for 'TextOverlay'.
defaultTextOverlay
  ::  TextOverlay
defaultTextOverlay =
  TextOverlay
    { textOverlayText         = ""
    , textOverlayFontFamily   = "Sans"
    , textOverlayFontStyle    = "Normal"
    , textOverlayFontStretch  = "Normal"
    , textOverlayFontWeight   = 400
    , textOverlayFontSize     = 30
    , textOverlayOrigin       = TextOverlayOriginCenter
    , textOverlayXTranslation = 0.0
    , textOverlayYTranslation = 0.0
    , textOverlayRotation     = 0
    , textOverlayStartTime    = 0
    , textOverlayEndTime      = 0
    , textOverlayOutlineSize  = textOverlayOutlineSizeMax
    , textOverlayOutlineColor = "rgba(0,0,0,1.0)"
    , textOverlayFillColor    = "rgba(255,255,255,1.0)"
    }

-- | Specifies the default field values for 'GifParams'.
defaultGifParams
  ::  GifParams
defaultGifParams =
  GifParams
    { inputFile      = ""
    , outputFile     = ""
    , saveAsVideo    = False
    , startTime      = 0.0
    , endTime        = 1.0
    , width          = 500
    , fps            = 24
    , colorCount     = 256
    , dither         = False
    , textOverlays   = []
    , leftCrop       = 0.0
    , rightCrop      = 0.0
    , topCrop        = 0.0
    , bottomCrop     = 0.0
    }

-- | Creates a GIF passed on the given 'GifParams'.
-- Returns either left an error or right the file path to the created GIF.
createGif
  ::  GifParams
  ->  IO (Either IOError String)
createGif
  gifParams@GifParams
    { inputFile
    , width
    , saveAsVideo
    , startTime
    , textOverlays
    , fps
    , leftCrop
    , rightCrop
    }
  = do
  printGifParams gifParams
  paramsValid           <- validateGifParams   gifParams
  maybePlayableMetadata <- getPlayableMetadata inputFile
  case (paramsValid, maybePlayableMetadata) of
    (False, _)   -> return $ Left $ userError "Invalid parameters."
    (_, Nothing) -> return $ Left $ userError "Could not retrieve the playable metadata."
    (True, Just playableMetadata) -> do
      temporaryDirectory <- findOrCreateTemporaryDirectory
      withTempDirectory temporaryDirectory "gifcurry-frames" $
        \ tempDir ->
              handleFrameExtraction  tempDir playableMetadata
          >>= handleFrameAnnotations tempDir playableMetadata
          >>= handleFrameMerge       tempDir playableMetadata
  where
    handleFrameExtraction
      ::  String
      ->  PlayableMetadata
      ->  IO (Either IOError String)
    handleFrameExtraction tempDir _ = do
      result    <- extractFrames gifParams tempDir
      case result of
        Left x  -> do
          printError "Something went wrong with FFmpeg."
          return $ Left x
        Right _ -> return $ Right ""
    handleFrameAnnotations
      ::  String
      ->  PlayableMetadata
      ->  Either IOError String
      ->  IO (Either IOError String)
    handleFrameAnnotations
      tempDir
      PlayableMetadata
        { playableMetadataWidth
        , playableMetadataHeight
        }
      (Right _)
      | Prelude.null textOverlays = return $ Right ""
      | otherwise = do
        frameFilePaths <-
          SFF.find
            SFF.always
            (SFF.fileName SFF.~~? "*extracted-frames_*")
            tempDir
        let maybeFrameNumbers = getFrameNumbers frameFilePaths
        case maybeFrameNumbers of
          Just frameNumbers -> do
            fontFamilies <- getFontFamilies
            let frameSeconds =
                  Prelude.map
                    (\ x -> startTime + (fromIntegral x * (1.0 / fromIntegral fps)))
                    frameNumbers
            let frameFilePathsFrameSeconds = Prelude.zip frameFilePaths frameSeconds
            let width' = fromIntegral width / (1.0 - leftCrop - rightCrop)
            let (gifWidthNoCrop, gifHeightNoCrop) =
                  (     width'
                  ,     width'
                    * ( playableMetadataHeight
                      / playableMetadataWidth
                      )
                  )
            printInfo "Adding text."
            results <-
              mapM
              (\ (filePath, second) -> do
                let textOverlays' =
                      Prelude.foldl
                        (\ xs x ->
                          if      textOverlayStartTime x <= second
                              &&  textOverlayEndTime   x >= second
                            then xs ++ [x]
                            else xs
                        )
                        []
                        (Prelude.reverse textOverlays)
                annotateImage
                  gifParams
                  gifWidthNoCrop
                  gifHeightNoCrop
                  fontFamilies
                  filePath
                  textOverlays'
              )
              frameFilePathsFrameSeconds
            if Prelude.any isLeft results
              then
                case results of
                  (Left x:_) -> return $ Left x
                  _          -> return $ Left $ userError "Could not annotate the frames."
              else return $ Right ""
          Nothing -> do
            let errorString = "Could not find the frame numbers."
            printError errorString
            return $ Left $ userError errorString
    handleFrameAnnotations _ _ (Left x) = return $ Left x
    handleFrameMerge :: String -> PlayableMetadata -> Either IOError String -> IO (Either IOError String)
    handleFrameMerge tempDir _ (Right _) =
      if saveAsVideo
        then do
          result <- mergeFramesIntoVideo gifParams tempDir
          case result of
            Left x -> do
              printError "Something went wrong with FFmpeg."
              return $ Left x
            Right videoFilePath -> do
              printInfo "All done."
              return $ Right videoFilePath
        else do
          result <- mergeFramesIntoGif gifParams tempDir
          case result of
            Left x -> do
              printError "Something went wrong with ImageMagick."
              return $ Left x
            Right gifFilePath -> do
              printInfo "All done."
              return $ Right gifFilePath
    handleFrameMerge _ _ (Left x) = return $ Left x

-- | Convenience function that attempts to turn a string into a 'TextOverlayOrigin'.
-- @
--    textOverlayOriginFromString "  cEntEr " -- Just TextOverlayOriginCenter
--    textOverlayOriginFromString "test"      -- Nothing
-- @
textOverlayOriginFromString
  ::  String
  ->  Maybe Gifcurry.TextOverlayOrigin
textOverlayOriginFromString origin =
  textOverlayOriginFromString' $
    stripAndLowerString origin
  where
    textOverlayOriginFromString' :: String -> Maybe TextOverlayOrigin
    textOverlayOriginFromString' "northwest" = Just TextOverlayOriginNorthWest
    textOverlayOriginFromString' "north"     = Just TextOverlayOriginNorth
    textOverlayOriginFromString' "northeast" = Just TextOverlayOriginNorthEast
    textOverlayOriginFromString' "west"      = Just TextOverlayOriginWest
    textOverlayOriginFromString' "center"    = Just TextOverlayOriginCenter
    textOverlayOriginFromString' "east"      = Just TextOverlayOriginEast
    textOverlayOriginFromString' "southwest" = Just TextOverlayOriginSouthWest
    textOverlayOriginFromString' "south"     = Just TextOverlayOriginSouth
    textOverlayOriginFromString' "southeast" = Just TextOverlayOriginSouthEast
    textOverlayOriginFromString' _           = Nothing

-- | Outputs `True` or `False` if 'GifParams' are valid.
validateGifParams
  ::  GifParams
  ->  IO Bool
validateGifParams
  GifParams
    { inputFile
    , outputFile
    , startTime
    , endTime
    , width
    , fps
    , colorCount
    , leftCrop
    , rightCrop
    , topCrop
    , bottomCrop
    , textOverlays
    }
  = do
  inputFileExists <-
    case Prelude.length inputFile of
      0 -> return False
      _ -> doesFileExist inputFile
  let width'                      = fromIntegral width :: Double
  let outputFileValid             = not $ Data.Text.null $ Data.Text.strip $ Data.Text.pack outputFile
  let startTimeValid              = startTime >= 0.0
  let endTimeValid                = endTime > 0.0 && endTime > startTime
  let widthValid                  = width >= 1
  let fpsValid                    = fps >= 15 && fps <= 60
  let colorCountValid             = colorCount >= 1 && colorCount <= 256
  let leftCropValid               = cropValid leftCrop
  let rightCropValid              = cropValid rightCrop
  let topCropValid                = cropValid topCrop
  let bottomCropValid             = cropValid bottomCrop
  let leftRightCropValid          = cropValid (leftCrop + rightCrop)
  let topBottomCropValid          = cropValid (topCrop + bottomCrop)
  let widthLeftRightCropSizeValid =
        (width' - (width' * leftCrop) - (width' * rightCrop)) >= 1.0

  unless inputFileExists              $ printError   "Input video file does not exist."
  unless outputFileValid              $ printInvalid "Output file"
  unless startTimeValid               $ printInvalid "Start time"
  unless endTimeValid                 $ printInvalid "End time"
  unless widthValid                   $ printInvalid "Width"
  unless fpsValid                     $ printInvalid "FPS"
  unless colorCountValid              $ printInvalid "Color count"
  unless leftCropValid                $ printInvalid "Left crop"
  unless rightCropValid               $ printInvalid "Right crop"
  unless topCropValid                 $ printInvalid "Top crop"
  unless bottomCropValid              $ printInvalid "Bottom crop"
  unless leftRightCropValid           $ printInvalid "Left and right crop"
  unless topBottomCropValid           $ printInvalid "Top and bottom crop"
  unless widthLeftRightCropSizeValid  $ printError   "Width is too small with left and right crop."

  textOverlaysValid <- Prelude.and <$> mapM validateTextOverlay textOverlays

  unless textOverlaysValid            $ printError   "Text overlays are invalid."

  return
    $  inputFileExists
    && outputFileValid
    && startTimeValid
    && endTimeValid
    && widthValid
    && fpsValid
    && colorCountValid
    && leftCropValid
    && rightCropValid
    && topCropValid
    && bottomCropValid
    && widthLeftRightCropSizeValid
    && textOverlaysValid
  where
    cropValid :: Double -> Bool
    cropValid c = c >= 0.0 && c < 1.0
    printInvalid :: String -> IO ()
    printInvalid s = printError $ s ++ " is invalid."
    validateTextOverlay :: TextOverlay -> IO Bool
    validateTextOverlay
      TextOverlay
        { textOverlayText
        , textOverlayFontFamily
        , textOverlayFontStyle
        , textOverlayFontStretch
        , textOverlayFontWeight
        , textOverlayFontSize
        , textOverlayOrigin
        , textOverlayXTranslation
        , textOverlayYTranslation
        , textOverlayRotation
        , textOverlayStartTime
        , textOverlayEndTime
        , textOverlayOutlineSize
        , textOverlayOutlineColor
        , textOverlayFillColor
        }
      = do
      let textOverlayTextValid         = (not . Prelude.null) textOverlayText
      let textOverlayFontFamilyValid   = (not . Prelude.null) textOverlayFontFamily
      let textOverlayFontStyleValid
            | stringsEqual textOverlayFontStyle "Any"     = True
            | stringsEqual textOverlayFontStyle "Italic"  = True
            | stringsEqual textOverlayFontStyle "Normal"  = True
            | stringsEqual textOverlayFontStyle "Oblique" = True
            | otherwise                        = False
      let textOverlayFontStretchValid
            | stringsEqual textOverlayFontStretch "Any"            = True
            | stringsEqual textOverlayFontStretch "Condensed"      = True
            | stringsEqual textOverlayFontStretch "Expanded"       = True
            | stringsEqual textOverlayFontStretch "ExtraCondensed" = True
            | stringsEqual textOverlayFontStretch "ExtraExpanded"  = True
            | stringsEqual textOverlayFontStretch "Normal"         = True
            | stringsEqual textOverlayFontStretch "SemiCondensed"  = True
            | stringsEqual textOverlayFontStretch "SemiExpanded"   = True
            | stringsEqual textOverlayFontStretch "UltraCondensed" = True
            | stringsEqual textOverlayFontStretch "UltraExpanded"  = True
            | otherwise                                            = False
      let textOverlayFontWeightValid
            | textOverlayFontWeight > 0 = True
            | otherwise                 = False
      let textOverlayFontSizeValid
            | textOverlayFontSize > 0   = True
            | otherwise                 = False
      let textOverlayXTranslationValid
            |   textOverlayOrigin == TextOverlayOriginNorthWest
            &&  textOverlayXTranslation >=  0.0 && textOverlayXTranslation <= 1.0 = True
            |   textOverlayOrigin == TextOverlayOriginNorth
            &&  textOverlayXTranslation >= -0.5 && textOverlayXTranslation <= 0.5 = True
            |   textOverlayOrigin == TextOverlayOriginNorthEast
            &&  textOverlayXTranslation >= -1.0 && textOverlayXTranslation <= 0.0 = True
            |   textOverlayOrigin == TextOverlayOriginEast
            &&  textOverlayXTranslation >= -1.0 && textOverlayXTranslation <= 0.0 = True
            |   textOverlayOrigin == TextOverlayOriginSouthEast
            &&  textOverlayXTranslation >= -1.0 && textOverlayXTranslation <= 0.0 = True
            |   textOverlayOrigin == TextOverlayOriginSouth
            &&  textOverlayXTranslation >= -0.5 && textOverlayXTranslation <= 0.5 = True
            |   textOverlayOrigin == TextOverlayOriginSouthWest
            &&  textOverlayXTranslation >=  0.0 && textOverlayXTranslation <= 1.0 = True
            |   textOverlayOrigin == TextOverlayOriginWest
            &&  textOverlayXTranslation >=  0.0 && textOverlayXTranslation <= 1.0 = True
            |   textOverlayOrigin == TextOverlayOriginCenter
            &&  textOverlayXTranslation >= -0.5 && textOverlayXTranslation <= 0.5 = True
            |   otherwise                                                         = False
      let textOverlayYTranslationValid
            |   textOverlayOrigin == TextOverlayOriginNorthWest
            &&  textOverlayYTranslation >=  0.0 && textOverlayYTranslation <= 1.0 = True
            |   textOverlayOrigin == TextOverlayOriginNorth
            &&  textOverlayYTranslation >=  0.0 && textOverlayYTranslation <= 1.0 = True
            |   textOverlayOrigin == TextOverlayOriginNorthEast
            &&  textOverlayYTranslation >=  0.0 && textOverlayYTranslation <= 1.0 = True
            |   textOverlayOrigin == TextOverlayOriginEast
            &&  textOverlayYTranslation >= -0.5 && textOverlayYTranslation <= 0.5 = True
            |   textOverlayOrigin == TextOverlayOriginSouthEast
            &&  textOverlayYTranslation >= -1.0 && textOverlayYTranslation <= 0.0 = True
            |   textOverlayOrigin == TextOverlayOriginSouth
            &&  textOverlayYTranslation >= -1.0 && textOverlayYTranslation <= 0.0 = True
            |   textOverlayOrigin == TextOverlayOriginSouthWest
            &&  textOverlayYTranslation >= -1.0 && textOverlayYTranslation <= 0.0 = True
            |   textOverlayOrigin == TextOverlayOriginWest
            &&  textOverlayYTranslation >= -0.5 && textOverlayYTranslation <= 0.5 = True
            |   textOverlayOrigin == TextOverlayOriginCenter
            &&  textOverlayYTranslation >= -0.5 && textOverlayYTranslation <= 0.5 = True
            |   otherwise                                                         = False
      let textOverlayRotationValid
            | textOverlayRotation < 0   = False
            | textOverlayRotation > 360 = False
            | otherwise      = True
      let textOverlayStartTimeValid
            | textOverlayStartTime < 0                   = False
            | textOverlayStartTime >= textOverlayEndTime = False
            | otherwise                                  = True
      let textOverlayEndTimeValid
            | textOverlayEndTime <= 0                    = False
            | textOverlayEndTime <= textOverlayStartTime = False
            | otherwise                                  = True
      let textOverlayOutlineSizeValid
            | textOverlayOutlineSize < 0                         = False
            | textOverlayOutlineSize > textOverlayOutlineSizeMax = False
            | otherwise                                          = True
      let textOverlayOutlineColorValid = colorValid textOverlayOutlineColor
      let textOverlayFillColorValid    = colorValid textOverlayFillColor

      let valid =
              textOverlayTextValid
            && textOverlayFontFamilyValid
            && textOverlayFontStyleValid
            && textOverlayFontStretchValid
            && textOverlayFontWeightValid
            && textOverlayFontSizeValid
            && textOverlayXTranslationValid
            && textOverlayYTranslationValid
            && textOverlayRotationValid
            && textOverlayStartTimeValid
            && textOverlayEndTimeValid
            && textOverlayOutlineSizeValid
            && textOverlayOutlineColorValid
            && textOverlayFillColorValid

      unless valid
        $ printError
          $ Prelude.concat
            [ "Text overlay \""
            , textOverlayText
            , "\" is invalid."
            ]
      unless textOverlayTextValid
        $ printInvalid "Text overlay text"
      unless textOverlayFontFamilyValid
        $ printInvalid "Text overlay font family"
      unless textOverlayFontStyleValid
        $ printError "Text overlay font style is invalid. It can be Any, Italic, Oblique, and Normal."
      unless textOverlayFontStretchValid
        $ printError
          $  Prelude.unwords
            [ "Text overlay font stretch is invalid. It can be "
            , "Any"
            , ","
            , "Condensed"
            , ","
            , "Expanded"
            , ","
            , "ExtraCondensed"
            , ","
            , "ExtraExpanded"
            , ","
            , "Normal"
            , ","
            , "SemiCondensed"
            , ","
            , "SemiExpanded"
            , ","
            , "UltraCondensed"
            , ","
            , "and"
            , "UltraExpanded"
            , "."
            ]
      unless textOverlayFontWeightValid
        $ printInvalid "Text overlay font weight"
      unless textOverlayFontSizeValid
        $ printInvalid "Text overlay font size"
      unless textOverlayXTranslationValid
        $ printInvalid "Text overlay x translation"
      unless textOverlayYTranslationValid
        $ printInvalid "Text overlay y translation"
      unless textOverlayRotationValid
        $ printInvalid "Text overlay rotation"
      unless textOverlayStartTimeValid
        $ printInvalid "Text overlay start time"
      unless textOverlayEndTimeValid
        $ printInvalid "Text overlay end time"
      unless textOverlayOutlineSizeValid
        $ printInvalid "Text overlay outline size"
      unless textOverlayOutlineColorValid
        $ printInvalid "Text overlay outline color"
      unless textOverlayFillColorValid
        $ printInvalid "Text overlay fill color"

      return valid
      where
        colorValid :: String -> Bool
        colorValid s =
          case getRgba s of
            Nothing           -> False
            Just (r, g, b, a) ->
                  rgbValid r
              &&  rgbValid g
              &&  rgbValid b
              &&  a >= 0 && a <= 1
          where
            rgbValid :: Int -> Bool
            rgbValid x
              | x < 0     = False
              | x > 255   = False
              | otherwise = True

-- | Returns the metadata for a playable media file if possible.
getPlayableMetadata
  ::  String
  ->  IO (Maybe PlayableMetadata)
getPlayableMetadata
  inputFile
  = do
  inputFileExists <- doesFileExist inputFile
  eitherResult    <- if inputFileExists
                      then tryFfprobe params
                      else return (Left (userError "File not found."))
  case eitherResult of
    Left  _      -> return Nothing
    Right result -> do
      let entries =
            Prelude.filter
              ((==) 2 . Prelude.length) $
              Prelude.map
                (  Prelude.map Data.Text.unpack
                .  Data.Text.split (== '=')
                .  Data.Text.pack
                )
                (Prelude.lines result)
      let frameRates' = frameRates entries
      let formats' =
            case formats entries of
              (f:_) -> f
              _     -> []
      let width' =
            case widths entries of
              (w:_) -> Just w
              _     -> Nothing
      let height' =
            case heights entries of
              (h:_) -> Just h
              _     -> Nothing
      let fps' =
            case frameRates' of
              (fr:_) -> Just fr
              _      -> Nothing
      let duration =
            if isGif
              then
                case (frameRates', frameCounts entries) of
                  (fr:_, fc:_) ->
                    if fc <= 0.0
                      then Nothing
                      else Just $ fc / fr
                  _ -> Nothing
              else
                case durations entries of
                  (a:b:_) -> if a < b then Just b else Just a
                  (a:_)   -> Just a
                  _       -> Nothing
      return $
        case
          ( formats'
          , width'
          , height'
          , fps'
          , duration
          )
        of
          (f@(_:_), Just w, Just h, Just fr, Just d) ->
            Just
              PlayableMetadata
                { playableMetadataFormats  = f
                , playableMetadataWidth    = w
                , playableMetadataHeight   = h
                , playableMetadataFps      = fr
                , playableMetadataDuration = d
                }
          _ -> Nothing
  where
    isGif :: Bool
    isGif = hasFileExtension "gif" inputFile
    formats :: [[String]] -> [[String]]
    formats entries =
      getEntries
        entries
        "format_name"
        (  Just
        .  Prelude.map Data.Text.unpack
        .  Data.Text.split (== ',')
        .  Data.Text.pack
        )
        isJust
        (fromMaybe [])
        (not . Prelude.null)
    widths :: [[String]] -> [Double]
    widths = getDoubles "width"
    heights :: [[String]] -> [Double]
    heights = getDoubles "height"
    durations :: [[String]] -> [Double]
    durations = getDoubles "duration"
    frameRates :: [[String]] -> [Double]
    frameRates entries =
      getEntries
        entries
        "avg_frame_rate"
        parseFrameRate
        isJust
        (fromMaybe 0.0)
        (> 0.0)
    frameCounts :: [[String]] -> [Double]
    frameCounts = getDoubles "nb_read_frames"
    getDoubles :: String -> [[String]] -> [Double]
    getDoubles key entries=
      getEntries
        entries
        key
        (\ x -> readMaybe x :: Maybe Double)
        isJust
        (fromMaybe 0.0)
        (> 0.0)
    getEntries
      ::  [[String]]
      ->  String
      ->  (String -> Maybe a)
      ->  (Maybe a -> Bool)
      ->  (Maybe a -> a)
      ->  (a -> Bool)
      ->  [a]
    getEntries
      entries
      key
      parser
      maybeFilterer
      maybeMaper
      filterer
      =
      Prelude.filter filterer
        $ Prelude.map maybeMaper
         $ Prelude.filter maybeFilterer
          $ Prelude.map parser
           $ findValues entries key
    parseFrameRate :: String -> Maybe Double
    parseFrameRate s =
      case (numerator, denominator) of
        (Just n, Just d) -> if d == 0.0 then Nothing else Just $ n / d
        _                -> Nothing
      where
        text :: Data.Text.Text
        text = Data.Text.pack s
        split' :: [String]
        split' = Prelude.map Data.Text.unpack $ Data.Text.split (=='/') text
        numerator :: Maybe Double
        numerator =
          if Prelude.length split' == 2
            then readMaybe (Prelude.head split') :: Maybe Double
            else Nothing
        denominator :: Maybe Double
        denominator =
          if Prelude.length split' == 2
            then readMaybe (Prelude.last split') :: Maybe Double
            else Nothing
    findValues :: [[String]] -> String -> [String]
    findValues entries key =
      Prelude.foldl
        find'
        []
        entries
      where
        find' :: [String] -> [String] -> [String]
        find' a (x:y:_) = if x == key then a ++ [y] else a
        find' a _ = a
    params :: [String]
    params =
          [ "-i"
          , inputFile
          , "-v"
          , "error"
          ]
      ++  ["-count_frames" | isGif]
      ++  [ "-select_streams"
          , "v:0"
          , "-show_entries"
          , "stream=duration,avg_frame_rate,width,height" ++ if isGif then ",nb_read_frames" else ""
          , "-show_entries"
          , "format=format_name,duration"
          , "-of"
          , "default=noprint_wrappers=1"
          ]

-- | Finds or creates the temporary directory for Gifcurry.
-- This directory is used for storing temporary frames.
findOrCreateTemporaryDirectory
  ::  IO FilePath
findOrCreateTemporaryDirectory = do
  filePath <- System.Directory.getXdgDirectory System.Directory.XdgCache "gifcurry"
  System.Directory.createDirectoryIfMissing True filePath
  return filePath

-- | Adds the proper file extension to the 'outputFile' depending on 'saveAsVideo'.
getOutputFileWithExtension
  ::  GifParams
  ->  String
getOutputFileWithExtension
  GifParams
    { outputFile
    , saveAsVideo
    }
  =
  if hasFileExtension fileExtension outputFile
    then
          outputFile
    else
          outputFile
      ++  "."
      ++  fileExtension
  where
    fileExtension
      ::  String
    fileExtension
      =
      stripAndLowerString
        (if saveAsVideo then videoExtension else gifExtension)

-- | Convenience function for saving 'TextOverlay' to a file.
saveTextOverlaysToFile
  ::  String
  ->  [TextOverlay]
  ->  IO ()
saveTextOverlaysToFile
  = encodeFile

-- | Convenience function for converting a SRT or text overlays YAML file to 'TextOverlay'.
convertFileToTextOverlays
  ::  String
  ->  Maybe (Double, Double) -- ^ You can supply the width and height
                             -- of the video to convert SRT subtitle
                             -- X1 and Y1 to 'textOverlayXTranslation'
                             -- and 'textOverlayYTranslation'.
  ->  IO [TextOverlay]
convertFileToTextOverlays
  filePath
  maybeWidthHeight
  = do
  let srt  = hasFileExtension "srt"  filePath
  let yaml = hasFileExtension "yaml" filePath
  case (srt, yaml) of
    (True,  _) -> convertSrtFileToTextOverlays filePath maybeWidthHeight
    (_,  True) -> parseTextOverlaysFile        filePath
    _          -> return []

-- | Parses a string for a major, minor, and patch version number.
parseVersionNumber
  ::  ReadP (String, String, String)
parseVersionNumber = do
  _ <- munch1 (not . isNumber)
  major <- parseNumber
  _ <- char '.'
  minor <- parseNumber
  _ <- char '.'
  patch <- parseNumber
  _ <- char ' ' <|> char '-' <|> char '.'
  return (major, minor, patch)

-- | Parses a string, such as rgba(r,g,b,a) or rgb(r,g,b),
-- for a red, blue, green, and (optional) alpha component
-- if possible.
getRgba
  ::  String
  ->  Maybe (Int, Int, Int, Double)
getRgba s =
  case parsedResult s of
    (r, g, b, a) ->
      case ( readMaybeInt r
           , readMaybeInt g
           , readMaybeInt b
           , readMaybeDouble a
           ) of
        (Just r', Just g', Just b', Just a')
          -> Just (r', g', b', a')
        _ -> Nothing
  where
    readMaybeInt :: String -> Maybe Int
    readMaybeInt = readMaybe
    readMaybeDouble :: String -> Maybe Double
    readMaybeDouble = readMaybe
    parsedResult :: String -> (String, String, String, String)
    parsedResult s' =
      case readP_to_S parseRgba s' of
        [((r, g, b, a), _)] -> ( r,  g,  b, a)
        _                   -> ("", "", "", "")

gifOutputFile
  ::  String
  ->  String
gifOutputFile outputFile =
  getOutputFileWithExtension $
  defaultGifParams { outputFile = outputFile, saveAsVideo = False }

videoOutputFile
  ::  String
  ->  String
videoOutputFile outputFile =
  getOutputFileWithExtension $
  defaultGifParams { outputFile = outputFile, saveAsVideo = True }

printGifParams
  ::  GifParams
  ->  IO ()
printGifParams
  gifParams@GifParams
    { inputFile
    , saveAsVideo
    , startTime
    , endTime
    , width
    , fps
    , colorCount
    , dither
    , leftCrop
    , rightCrop
    , topCrop
    , bottomCrop
    , textOverlays
    }
  =
  putStrLn $
    Prelude.unlines $
          [ "[INFO] Here are your settings."
          , ""
          , "  - FILE IO:"
          , "    - Input File:    " ++ inputFile
          , "    - Output File:   " ++ getOutputFileWithExtension gifParams
          , "    - Save As Video: " ++ if saveAsVideo then "Yes" else "No"
          , "  - TIME:"
          , "    - Start Second: "  ++ printDouble startTime
          , "    - End   Second: "  ++ printDouble endTime
          , "  - OUTPUT FILE SIZE:"
          , "    - Width:       "   ++ show width ++ "px"
          , "    - FPS:         "   ++ show fps
          , "    - Color Count: "   ++ show colorCount
          , "    - Dither:      "   ++ show dither
          ]
      ++  ( if Prelude.null textOverlays
              then []
              else
                    "  - TEXT:"
                :   Prelude.foldl
                      (\  xs
                          TextOverlay
                            { textOverlayText
                            , textOverlayFontFamily
                            , textOverlayFontStyle
                            , textOverlayFontStretch
                            , textOverlayFontWeight
                            , textOverlayFontSize
                            , textOverlayStartTime
                            , textOverlayEndTime
                            , textOverlayOrigin
                            , textOverlayXTranslation
                            , textOverlayYTranslation
                            , textOverlayRotation
                            , textOverlayOutlineSize
                            , textOverlayOutlineColor
                            , textOverlayFillColor
                            }
                        ->
                            xs
                        ++  [ "    - Text: "             ++ textOverlayText
                            , "      - Font:"
                            , "        - Family:  "      ++ textOverlayFontFamily
                            , "        - Size:    "      ++ show textOverlayFontSize
                            , "        - Style:   "      ++ textOverlayFontStyle
                            , "        - Stretch: "      ++ textOverlayFontStretch
                            , "        - Weight:  "      ++ show textOverlayFontWeight
                            , "      - Time:"
                            , "        - Start Second: " ++ printDouble textOverlayStartTime
                            , "        - End   Second: " ++ printDouble textOverlayEndTime
                            , "      - Translation:"
                            , "        - Origin: "       ++ show textOverlayOrigin
                            , "        - X:      "       ++ show textOverlayXTranslation
                            , "        - Y:      "       ++ show textOverlayYTranslation
                            , "      - Rotation:"
                            , "        - Degrees: "      ++ show textOverlayRotation
                            , "      - Outline:"
                            , "        - Size:  "        ++ show textOverlayOutlineSize
                            , "        - Color: "        ++ textOverlayOutlineColor
                            , "      - Fill:"
                            , "        - Color: "        ++ textOverlayFillColor
                            ]
                      )
                      []
                      textOverlays
          )
      ++
          [ "  - CROP:"
          , "    - Left:   " ++ printDouble leftCrop
          , "    - Right:  " ++ printDouble rightCrop
          , "    - Top:    " ++ printDouble topCrop
          , "    - Bottom: " ++ printDouble bottomCrop
          ]
  where
    printDouble
      ::  Double
      ->  String
    printDouble = printf "%.3f"

frameFileExtension
  ::  String
frameFileExtension = "png"

gifExtension
  ::  String
gifExtension = "gif"

videoExtension
  ::  String
videoExtension = "webm"

extractFrames
  ::  GifParams
  ->  String
  ->  IO (Either IOError String)
extractFrames
  GifParams
    { inputFile
    , startTime
    , fps
    , endTime
    , width
    , leftCrop
    , rightCrop
    , topCrop
    , bottomCrop
    }
  tempDir
  = do
  printInfo $ "Writing the temporary frames to: " ++ tempDir
  ffmpegVersionNumber <- getFfmpegVersionNumber
  let useExact        = ffmpegCanUseCropExact ffmpegVersionNumber
  let params'         = params useExact
  when (not useExact && cropNeeded)
    $ printWarning "Cannot use exact crop dimensions. Please upgrade FFmpeg."
  tryProcess "ffmpeg" params'
  where
    startTime' :: String
    startTime' = printf "%.3f" startTime
    durationTime' :: String
    durationTime' = printf "%.3f" (endTime - startTime)
    width' :: String
    width' = show $ fromIntegral width / (1.0 - leftCrop - rightCrop)
    frameRate' :: String
    frameRate' = show fps
    cropNeeded :: Bool
    cropNeeded = leftCrop + rightCrop + topCrop + bottomCrop > 0
    params :: Bool -> [String]
    params useExact =
      [ "-nostats"
      , "-loglevel"
      , "error"
      , "-an"
      , "-ss"
      , startTime'
      , "-i"
      , inputFile
      , "-t"
      , durationTime'
      , "-r"
      , frameRate'
      , "-q:v"
      , "31"
      , "-vf"
      , "scale="
        ++ width'
        ++ ":-1"
        ++ if cropNeeded
            then
                 ",crop=w=iw*(1-"
              ++ show (leftCrop + rightCrop)
              ++ "):h=ih*(1-"
              ++ show (topCrop  + bottomCrop)
              ++ "):x=iw*"
              ++ show leftCrop
              ++ ":y=ih*"
              ++ show topCrop
              ++ if useExact then ":exact=1" else ""
            else
              ""
      , "-start_number"
      , "0"
      , "-f"
      , "image2"
      , tempDir ++ [pathSeparator] ++ "extracted-frames_%010d." ++ frameFileExtension
      ]

annotateImage
  ::  GifParams
  ->  Double
  ->  Double
  ->  [Text]
  ->  String
  ->  [TextOverlay]
  ->  IO (Either IOError String)
annotateImage
  GifParams
    { leftCrop
    , rightCrop
    , topCrop
    , bottomCrop
    }
    gifWidthNoCrop
    gifHeightNoCrop
    fontFamilies
    filePath
    textOverlays
  = do
  let annotations =
        Prelude.foldl
          (\ xs
            TextOverlay
              { textOverlayText
              , textOverlayFontFamily
              , textOverlayFontStyle
              , textOverlayFontStretch
              , textOverlayFontWeight
              , textOverlayFontSize
              , textOverlayOrigin
              , textOverlayXTranslation
              , textOverlayYTranslation
              , textOverlayRotation
              , textOverlayOutlineSize
              , textOverlayOutlineColor
              , textOverlayFillColor
              }
            ->
                  xs
              ++  fontFamilyArg fontFamilies textOverlayFontFamily
              ++  [ "-style"
                  , textOverlayFontStyle
                  , "-stretch"
                  , textOverlayFontStretch
                  , "-weight"
                  , show textOverlayFontWeight
                  , "-pointsize"
                  , show textOverlayFontSize
                  , "-gravity"
                  , show textOverlayOrigin
                  , "-density"
                  , "96"
                  ]
              ++  ( if textOverlayOutlineSize <= 0
                      then []
                      else
                        [ "-strokewidth"
                        , show textOverlayOutlineSize
                        , "-stroke"
                        , textOverlayOutlineColor
                        , "-annotate"
                        ,     rotation textOverlayRotation
                          ++  position textOverlayOrigin textOverlayXTranslation textOverlayYTranslation
                        , textOverlayText
                        ]
                  )
              ++  ["-stroke"
                  , "none"
                  , "-fill"
                  , textOverlayFillColor
                  , "-annotate"
                  ,     rotation textOverlayRotation
                    ++  position textOverlayOrigin textOverlayXTranslation textOverlayYTranslation
                  , textOverlayText
                  ]
          )
          []
          textOverlays
  let params =
            [ "-quiet"
            , filePath
            ]
        ++  annotations
        ++  [ "-set"
            , "colorspace"
            , "sRGB"
            ]
        ++  [filePath]
  result <- tryProcess "convert" params
  if isLeft result
    then return result
    else return $ Right $ "Annotated " ++ filePath
  where
    {-
      .+    .+   +.
      +     +     +

      .+    .+   +.
      +     +     +

      +     +     +
      . +   .+   +.
    -}
    position :: TextOverlayOrigin -> Double -> Double -> String
    position TextOverlayOriginNorthWest textOverlayXTranslation textOverlayYTranslation =
          toString (x pos textOverlayXTranslation 1.0 0.0)
      ++  toString (y pos textOverlayYTranslation 1.0 0.0)
    position TextOverlayOriginNorth textOverlayXTranslation textOverlayYTranslation =
          toString (x pos textOverlayXTranslation 0.5 0.5)
      ++  toString (y pos textOverlayYTranslation 1.0 0.0)
    position TextOverlayOriginNorthEast textOverlayXTranslation textOverlayYTranslation =
          toString (x neg textOverlayXTranslation 0.0 1.0)
      ++  toString (y pos textOverlayYTranslation 1.0 0.0)
    position TextOverlayOriginWest textOverlayXTranslation textOverlayYTranslation =
          toString (x pos textOverlayXTranslation 1.0 0.0)
      ++  toString (y pos textOverlayYTranslation 0.5 0.5)
    position TextOverlayOriginCenter textOverlayXTranslation textOverlayYTranslation =
          toString (x pos textOverlayXTranslation 0.5 0.5)
      ++  toString (y pos textOverlayYTranslation 0.5 0.5)
    position TextOverlayOriginEast textOverlayXTranslation textOverlayYTranslation =
          toString (x neg textOverlayXTranslation 0.0 1.0)
      ++  toString (y pos textOverlayYTranslation 0.5 0.5)
    position TextOverlayOriginSouthWest textOverlayXTranslation textOverlayYTranslation =
          toString (x pos textOverlayXTranslation 1.0 0.0)
      ++  toString (y neg textOverlayYTranslation 0.0 1.0)
    position TextOverlayOriginSouth textOverlayXTranslation textOverlayYTranslation =
          toString (x pos textOverlayXTranslation 0.5 0.5)
      ++  toString (y neg textOverlayYTranslation 0.0 1.0)
    position TextOverlayOriginSouthEast textOverlayXTranslation textOverlayYTranslation =
          toString (x neg textOverlayXTranslation 0.0 1.0)
      ++  toString (y neg textOverlayYTranslation 0.0 1.0)
    x :: Double -> Double -> Double -> Double -> Double
    x f t lc rc = f * (originX t - (gifWidthLeftCrop  * lc) + (gifWidthRightCrop   * rc))
    y :: Double -> Double -> Double -> Double -> Double
    y f t tc bc = f * (originY t - (gifHeightTopCrop  * tc) + (gifHeightBottomCrop * bc))
    originX :: Double -> Double
    originX = (*) gifWidthNoCrop
    originY :: Double -> Double
    originY = (*) gifHeightNoCrop
    gifWidthLeftCrop :: Double
    gifWidthLeftCrop    = gifWidthNoCrop  * leftCrop
    gifWidthRightCrop :: Double
    gifWidthRightCrop   = gifWidthNoCrop  * rightCrop
    gifHeightTopCrop :: Double
    gifHeightTopCrop    = gifHeightNoCrop * topCrop
    gifHeightBottomCrop :: Double
    gifHeightBottomCrop = gifHeightNoCrop * bottomCrop
    neg :: Double
    neg = -1.0
    pos :: Double
    pos =  1.0
    toString :: Double -> String
    toString f
      | f >= 0.0  = "+" ++ show (abs (round f :: Int))
      | otherwise = "-" ++ show (abs (round f :: Int))
    rotation :: Int -> String
    rotation d = show d' ++ "x" ++ show d'
      where
        d' :: Int
        d' = mod d 360

mergeFramesIntoGif
  ::  GifParams
  ->  String
  ->  IO (Either IOError String)
mergeFramesIntoGif
  GifParams
    { outputFile
    , fps
    , colorCount
    , dither
    }
  tempDir
  = do
  let outputFile' = gifOutputFile outputFile
  let params      =
            [ "-quiet"
            , "-delay"
            , show $ toInt $ 100.0 / fromIntegral fps
            , tempDir ++ [pathSeparator] ++ "extracted-frames_*." ++ frameFileExtension
            ]
        ++  (if dither then ["-dither", "FloydSteinberg"] else ["+dither"])
        ++  [ "-colors"
            , show colorCount
            , "-fuzz"
            , fuzz colorCount
            ,"-layers"
            , "OptimizeFrame"
            , "-layers"
            , "OptimizeTransparency"
            , "-loop"
            , "0"
            , "+map"
            , "-set"
            , "colorspace"
            , "sRGB"
            , outputFile'
            ]
  printInfo $ "Saving your GIF to: " ++ outputFile'
  result <- tryProcess "convert" params
  if isLeft result
    then return result
    else return $ Right outputFile'

mergeFramesIntoVideo
  ::  GifParams
  ->  String
  ->  IO (Either IOError String)
mergeFramesIntoVideo
  GifParams
    { outputFile
    , fps
    , colorCount
    , dither
    }
  tempDir
  = do
  when (colorCount < 256 && colorCount >= 1) $ do
    printInfo "Converting the frames to the specified color count."
    result' <-
      tryProcess
        "convert"
        (   [ "-quiet"
            , tempDir ++ [pathSeparator] ++ "extracted-frames_*." ++ frameFileExtension
            ]
        ++  (if dither then ["-dither", "FloydSteinberg"] else ["+dither"])
        ++  [ "-colors"
            , show colorCount
            , "-fuzz"
            , fuzz colorCount
            , "+map"
            , "-set"
            , "colorspace"
            , "sRGB"
            , "-set"
            , "filename:t"
            , "%d" ++ [pathSeparator] ++ "%t"
            , "%[filename:t]." ++ frameFileExtension
            ]
        )
    when (isLeft result') $
      printError "Something went wrong with ImageMagick."
  let outputFile' = videoOutputFile outputFile
  let params =
        [ "-nostats"
        , "-loglevel"
        , "error"
        , "-y"
        , "-framerate"
        , show fps
        , "-start_number"
        , "0"
        , "-i"
        , tempDir ++ [pathSeparator] ++ "extracted-frames_%010d." ++ frameFileExtension
        , "-c:v"
        , "libvpx-vp9"
        , "-crf"
        , show $ targetQuality colorCount
        , "-b:v"
        , "0"
        , "-pix_fmt"
        , "yuv420p"
        , "-vf"
        , "scale=trunc(iw/2)*2:trunc(ih/2)*2"
        , "-an"
        , outputFile'
        ]
  printInfo $ "Saving your video to: " ++ outputFile'
  result <- tryProcess "ffmpeg" params
  if isLeft result
    then return result
    else return (Right outputFile')
  where
    targetQuality :: Int -> Int
    targetQuality colorCount'
      | colorCount' >=   1 && colorCount' <=  85 = 15
      | colorCount' >=  86 && colorCount' <= 172 = 34
      | colorCount' >= 173 && colorCount' <= 256 = 37
      | otherwise                                = 37

fuzz
  ::  Int
  ->  String
fuzz colorCount'
  | colorCount' >=   1 && colorCount' <=  85 = "3%"
  | colorCount' >=  86 && colorCount' <= 172 = "2%"
  | colorCount' >= 173 && colorCount' <= 256 = "1%"
  | otherwise                                = "1%"

tryFfprobe
  ::  [String]
  ->  IO (Either IOError String)
tryFfprobe = tryProcess "ffprobe"

tryProcess
  ::  String
  ->  [String]
  ->  IO (Either IOError String)
tryProcess process params = try $ readProcess process params []

fontFamilyArg
  ::  [Text]
  ->  String
  ->  [String]
fontFamilyArg fontFamilies fontFamily = ["-family", fontFamily']
  where
    fontFamily' :: String
    fontFamily' = findFontFamily fontFamilies fontFamily

findFontFamily
  ::  [Text]
  ->  String
  ->  String
findFontFamily fontFamilies fontFamily =
  if hasFontFamily fontFamilies fontFamily
    then fontFamily
    else Data.Text.unpack $ getSansFontFamily fontFamilies

hasFontFamily
  ::  [Text]
  ->  String
  ->  Bool
hasFontFamily fontFamilies fontFamily =
  Prelude.any ((== fontFamily') . Data.Text.toLower) fontFamilies
  where
    fontFamily' :: Data.Text.Text
    fontFamily' = Data.Text.toLower $ Data.Text.pack fontFamily

getSansFontFamily
  ::  [Text]
  ->  Text
getSansFontFamily fontFamilies
  | notNull  preferedFontFamily = preferedFontFamily
  | notNull' sansFontFamilies   = Prelude.head sansFontFamilies
  | otherwise                   = "Sans"
  where
    preferedFontFamily :: Text
    preferedFontFamily =
      Prelude.foldl
        (\ xs x ->
          if notNull xs
            then xs
            else
              if contains "dejavu"    x ||
                 contains "ubuntu"    x ||
                 contains "droid"     x ||
                 contains "open"      x ||
                 contains "helvetica" x ||
                 contains "arial"     x
                then x
                else ""
        )
        ""
        sansFontFamilies
    notNull' :: [Text] -> Bool
    notNull' = Prelude.not . Prelude.null
    notNull :: Text -> Bool
    notNull = Prelude.not . Data.Text.null
    contains :: Text -> Text -> Bool
    contains h n = Data.Text.isInfixOf h $ Data.Text.toLower n
    sansFontFamilies :: [Text]
    sansFontFamilies =
      Prelude.filter
        (Data.Text.isInfixOf "sans" . Data.Text.toLower)
        fontFamilies

getFontFamilies
  ::  IO [Text]
getFontFamilies = do
  (_, stdout', _) <- readProcessWithExitCode "convert" ["-list", "font"] []
  let fontFamilies =
        Prelude.map
          (Data.Text.strip . Data.Text.drop 7 . Data.Text.strip)
            $ Prelude.filter (Data.Text.isInfixOf "family:" . Data.Text.toLower)
              $ Data.Text.splitOn "\n"
                $ Data.Text.strip
                  $ Data.Text.pack stdout'
  return fontFamilies

getFrameNumbers
  ::  [String]
  ->  Maybe [Int]
getFrameNumbers filePaths =
  if Prelude.length frameNumbers == Prelude.length filePaths
    then Just frameNumbers
    else Nothing
  where
    maybeFrameNumbers :: [Maybe Int]
    maybeFrameNumbers = Prelude.map (getFrameNumber . System.FilePath.takeFileName) filePaths
    frameNumbers :: [Int]
    frameNumbers = Prelude.foldl folder [] maybeFrameNumbers
      where
        folder :: [Int] -> Maybe Int -> [Int]
        folder xs (Just int) = xs ++ [int]
        folder xs Nothing    = xs

getFrameNumber
  ::  String
  ->  Maybe Int
getFrameNumber s =
  readMaybe (parsedResult s) :: Maybe Int
  where
    parsedResult :: String -> String
    parsedResult s' =
      case readP_to_S parseFileName s' of
        [(x, _)] -> x
        _        -> ""
    parseFileName :: ReadP String
    parseFileName = do
      _ <- string "extracted-frames_"
      digits <- parseNumber
      _ <- char '.'
      return digits

ffmpegCanUseCropExact
  ::  Maybe (Int, Int, Int)
  ->  Bool
ffmpegCanUseCropExact Nothing              = False
ffmpegCanUseCropExact (Just (major, _, _)) = major >= 3

getFfmpegVersionNumber
  ::  IO (Maybe (Int, Int, Int))
getFfmpegVersionNumber =
  getProcessVersionNumber "ffmpeg" ["-version"]

getProcessVersionNumber
  ::  String
  ->  [String]
  ->  IO (Maybe (Int, Int, Int))
getProcessVersionNumber
  process
  args
  = do
  processResult <- tryProcess process args
  case processResult of
    Left  _       -> return Nothing
    Right string' -> do
      let result =
            readP_to_S
              parseVersionNumber
              (stripAndLowerString string')
      case result of
        [((x, y, z), _)] -> do
          let x' = readMaybe x :: Maybe Int
          let y' = readMaybe y :: Maybe Int
          let z' = readMaybe z :: Maybe Int
          case (x', y', z') of
            (Just major, Just minor, Just patch)
              -> return $ Just (major, minor, patch)
            _ -> return Nothing
        _ -> return Nothing

safeRgbaString
  ::  String
  ->  (Int, Int, Int, Double)
  ->  String
safeRgbaString s d =
  case getRgba s of
    Just x  -> makeString x
    Nothing -> makeString d
  where
    makeString :: (Int, Int, Int, Double) -> String
    makeString (r, g, b, a) =
      Prelude.concat
        [ "rgba("
        , show $ clipRgb r
        , ","
        , show $ clipRgb g
        , ","
        , show $ clipRgb b
        , ","
        , show $ clipA   a
        , ")"
        ]
    clipA :: Double -> Double
    clipA = clip 0.0 1.0
    clipRgb :: Int -> Int
    clipRgb = clip 0 255
    clip :: (Num a, Ord a) => a -> a -> a -> a
    clip l r x
      | x < l     = l
      | x > r     = r
      | otherwise = x

convertSrtFileToTextOverlays
  ::  String
  ->  Maybe (Double, Double)
  ->  IO [TextOverlay]
convertSrtFileToTextOverlays
  filePath
  maybeWidthHeight
  =
      Prelude.map srtSubtitleToTextOverlay
  .   sortBy sortSrtSubtitles
  <$> getSrtSubtitles filePath
  where
    sortSrtSubtitles
      :: SrtSubtitle
      -> SrtSubtitle
      -> Ordering
    sortSrtSubtitles
      a
      b
      = compare (srtSubtitleIndex a) (srtSubtitleIndex b)
    srtSubtitleToTextOverlay
      ::  SrtSubtitle
      ->  TextOverlay
    srtSubtitleToTextOverlay
      SrtSubtitle
        { srtSubtitleStart
        , srtSubtitleEnd
        , srtSubtitleText
        , srtSubtitleCoordinates
        }
      =
      defaultTextOverlay
        { textOverlayText         = Prelude.unwords srtSubtitleText
        , textOverlayFontSize     = 10
        , textOverlayStartTime    = timestampToSecond srtSubtitleStart
        , textOverlayEndTime      = timestampToSecond srtSubtitleEnd
        , textOverlayOrigin       = origin       srtSubtitleCoordinates maybeWidthHeight
        , textOverlayXTranslation = xTranslation srtSubtitleCoordinates maybeWidthHeight
        , textOverlayYTranslation = yTranslation srtSubtitleCoordinates maybeWidthHeight
        }
    timestampToSecond
      ::  Timestamp
      ->  Double
    timestampToSecond
      Timestamp
        { hours
        , minutes
        , seconds
        , milliseconds
        }
      =  fromIntegral (hours   * 60 * 60)
      +  fromIntegral (minutes * 60)
      +  fromIntegral  seconds
      + (fromIntegral  milliseconds * 0.001)
    origin
      ::  Maybe SrtSubtitleCoordinates
      ->  Maybe (Double, Double)
      ->  TextOverlayOrigin
    origin
      (Just
        SrtSubtitleCoordinates
          { x1
          , y1
          }
      )
      (Just (width, height))
      |  width           > 0
      && height          > 0
      && x1              >= 0
      && y1              >= 0
      && fromIntegral x1 <= width
      && fromIntegral y1 <= height = TextOverlayOriginNorthWest
    origin _ _                     = TextOverlayOriginSouth
    xTranslation
      ::  Maybe SrtSubtitleCoordinates
      ->  Maybe (Double, Double)
      ->  Double
    xTranslation
      (Just
        SrtSubtitleCoordinates
          { x1
          }
      )
      (Just (width, _))
      | width > 0 && x1 >= 0 && fromIntegral x1 <= width = fromIntegral x1 / width
      | otherwise                                        = 0
    xTranslation _ _                                     = 0
    yTranslation
      ::  Maybe SrtSubtitleCoordinates
      ->  Maybe (Double, Double)
      ->  Double
    yTranslation
      (Just
        SrtSubtitleCoordinates
          { y1
          }
      )
      (Just (_, height))
      | height > 0 && y1 >= 0 && fromIntegral y1 <= height = fromIntegral y1 / height
      | otherwise                                          = -0.05
    yTranslation _ _                                       = -0.05

getSrtSubtitles
  ::  String
  ->  IO [SrtSubtitle]
getSrtSubtitles
  filePath
  = do
  fileExist <- doesFileExist filePath
  if fileExist
    then do
      fileString <- tryReadFile
      let result = readP_to_S parseSrt fileString
      if not (Prelude.null result)
        then return $ fst $ Prelude.last result
        else return []
    else return []
  where
    tryReadFile :: IO String
    tryReadFile = do
      result    <- readFile filePath
      -- Force it to read the file.
      -- Crashes on "text/plain; charset=unknown-8bit".
      let force = seq (Prelude.length result) (return ())
      e         <- try force :: IO (Either SomeException ())
      case e of
        Left _  -> do
          printError $ "Could not open file: " ++ filePath
          return ""
        Right _ -> return result

parseSrt
  ::  ReadP [SrtSubtitle]
parseSrt = do
  skipSpaces
  many1 parseBlock
  where
    parseBlock
      ::  ReadP SrtSubtitle
    parseBlock = do
      i   <- parseNumber
      _   <- char '\n'
      sh  <- parseNumber
      _   <- char ':'
      sm  <- parseNumber
      _   <- char ':'
      ss  <- parseNumber
      _   <- char ','
      sm' <- parseNumber
      _   <- skipSpaces
      _   <- string "-->"
      _   <- skipSpaces
      eh  <- parseNumber
      _   <- char ':'
      em  <- parseNumber
      _   <- char ':'
      es  <- parseNumber
      _   <- char ',' <|> char '.'
      em' <- parseNumber
      r   <- option Nothing $ do
        _  <- skipSpaces1
        x1 <- getCoordinate 'x' 1
        _  <- skipSpaces1
        x2 <- getCoordinate 'x' 2
        _  <- skipSpaces1
        y1 <- getCoordinate 'y' 1
        _  <- skipSpaces1
        y2 <- getCoordinate 'y' 2
        return
          $ Just
            SrtSubtitleCoordinates
              { x1 = readInt 0 x1
              , x2 = readInt 0 x2
              , y1 = readInt 0 y1
              , y2 = readInt 0 y2
              }
      _   <- char '\n'
      s   <- manyTill
              ( manyTill
                  (satisfy (const True))
                  (void (string "\n") <|> eof)
              )
              (void (string "\n") <|> eof)
      return
        SrtSubtitle
          { srtSubtitleIndex       = readInt 0 i
          , srtSubtitleStart       =
            Timestamp
              { hours              = readInt 0 sh
              , minutes            = readInt 0 sm
              , seconds            = readInt 0 ss
              , milliseconds       = readInt 0 sm'
              }
          , srtSubtitleEnd         =
            Timestamp
              { hours              = readInt 0 eh
              , minutes            = readInt 0 em
              , seconds            = readInt 0 es
              , milliseconds       = readInt 0 em'
              }
          , srtSubtitleCoordinates = r
          , srtSubtitleText        = s
          }
    skipSpaces1 :: ReadP ()
    skipSpaces1 = void $ skipMany1 (char ' ')
    getCoordinate :: Char -> Int -> ReadP String
    getCoordinate c n = do
      _  <- char (Data.Char.toUpper c) <|> char (Data.Char.toLower c)
      _  <- string $ show n ++ ":"
      parseNumber

parseTextOverlaysFile
  ::  String
  ->  IO [TextOverlay]
parseTextOverlaysFile
  textOverlaysFile
  = do
  let textOverlaysFile' = unpack . strip . pack $ textOverlaysFile
  if Prelude.null textOverlaysFile'
    then return []
    else do
      textOverlaysFileExists  <- doesFileExist textOverlaysFile
      textOverlaysData        <-
        if textOverlaysFileExists
          then DBC.readFile textOverlaysFile
          else return ""
      let maybeTextOverlays =
            Data.Yaml.decode textOverlaysData :: Maybe [TextOverlay]
      case maybeTextOverlays of
        Nothing -> do
          printWarning $ "Could not parse the " ++ textOverlaysFile' ++ " YAML file!"
          return []
        Just textOverlays -> return textOverlays

parseRgba
  ::  ReadP (String, String, String, String)
parseRgba = do
  _ <- string "rgb"
  _ <- string "a(" <|> string "("
  r <- parseNumber
  _ <- char ','
  _ <- skipSpaces
  g <- parseNumber
  _ <- char ','
  _ <- skipSpaces
  b <- parseNumber
  a <- option "1.0" $ do
    _   <- char ','
    _   <- skipSpaces
    a'  <- parseNumber
    a'' <- option "0" $ do
      _ <- char '.'
      parseNumber
    return (a' ++ "." ++ a'')
  _ <- char ')'
  return (r, g, b, a)

parseNumber
  ::  ReadP String
parseNumber = many1 (satisfy isNumber)

readInt
  ::  Int
  ->  String
  ->  Int
readInt i s = fromMaybe i (readMaybe s :: Maybe Int)

toInt
  ::  Double
  ->  Int
toInt = round

stripAndLowerString
  ::  String
  ->  String
stripAndLowerString =
    Data.Text.unpack
  . Data.Text.toLower
  . Data.Text.strip
  . Data.Text.pack

stringsEqual
  ::  String
  ->  String
  ->  Bool
stringsEqual a b =
  stripAndLowerString a == stripAndLowerString b

textOverlayOutlineSizeMax :: Int
textOverlayOutlineSizeMax = 10

printError
  ::  String
  ->  IO ()
printError = printLevel "ERROR"

printWarning
  ::  String
  ->  IO ()
printWarning = printLevel "WARNING"

printInfo
  ::  String
  ->  IO ()
printInfo = printLevel "INFO"

printLevel
  ::  String
  ->  String
  ->  IO ()
printLevel l s =
  putStrLn
    $ Prelude.concat
      [ "["
      , Prelude.map Data.Char.toUpper l
      , "]"
      , " "
      , s
      ]

hasFileExtension
  ::  String
  ->  String
  ->  Bool
hasFileExtension
  extension
  string'
  =
      getFileExtension string'
  ==  stripAndLowerString ("." ++ Prelude.filter ('.' /=) extension)

getFileExtension
  ::  String
  ->  String
getFileExtension
  string'
  =
  takeExtension $ stripAndLowerString string'

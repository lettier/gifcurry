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
  , defaultGifParams
  , gifParamsValid
  , getPlayableMetadata
  , getOutputFileWithExtension
  , textOverlayOriginFromString
  , gif
  )
where

import System.Process
import System.IO.Temp
import System.Directory
import System.FilePath
import qualified System.FilePath.Find as SFF
import Control.Exception
import Control.Monad
import Text.Read
import Text.ParserCombinators.ReadP
import Text.Printf
import Data.Maybe
import Data.Text
import Data.Either

-- | The data type record required by 'gif'.
data GifParams =
  GifParams
    { inputFile      :: String
    , outputFile     :: String
    , saveAsVideo    :: Bool
    , startTime      :: Double
    , durationTime   :: Double
    , width          :: Int
    , fps            :: Int
    , colorCount     :: Int
    , textOverlays   :: [TextOverlay]
    , leftCrop       :: Double
    , rightCrop      :: Double
    , topCrop        :: Double
    , bottomCrop     :: Double
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
    , textOverlayDurationTime :: Double
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
  deriving (Read)

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

-- | The version number.
versionNumber :: String
versionNumber = "5.0.0.0"

-- | Specifies the default parameters for the following.
-- * 'startTime'
-- * 'durationTime'
-- * 'width'
-- * 'fps'
-- * 'colorCount'
-- * 'textOverlays'
-- * 'leftCrop'
-- * 'rightCrop'
-- * 'topCrop'
-- * 'bottomCrop'
defaultGifParams :: GifParams
defaultGifParams =
  GifParams
    { inputFile      = ""
    , outputFile     = ""
    , saveAsVideo    = False
    , startTime      = 0.0
    , durationTime   = 1.0
    , width          = 500
    , fps            = 24
    , colorCount     = 256
    , textOverlays   = []
    , leftCrop       = 0.0
    , rightCrop      = 0.0
    , topCrop        = 0.0
    , bottomCrop     = 0.0
    }

-- | Inputs 'GifParams' and outputs either an IO IOError or IO String.
--
-- @
--    import qualified Gifcurry (gif, GifParams(..), defaultGifParams, gifParamsValid)
--
--    main :: IO ()
--    main = do
--      let params =
--            Gifcurry.defaultGifParams
--              { Gifcurry.inputFile = ".\/in.mov"
--              , Gifcurry.outputFile = ".\/out.gif"
--              }
--      valid <- Gifcurry.gifParamsValid params
--      if valid
--        then do
--          result <- Gifcurry.gif params
--          print result
--        else return ()
-- @
gif
  ::  GifParams
  ->  IO (Either IOError String)
gif
  gifParams@GifParams
    { width
    , saveAsVideo
    , startTime
    , textOverlays
    , fps
    , leftCrop
    , rightCrop
    }
  = do
  printGifParams gifParams
  paramsValid           <- gifParamsValid gifParams
  maybePlayableMetadata <- getPlayableMetadata gifParams
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
          putStrLn "[ERROR] Something went wrong with FFmpeg."
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
            putStrLn "[INFO] Adding text."
            results <-
              mapM
              (\ (filePath, second) -> do
                let textOverlays' =
                      Prelude.foldl
                        (\ xs x ->
                          if      textOverlayStartTime x <= second
                              &&  textOverlayStartTime x + textOverlayDurationTime x >= second
                            then xs ++ [x]
                            else xs
                        )
                        []
                        textOverlays
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
            putStrLn $ "[ERROR] " ++ errorString
            return $ Left $ userError errorString
    handleFrameAnnotations _ _ (Left x) = return $ Left x
    handleFrameMerge :: String -> PlayableMetadata -> Either IOError String -> IO (Either IOError String)
    handleFrameMerge tempDir _ (Right _) =
      if saveAsVideo
        then do
          result <- mergeFramesIntoVideo gifParams tempDir
          case result of
            Left x -> do
              putStrLn "[ERROR] Something went wrong with FFmpeg."
              return $ Left x
            Right videoFilePath -> do
              putStrLn "[INFO] All done."
              return $ Right videoFilePath
        else do
          result <- mergeFramesIntoGif gifParams tempDir
          case result of
            Left x -> do
              putStrLn "[ERROR] Something went wrong with ImageMagick."
              return $ Left x
            Right gifFilePath -> do
              putStrLn "[INFO] All done."
              return $ Right gifFilePath
    handleFrameMerge _ _ (Left x) = return $ Left x

-- | Convenience function that attempts to turn a string into a 'TextOverlayOrigin'.
-- @
--    textOverlayOriginFromString "  cEntEr " -- Just TextOverlayOriginCenter
--    textOverlayOriginFromString "test"      -- Nothing
-- @
textOverlayOriginFromString :: String -> Maybe Gifcurry.TextOverlayOrigin
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

-- | Outputs `True` or `False` if the parameters in the `GifParams` record are valid.
gifParamsValid :: GifParams -> IO Bool
gifParamsValid
  GifParams
    { inputFile
    , outputFile
    , startTime
    , durationTime
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
  let durationTimeValid           = durationTime > 0.0
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
  let textOverlayColorsValid      =
        Prelude.all
        (\ TextOverlay { textOverlayOutlineColor, textOverlayFillColor } ->
          isJust (getRgb textOverlayOutlineColor) && isJust (getRgb textOverlayFillColor)
        )
        textOverlays
  unless inputFileExists              $ printError   "Input video file does not exist."
  unless outputFileValid              $ printInvalid "Output File"
  unless startTimeValid               $ printInvalid "Start Time"
  unless durationTimeValid            $ printInvalid "Duration Time"
  unless widthValid                   $ printInvalid "Width"
  unless fpsValid                     $ printInvalid "FPS"
  unless colorCountValid              $ printInvalid "Color Count"
  unless leftCropValid                $ printInvalid "Left Crop"
  unless rightCropValid               $ printInvalid "Right Crop"
  unless topCropValid                 $ printInvalid "Top Crop"
  unless bottomCropValid              $ printInvalid "Bottom Crop"
  unless leftRightCropValid           $ printInvalid "Left and Right Crop"
  unless topBottomCropValid           $ printInvalid "Top and Bottom Crop"
  unless widthLeftRightCropSizeValid  $ printError   "Width is too small with Left and Right Crop."
  unless textOverlayColorsValid       $ printError   "Text overlay color(s) invalid. The format is: rgb(r,g,b)"
  return $
       inputFileExists
    && outputFileValid
    && startTimeValid
    && durationTimeValid
    && widthValid
    && fpsValid
    && colorCountValid
    && leftCropValid
    && rightCropValid
    && topCropValid
    && bottomCropValid
    && widthLeftRightCropSizeValid
    && textOverlayColorsValid
  where
    cropValid :: Double -> Bool
    cropValid c = c >= 0.0 && c < 1.0
    printInvalid :: String -> IO ()
    printInvalid s = printError $ s ++ " is invalid."
    printError :: String -> IO ()
    printError s = putStrLn $ "[ERROR] " ++ s

-- | Returns the metadata for a playable media file if possible.
getPlayableMetadata :: GifParams -> IO (Maybe PlayableMetadata)
getPlayableMetadata GifParams { inputFile } = do
  eitherResult <- tryFfprobe params
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
    isGif = extension == ".gif"
    extension :: String
    extension = takeExtension $ stripAndLowerString inputFile
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
      Prelude.filter filterer $
        Prelude.map maybeMaper $
         Prelude.filter maybeFilterer $
          Prelude.map parser $
           findValues entries key
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
findOrCreateTemporaryDirectory :: IO FilePath
findOrCreateTemporaryDirectory = do
  filePath <- System.Directory.getXdgDirectory System.Directory.XdgCache "gifcurry"
  System.Directory.createDirectoryIfMissing True filePath
  return filePath

-- | Adds the proper file extension to the 'outputFile' depending on 'saveAsVideo'.
getOutputFileWithExtension :: GifParams -> String
getOutputFileWithExtension GifParams { outputFile, saveAsVideo } =
      outputFile
  ++  "."
  ++  (if saveAsVideo then videoExtension else gifExtension)

gifOutputFile :: String -> String
gifOutputFile outputFile =
  getOutputFileWithExtension $
  defaultGifParams { outputFile = outputFile, saveAsVideo = False }

videoOutputFile :: String -> String
videoOutputFile outputFile =
  getOutputFileWithExtension $
  defaultGifParams { outputFile = outputFile, saveAsVideo = True }

printGifParams :: GifParams -> IO ()
printGifParams
  gifParams@GifParams
    { inputFile
    , saveAsVideo
    , startTime
    , durationTime
    , width
    , fps
    , colorCount
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
          , "    - Input File: "    ++ inputFile
          , "    - Output File: "   ++ getOutputFileWithExtension gifParams
          , "    - Save As Video: " ++ if saveAsVideo then "Yes" else "No"
          , "  - TIME:"
          , "    - Start Second: "  ++ printDouble startTime
          , "    - Duration Time: " ++ printDouble durationTime ++ " seconds"
          , "  - OUTPUT FILE SIZE:"
          , "    - Width: "         ++ show width ++ "px"
          , "    - FPS: "           ++ show fps
          , "    - Color Count: "   ++ show colorCount
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
                            , textOverlayDurationTime
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
                            , "        - Family: "       ++ textOverlayFontFamily
                            , "        - Size: "         ++ show textOverlayFontSize
                            , "        - Style: "        ++ textOverlayFontStyle
                            , "        - Stretch: "      ++ textOverlayFontStretch
                            , "        - Weight: "       ++ show textOverlayFontWeight
                            , "      - Time:"
                            , "        - Start Second: " ++ printDouble textOverlayStartTime
                            , "        - Duration: "     ++ printDouble textOverlayDurationTime ++ " seconds"
                            , "      - Translation:"
                            , "        - Origin: "       ++ show textOverlayOrigin
                            , "        - X: "            ++ show textOverlayXTranslation
                            , "        - Y: "            ++ show textOverlayYTranslation
                            , "      - Rotation:"
                            , "        - Degrees: "      ++ show textOverlayRotation
                            , "      - Outline: "
                            , "        - Size: "         ++ show textOverlayOutlineSize
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
          , "    - Left: "   ++ printDouble leftCrop
          , "    - Right: "  ++ printDouble rightCrop
          , "    - Top: "    ++ printDouble topCrop
          , "    - Bottom: " ++ printDouble bottomCrop
          ]
  where
    printDouble :: Double -> String
    printDouble = printf "%.3f"

frameFileExtension :: String
frameFileExtension = "png"

gifExtension :: String
gifExtension = "gif"

videoExtension :: String
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
    , durationTime
    , width
    , leftCrop
    , rightCrop
    , topCrop
    , bottomCrop
    }
  tempDir
  = do
  putStrLn $ "[INFO] Writing the temporary frames to: " ++ tempDir
  tryProcess "ffmpeg" params
  where
    startTime' :: String
    startTime' = printf "%.3f" startTime
    durationTime' :: String
    durationTime' = printf "%.3f" durationTime
    width' :: String
    width' = show $ fromIntegral width / (1.0 - leftCrop - rightCrop)
    frameRate' :: String
    frameRate' = show fps
    params :: [String]
    params =
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
        ++",crop=w=iw*(1-"
        ++ show (leftCrop + rightCrop)
        ++ "):h=ih*(1-"
        ++ show (topCrop  + bottomCrop)
        ++ "):x=iw*"
        ++ show leftCrop
        ++ ":y=ih*"
        ++ show topCrop
        ++ ":exact=1"
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
    }
  tempDir
  = do
  let outputFile' = gifOutputFile outputFile
  let params      =
        [ "-quiet"
        , "-delay"
        , show $ toInt $ 100.0 / fromIntegral fps
        , tempDir ++ [pathSeparator] ++ "extracted-frames_*." ++ frameFileExtension
        , "+dither"
        , "-colors"
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
  putStrLn $ "[INFO] Saving your GIF to: " ++ outputFile'
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
    }
  tempDir
  = do
  when (colorCount < 256 && colorCount >= 1) $ do
    putStrLn "[INFO] Converting the frames to the specified color count."
    result' <-
      tryProcess
        "convert"
        [ "-quiet"
        , tempDir ++ [pathSeparator] ++ "extracted-frames_*." ++ frameFileExtension
        , "+dither"
        , "-colors"
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
    when (isLeft result') $
      putStrLn "[ERROR] Something went wrong with ImageMagick."
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
  putStrLn $ "[INFO] Saving your video to: " ++ outputFile'
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

fuzz :: Int -> String
fuzz colorCount'
  | colorCount' >=   1 && colorCount' <=  85 = "3%"
  | colorCount' >=  86 && colorCount' <= 172 = "2%"
  | colorCount' >= 173 && colorCount' <= 256 = "1%"
  | otherwise                                = "1%"

tryFfprobe :: [String] -> IO (Either IOError String)
tryFfprobe = tryProcess "ffprobe"

tryProcess :: String -> [String] -> IO (Either IOError String)
tryProcess process params = try $ readProcess process params []

fontFamilyArg :: [Text] -> String -> [String]
fontFamilyArg fontFamilies fontFamily = ["-family", fontFamily']
  where
    fontFamily' :: String
    fontFamily' = findFontFamily fontFamilies fontFamily

findFontFamily :: [Text] -> String -> String
findFontFamily fontFamilies fontFamily =
  if hasFontFamily fontFamilies fontFamily
    then fontFamily
    else Data.Text.unpack $ getSansFontFamily fontFamilies

hasFontFamily :: [Text] -> String -> Bool
hasFontFamily fontFamilies fontFamily =
  Prelude.any ((== fontFamily') . Data.Text.toLower) fontFamilies
  where
    fontFamily' :: Data.Text.Text
    fontFamily' = Data.Text.toLower $ Data.Text.pack fontFamily

getSansFontFamily :: [Text] -> Text
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

getFontFamilies :: IO [Text]
getFontFamilies = do
  (_, stdout, _) <- readProcessWithExitCode "convert" ["-list", "font"] []
  let fontFamilies =
        Prelude.map
          (Data.Text.strip . Data.Text.drop 7 . Data.Text.strip) $
            Prelude.filter (Data.Text.isInfixOf "family:" . Data.Text.toLower) $
              Data.Text.splitOn "\n" $
                Data.Text.strip $
                  Data.Text.pack stdout
  return fontFamilies

getFrameNumbers :: [String] -> Maybe [Int]
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

getFrameNumber :: String -> Maybe Int
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

getRgb :: String -> Maybe (Int, Int, Int)
getRgb s =
  case parsedResult s of
    (r, g, b) ->
      case (readMaybe' r, readMaybe' g, readMaybe' b) of
        (Just r', Just g', Just b') -> Just (r', g', b')
        _                           -> Nothing
  where
    readMaybe' :: String -> Maybe Int
    readMaybe' = readMaybe
    parsedResult :: String -> (String, String, String)
    parsedResult s' =
      case readP_to_S parseRgb s' of
        [((r,g,b), _)] -> (r, g, b)
        _              -> ("", "", "")
    parseRgb :: ReadP (String, String, String)
    parseRgb = do
      _ <- string "rgb("
      r <- parseNumber
      _ <- char ','
      g <- parseNumber
      _ <- char ','
      b <- parseNumber
      _ <- char ')'
      return (r, g, b)

parseNumber :: ReadP String
parseNumber = many (satisfy isNumber)
  where
    isNumber :: Char -> Bool
    isNumber = flip elem numbers
    numbers :: String
    numbers = "0123456789"

toInt :: Double -> Int
toInt = round

stripAndLowerString :: String -> String
stripAndLowerString =
  Data.Text.unpack . Data.Text.toLower . Data.Text.strip . Data.Text.pack

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
  ( gif
  , GifParams(..)
  , Quality(..)
  , TextOverlay(..)
  , TextOverlayOrigin(..)
  , defaultGifParams
  , gifParamsValid
  , versionNumber
  , getVideoDurationInSeconds
  , getOutputFileWithExtension
  , getVideoWidthAndHeight
  , findOrCreateTemporaryDirectory
  , qualityFromString
  , textOverlayOriginFromString
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
import Data.List
import Data.Text
import Data.Either

-- | The data type record required by 'gif'.
data GifParams =
  GifParams
    { inputFile      :: String
    , outputFile     :: String
    , saveAsVideo    :: Bool
    , startTime      :: Float
    , durationTime   :: Float
    , widthSize      :: Int
    , quality        :: Quality
    , textOverlays   :: [TextOverlay]
    , leftCrop       :: Float
    , rightCrop      :: Float
    , topCrop        :: Float
    , bottomCrop     :: Float
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
    , textOverlayXTranslation :: Float
    , textOverlayYTranslation :: Float
    , textOverlayRotation     :: Int
    , textOverlayStartTime    :: Float
    , textOverlayDurationTime :: Float
    , textOverlayOutlineSize  :: Int
    , textOverlayOutlineColor :: String
    , textOverlayFillColor    :: String
    }
  deriving (Show, Read)

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

-- | Controls the amount of colors used and the frame rate.
-- Higher values will result in a larger file size.
data Quality =
    QualityHigh
  | QualityMedium
  | QualityLow
  deriving (Read)

instance Show Quality where
  show QualityHigh     = "High"
  show QualityMedium   = "Medium"
  show QualityLow      = "Low"

-- | The version number.
versionNumber :: String
versionNumber = "4.0.0.0"

-- | Specifies the default parameters for the following.
-- * 'startTime'
-- * 'durationTime'
-- * 'widthSize'
-- * 'quality'
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
    , widthSize      = 500
    , quality        = QualityHigh
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
--    main :: IO ()
--    main = do
--      let params = Gifcurry.defaultGifParams { Gifcurry.inputFile = ".\/in.mov", Gifcurry.outputFile = ".\/out.gif" }
--      valid <- Gifcurry.gifParamsValid params
--      if valid
--        then do
--          result <- Gifcurry.gif params
--          print result
--        else return ()
-- @
gif :: GifParams -> IO (Either IOError String)
gif
  gifParams@GifParams
    { widthSize
    , saveAsVideo
    , startTime
    , textOverlays
    , quality
    }
  = do
  printGifParams gifParams
  validParams <- gifParamsValid gifParams
  if validParams
    then do
      temporaryDirectory <- findOrCreateTemporaryDirectory
      withTempDirectory temporaryDirectory "gifcurry-frames" $ \ tempDir ->
        handleFrameExtraction tempDir >>=
          handleFrameAnnotations tempDir >>=
            handleFrameMerge tempDir
    else return $ Left $ userError "Invalid params."
  where
    handleFrameExtraction :: String -> IO (Either IOError Float)
    handleFrameExtraction tempDir = do
      frameRate <- qualityAndFrameRateToFrameRate quality . fromMaybe defaultFrameRate <$>
                      getVideoAverageFrameRateInSeconds gifParams
      result    <- extractFrames gifParams tempDir frameRate
      case result of
        Left x  -> do
          putStrLn "[ERROR] Something went wrong with FFmpeg."
          return $ Left x
        Right _ -> return $ Right frameRate
    handleFrameAnnotations :: String -> Either IOError Float -> IO (Either IOError Float)
    handleFrameAnnotations tempDir (Right frameRate)
      | Prelude.null textOverlays = return $ Right frameRate
      | frameRate <= 0.0 = do
        let errorString = "Frame rate is less than or equal to zero."
        putStrLn $ "[ERROR] " ++ errorString
        return $ Left $ userError errorString
      | otherwise = do
        frameFilePaths <-
          SFF.find
            SFF.always
            (SFF.fileName SFF.~~? "*extracted-frames_*")
            tempDir
        let maybeFrameNumbers = getFrameNumbers frameFilePaths
        case maybeFrameNumbers of
          Just frameNumbers -> do
            fontFamilies            <- getFontFamilies
            maybeInVideoWidthHeight <- getVideoWidthAndHeight gifParams
            let frameSeconds =
                  Prelude.map
                    (\ x -> startTime + ((realToFrac x :: Float) * (1.0 / frameRate)))
                    frameNumbers
            let frameFilePathsFrameSeconds = Prelude.zip frameFilePaths frameSeconds
            let widthSize' = fromIntegral widthSize :: Float
            let (gifWidthNoCrop, gifHeightNoCrop) =
                  case maybeInVideoWidthHeight of
                    Just (w, h) -> (widthSize', widthSize' * (h / w))
                    _           -> (       0.0,                  0.0)
            putStrLn "[INFO] Adding text..."
            results <-
              mapM
              (\ (filePath, second) -> do
                let textOverlays' =
                      Prelude.foldl
                        (\ xs x ->
                          if textOverlayStartTime x <= second &&
                              textOverlayStartTime x + textOverlayDurationTime x >= second
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
              else return $ Right frameRate
          Nothing -> do
            let errorString = "Could not find the frame numbers."
            putStrLn $ "[ERROR] " ++ errorString
            return $ Left $ userError errorString
    handleFrameAnnotations _ (Left x) = return $ Left x
    handleFrameMerge :: String -> Either IOError Float -> IO (Either IOError String)
    handleFrameMerge tempDir (Right frameRate) =
      if saveAsVideo
        then do
          result <- mergeFramesIntoVideo gifParams tempDir frameRate
          case result of
            Left x -> do
              putStrLn "[ERROR] Something went wrong with FFmpeg."
              return $ Left x
            Right videoFilePath -> do
              putStrLn "[INFO] All done."
              return $ Right videoFilePath
        else do
          result <- mergeFramesIntoGif gifParams tempDir frameRate
          case result of
            Left x -> do
              putStrLn "[ERROR] Something went wrong with ImageMagick."
              return $ Left x
            Right gifFilePath -> do
              putStrLn "[INFO] All done."
              return $ Right gifFilePath
    handleFrameMerge _ (Left x) = return $ Left x

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

-- | Convenience function that attempts to turn a string into a 'Quality'.
-- @
--    qualityFromString "  hIgH " -- Just QualityHigh
--    qualityFromString "test"     -- Nothing
-- @
qualityFromString :: String -> Maybe Quality
qualityFromString quality =
  qualityFromString' $
    stripAndLowerString quality
  where
    qualityFromString' :: String -> Maybe Quality
    qualityFromString' "high"      = Just QualityHigh
    qualityFromString' "medium"    = Just QualityMedium
    qualityFromString' "low"       = Just QualityLow
    qualityFromString' _           = Nothing

-- | Outputs `True` or `False` if the parameters in the `GifParams` record are valid.
gifParamsValid :: GifParams -> IO Bool
gifParamsValid
  GifParams
    { inputFile
    , outputFile
    , startTime
    , durationTime
    , widthSize
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
  let widthSize'                  = fromIntegral widthSize :: Float
  let outputFileValid             = not $ Data.Text.null $ Data.Text.strip $ Data.Text.pack outputFile
  let startTimeValid              = startTime >= 0.0
  let durationTimeValid           = durationTime > 0.0
  let widthSizeValid              = widthSize >= 1
  let leftCropValid               = cropValid leftCrop
  let rightCropValid              = cropValid rightCrop
  let topCropValid                = cropValid topCrop
  let bottomCropValid             = cropValid bottomCrop
  let leftRightCropValid          = cropValid (leftCrop + rightCrop)
  let topBottomCropValid          = cropValid (topCrop + bottomCrop)
  let widthLeftRightCropSizeValid =
        (widthSize' - (widthSize' * leftCrop) - (widthSize' * rightCrop)) >= 1.0
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
  unless widthSizeValid               $ printInvalid "Width Size"
  unless leftCropValid                $ printInvalid "Left Crop"
  unless rightCropValid               $ printInvalid "Right Crop"
  unless topCropValid                 $ printInvalid "Top Crop"
  unless bottomCropValid              $ printInvalid "Bottom Crop"
  unless leftRightCropValid           $ printInvalid "Left and Right Crop"
  unless topBottomCropValid           $ printInvalid "Top and Bottom Crop"
  unless widthLeftRightCropSizeValid  $ printError   "Width Size too small with Left and Right Crop."
  unless textOverlayColorsValid       $ printError   "Text overlay color(s) invalid. The format is: rgb(r,g,b)"
  return $
       inputFileExists
    && outputFileValid
    && startTimeValid
    && durationTimeValid
    && widthSizeValid
    && leftCropValid
    && rightCropValid
    && topCropValid
    && bottomCropValid
    && widthLeftRightCropSizeValid
    && textOverlayColorsValid
  where
    cropValid :: Float -> Bool
    cropValid c = c >= 0.0 && c < 1.0
    printInvalid :: String -> IO ()
    printInvalid s = printError $ s ++ " is invalid."
    printError :: String -> IO ()
    printError s = putStrLn $ "[ERROR] " ++ s

-- | Returns the duration of the video in seconds if successful.
--
-- @
--    import qualified Gifcurry (getVideoDurationInSeconds)
--    -- ...
--    let params = Gifcurry.defaultGifParams { Gifcurry.inputFile = ".\/in.mov" }
--    maybeDuration <- Gifcurry.getVideoDurationInSeconds params
--    let duration = case maybeDuration of
--                      Nothing    -> 0.0
--                      Just float -> float
-- @
getVideoDurationInSeconds :: GifParams -> IO (Maybe Float)
getVideoDurationInSeconds GifParams { inputFile } = do
  streamResult <- result <$> tryFfprobe streamParams
  if streamResult <= 0.0
    then do
      containerResult <- result <$> tryFfprobe containerParams
      if containerResult <= 0.0
        then return Nothing
        else return $ Just containerResult
    else return $ Just streamResult
  where
    result :: Either IOError String -> Float
    result (Left _)               = 0.0
    result (Right durationString) = fromMaybe 0.0 (readMaybe durationString :: Maybe Float)
    streamParams :: [String]
    streamParams =
      [ "-i"
      , inputFile
      , "-v"
      , "error"
      , "-select_streams"
      , "v:0"
      , "-show_entries"
      , "stream=duration"
      , "-of"
      , "default=noprint_wrappers=1:nokey=1"
      ]
    containerParams :: [String]
    containerParams =
      [ "-i"
      , inputFile
      , "-v"
      , "error"
      , "-show_entries"
      , "format=duration"
      , "-of"
      , "default=noprint_wrappers=1:nokey=1"
      ]

-- | Returns the width and height of the video if successful.
-- If the width and/or height of the video is <= 0, it will
-- return nothing.
getVideoWidthAndHeight :: GifParams -> IO (Maybe (Float, Float))
getVideoWidthAndHeight GifParams { inputFile } = tryFfprobe params >>= result
  where
    result :: Either IOError String -> IO (Maybe (Float, Float))
    result (Left _)                  = return Nothing
    result (Right widthHeightString) =
      case (maybeWidth, maybeHeight) of
        (Just width, Just height) ->
          if width >= 0.0 && height > 0.0
            then return $ Just (width, height)
            else return Nothing
        _                         -> return Nothing
      where
        maybeWidth :: Maybe Float
        maybeWidth =
          case widthHeightTexts of
            (widthText:_) -> maybeFloat widthText
            _             -> Nothing
        maybeHeight :: Maybe Float
        maybeHeight =
          case widthHeightTexts of
            (_:heightText:_) -> maybeFloat heightText
            _                -> Nothing
        maybeFloat :: Text -> Maybe Float
        maybeFloat t = readMaybe (Data.Text.unpack t) :: Maybe Float
        widthHeightTexts :: [Text]
        widthHeightTexts =
          (Data.List.map Data.Text.strip . Data.Text.lines) widthHeightText
        widthHeightText :: Text
        widthHeightText =
          Data.Text.strip $ Data.Text.pack widthHeightString
    params :: [String]
    params =
      [ "-i"
      , inputFile
      , "-v"
      , "error"
      , "-select_streams"
      , "v:0"
      , "-show_entries"
      , "stream=width,height"
      , "-of"
      , "default=noprint_wrappers=1:nokey=1"
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
    , widthSize
    , quality
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
          , "    - Start Second: "  ++ printFloat startTime
          , "    - Duration Time: " ++ printFloat durationTime ++ " seconds"
          , "  - OUTPUT FILE SIZE:"
          , "    - Width Size: "    ++ show widthSize ++ "px"
          , "    - Quality: "       ++ show quality
          ]
      ++  if Prelude.null textOverlays
            then []
            else
                  [ "  - TEXT:"
                  ]
              ++
                  Prelude.foldl
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
                      ++  [ "    - Text: "         ++ textOverlayText
                          , "      - Font:"
                          , "        - Family: "   ++ textOverlayFontFamily
                          , "        - Size: "     ++ show textOverlayFontSize
                          , "        - Style: "    ++ textOverlayFontStyle
                          , "        - Stretch: "  ++ textOverlayFontStretch
                          , "        - Weight: "   ++ show textOverlayFontWeight
                          , "      - Time:"
                          , "        - Start: "    ++ printFloat textOverlayStartTime    ++ " seconds"
                          , "        - Duration: " ++ printFloat textOverlayDurationTime ++ " seconds"
                          , "      - Translation:"
                          , "        - Origin: "   ++ show textOverlayOrigin
                          , "        - X: "        ++ show textOverlayXTranslation
                          , "        - Y: "        ++ show textOverlayYTranslation
                          , "      - Rotation:"
                          , "        - Degrees: "  ++ show textOverlayRotation
                          , "      - Outline: "
                          , "        - Size: "     ++ show textOverlayOutlineSize
                          , "        - Color: "    ++ textOverlayOutlineColor
                          , "      - Fill:"
                          , "        - Color: "    ++ textOverlayFillColor
                          ]
                    )
                    []
                    textOverlays
      ++
          [ "  - CROP:"
          , "    - Left: "   ++ printFloat leftCrop
          , "    - Right: "  ++ printFloat rightCrop
          , "    - Top: "    ++ printFloat topCrop
          , "    - Bottom: " ++ printFloat bottomCrop
          ]
  where
    printFloat :: Float -> String
    printFloat = printf "%.3f"

frameFileExtension :: String
frameFileExtension = "png"

gifExtension :: String
gifExtension = "gif"

videoExtension :: String
videoExtension = "webm"

extractFrames :: GifParams -> String -> Float -> IO (Either IOError String)
extractFrames
  GifParams
    { inputFile
    , startTime
    , durationTime
    , widthSize
    , leftCrop
    , rightCrop
    , topCrop
    , bottomCrop
    }
  tempDir
  frameRate
  = do
  putStrLn $ "[INFO] Writing the temporary frames to: " ++ tempDir
  try $ readProcess "ffmpeg" params []
  where
    startTime' :: String
    startTime' = printf "%.3f" startTime
    durationTime' :: String
    durationTime' = printf "%.3f" durationTime
    widthSize' :: String
    widthSize' = show widthSize
    frameRate' :: String
    frameRate' = show frameRate
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
        ++ widthSize'
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
      , tempDir ++ "/extracted-frames_%010d." ++ frameFileExtension
      ]

annotateImage
  ::  GifParams
  ->  Float
  ->  Float
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
  result <- try $ readProcess "convert" params []
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
    position :: TextOverlayOrigin -> Float -> Float -> String
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
    x :: Float -> Float -> Float -> Float -> Float
    x f t lc rc = f * (originX t - (gifWidthLeftCrop  * lc) + (gifWidthRightCrop   * rc))
    y :: Float -> Float -> Float -> Float -> Float
    y f t tc bc = f * (originY t - (gifHeightTopCrop  * tc) + (gifHeightBottomCrop * bc))
    originX :: Float -> Float
    originX = (*) gifWidthNoCrop
    originY :: Float -> Float
    originY = (*) gifHeightNoCrop
    gifWidthLeftCrop :: Float
    gifWidthLeftCrop    = gifWidthNoCrop  * leftCrop
    gifWidthRightCrop :: Float
    gifWidthRightCrop   = gifWidthNoCrop  * rightCrop
    gifHeightTopCrop :: Float
    gifHeightTopCrop    = gifHeightNoCrop * topCrop
    gifHeightBottomCrop :: Float
    gifHeightBottomCrop = gifHeightNoCrop * bottomCrop
    neg :: Float
    neg = -1.0
    pos :: Float
    pos =  1.0
    toString :: Float -> String
    toString f
      | f >= 0.0  = "+" ++ show (abs (round f :: Int))
      | otherwise = "-" ++ show (abs (round f :: Int))
    rotation :: Int -> String
    rotation d = show d' ++ "x" ++ show d'
      where
        d' :: Int
        d' = mod d 360

mergeFramesIntoGif :: GifParams -> String -> Float -> IO (Either IOError String)
mergeFramesIntoGif
  GifParams
    { outputFile
    , quality
    }
  tempDir
  frameRate
  = do
  let outputFile' = gifOutputFile outputFile
  let (delay, colors, fuzz) = qualityAndFrameRateToGifSettings quality frameRate
  let params =
                [ "-quiet"
                ]
            ++  delay
            ++  [ tempDir ++ "/extracted-frames_*." ++ frameFileExtension
                , "+dither"
                ]
            ++  colors
            ++  fuzz
            ++  [ "-layers"
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
  result <- try $ readProcess "convert" params []
  if isLeft result
    then return result
    else return $ Right outputFile'

mergeFramesIntoVideo :: GifParams -> String -> Float -> IO (Either IOError String)
mergeFramesIntoVideo GifParams { outputFile, quality } tempDir frameRate = do
  let outputFile' = videoOutputFile outputFile
  let params =
        [ "-nostats"
        , "-loglevel"
        , "error"
        , "-y"
        , "-framerate"
        , show frameRate
        , "-start_number"
        , "0"
        , "-i"
        , tempDir ++ "/extracted-frames_%010d." ++ frameFileExtension
        , "-c:v"
        , "libvpx-vp9"
        , "-crf"
        , show $ targetQuality quality
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
  result <- try $ readProcess "ffmpeg" params []
  if isLeft result
    then return result
    else return (Right outputFile')
  where
    targetQuality :: Quality -> Int
    targetQuality QualityHigh   = 15
    targetQuality QualityMedium = 34
    targetQuality QualityLow    = 37

getVideoAverageFrameRateInSeconds :: GifParams -> IO (Maybe Float)
getVideoAverageFrameRateInSeconds GifParams { inputFile } = tryFfprobe params >>= result
  where
    result :: Either IOError String -> IO (Maybe Float)
    result (Left _)                   = return Nothing
    result (Right avgFrameRateString) = return $ processString avgFrameRateString
      where
        processString :: String -> Maybe Float
        processString =
          divideMaybeFloats . textsToMaybeFloats . filterNullTexts . splitText . cleanString
        cleanString :: String -> Text
        cleanString = Data.Text.strip . Data.Text.pack
        splitText :: Text -> [Text]
        splitText = Data.Text.split (== '/')
        filterNullTexts :: [Text] -> [Text]
        filterNullTexts = Data.List.filter (not . Data.Text.null)
        textsToMaybeFloats :: [Text] -> [Maybe Float]
        textsToMaybeFloats =
            Data.List.filter isJust
          . Data.List.map (\ s -> readMaybe (Data.Text.unpack s) :: Maybe Float)
        divideMaybeFloats :: [Maybe Float] -> Maybe Float
        divideMaybeFloats (Just n:Just d:_) =
          if d <= 0 || n <= 0 then Nothing else Just $ n / d
        divideMaybeFloats _ = Nothing
    params :: [String]
    params =
      [ "-v"
      , "error"
      , "-select_streams"
      , "v:0"
      , "-show_entries"
      , "stream=avg_frame_rate"
      , "-of"
      , "default=noprint_wrappers=1:nokey=1"
      , inputFile
      ]

tryFfprobe :: [String] -> IO (Either IOError String)
tryFfprobe = tryProcess "ffprobe"

tryProcess :: String -> [String] -> IO (Either IOError String)
tryProcess process params = try $ readProcess process params []

qualityAndFrameRateToGifSettings :: Quality -> Float -> ([String], [String], [String])
qualityAndFrameRateToGifSettings quality@QualityHigh frameRate =
  ( ["-delay"
    , qualityAndFrameRateToDelay quality frameRate
    ]
  , [ "-colors"
    , show $ toInt $ 256.0 * 1.0
    ]
  , [ "-fuzz"
    , "1%"
    ]
  )
qualityAndFrameRateToGifSettings quality@QualityMedium frameRate =
  ( [ "-delay"
    , qualityAndFrameRateToDelay quality frameRate
    ]
  , [ "-colors"
    , show $ toInt $ 256.0 * 0.75
    ]
  , [ "-fuzz"
    , "2%"
    ]
  )
qualityAndFrameRateToGifSettings quality@QualityLow frameRate =
  ( [ "-delay"
    , qualityAndFrameRateToDelay quality frameRate
    ]
  , [ "-colors"
    , show $ toInt $ 256.0 * 0.5
    ]
  , [ "-fuzz"
    , "3%"
    ]
  )

qualityAndFrameRateToDelay :: Quality -> Float -> String
qualityAndFrameRateToDelay quality frameRate =
  if delay <= 2
    then "2"
    else show delay
  where
    delay :: Int
    delay = toInt $ 100.0 / qualityAndFrameRateToFrameRate quality frameRate

qualityAndFrameRateToFrameRate :: Quality -> Float -> Float
qualityAndFrameRateToFrameRate    QualityHigh   frameRate = safeFrameRate $ 1.00 * frameRate
qualityAndFrameRateToFrameRate    QualityMedium frameRate = safeFrameRate $ 0.75 * frameRate
qualityAndFrameRateToFrameRate    QualityLow    frameRate = safeFrameRate $ 0.50 * frameRate

safeFrameRate :: Float -> Float
safeFrameRate frameRate
  | frameRate <= defaultFrameRate = defaultFrameRate
  | frameRate >= 50.0             = 50.0
  | otherwise                     = frameRate

defaultFrameRate :: Float
defaultFrameRate = 15.0

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

toInt :: Float -> Int
toInt = round

stripAndLowerString :: String -> String
stripAndLowerString =
  Data.Text.unpack . Data.Text.toLower . Data.Text.strip . Data.Text.pack

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
  , defaultGifParams
  , defaultFontChoice
  , gifParamsValid
  , versionNumber
  , getVideoDurationInSeconds
  , getOutputFileWithExtension
  , getVideoWidthAndHeight
  , findOrCreateTemporaryDirectory
  )
where

import System.Process
import System.IO.Temp
import System.Directory
import System.FilePath
import Text.Read
import Data.Maybe
import Data.List
import Data.Text
import Data.Either
import Text.Printf
import Control.Exception
import Control.Monad

-- | The data type record required by 'gif'.
data GifParams =
  GifParams
    { inputFile :: String
    , outputFile :: String
    , saveAsVideo :: Bool
    , startTime :: Float
    , durationTime :: Float
    , widthSize :: Int
    , qualityPercent :: Float
    , fontChoice :: String
    , topText :: String
    , bottomText :: String
    , leftCrop :: Float
    , rightCrop :: Float
    , topCrop :: Float
    , bottomCrop :: Float
    }
  deriving (Show, Read)

-- | The version number.
versionNumber :: String
versionNumber = "3.0.0.2"

-- | Specifies default parameters for 'startTime', 'durationTime', 'widthSize', 'qualityPercent', and 'fontChoice'.
defaultGifParams :: GifParams
defaultGifParams =
  GifParams
    { inputFile = ""
    , outputFile = ""
    , saveAsVideo = False
    , startTime = 0.0
    , durationTime = 1.0
    , widthSize = 500
    , qualityPercent = 100.0
    , fontChoice = defaultFontChoice
    , topText = ""
    , bottomText = ""
    , leftCrop = 0.0
    , rightCrop = 0.0
    , topCrop = 0.0
    , bottomCrop = 0.0
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
  gifParams@GifParams { saveAsVideo }
  = do
  temporaryDirectory <- findOrCreateTemporaryDirectory
  withTempDirectory temporaryDirectory "gifcurry-frames" $ \ tempDir ->
        handleFrameExtraction tempDir
    >>= handleFrameMerge tempDir
    >>= handleGifToVideoConversion
  where
    handleFrameExtraction :: String -> IO (Either IOError Float)
    handleFrameExtraction tempDir = do
      printGifParams gifParams
      validParams <- gifParamsValid gifParams
      if validParams
        then do
          frameRate <-
            validateAndAdjustFrameRate gifParams <$>
              getVideoAverageFrameRateInSeconds gifParams
          result <- extractFrames gifParams tempDir frameRate
          case result of
            Left x -> do
              putStrLn "[ERROR] Something went wrong with FFmpeg."
              return $ Left x
            Right _ -> return $ Right frameRate
        else return $ Left $ userError "Invalid params."
    handleFrameMerge :: String -> Either IOError Float -> IO (Either IOError String)
    handleFrameMerge tempDir (Right frameRate) = do
      fontMatch <- getFontMatch gifParams
      let gifParams' = gifParams { fontChoice = fontMatch }
      result <- mergeFramesIntoGif gifParams' tempDir frameRate
      case result of
        Left x -> do
          putStrLn "[ERROR] Something went wrong with ImageMagick."
          return $ Left x
        Right gifFilePath -> return $ Right gifFilePath
    handleFrameMerge _ (Left x) = return $ Left x
    handleGifToVideoConversion :: Either IOError String -> IO (Either IOError String)
    handleGifToVideoConversion (Right gifFilePath) =
      if saveAsVideo
        then do
          result <- convertGifToVideo gifParams gifFilePath
          case result of
            Left x -> do
              putStrLn "[ERROR] Something went wrong with FFmpeg."
              return $ Left x
            Right outputFileWithExtension -> do
              putStrLn "[INFO] All done."
              return $ Right outputFileWithExtension
        else do
          putStrLn "[INFO] All done."
          return $ Right gifFilePath
    handleGifToVideoConversion result@(Left _) = return result
    getFontMatch :: GifParams -> IO String
    getFontMatch GifParams { topText = "", bottomText = "" } = defaultFontMatch
    getFontMatch gifParams' = do
      fontNames <- getListOfFontNames
      let match = bestFontNameMatch (fontChoiceOrDefault gifParams') fontNames
      putStrLn $ "[INFO] Your font choice matched to \"" ++ match ++ "\"."
      return match
    defaultFontMatch :: IO String
    defaultFontMatch = return defaultFontChoice

-- | Outputs `True` or `False` if the parameters in the `GifParams` record are valid.
gifParamsValid :: GifParams -> IO Bool
gifParamsValid
  GifParams
    { inputFile
    , outputFile
    , startTime
    , durationTime
    , widthSize
    , qualityPercent
    , leftCrop
    , rightCrop
    , topCrop
    , bottomCrop
    }
  = do
  inputFileExists <-
    case Prelude.length inputFile of
      0 -> return False
      _ -> doesFileExist inputFile
  let widthSize' = fromIntegral widthSize :: Float
  let outputFileValid = not $ Data.Text.null $ Data.Text.strip $ Data.Text.pack outputFile
  let startTimeValid = startTime >= 0.0
  let durationTimeValid = durationTime > 0.0
  let widthSizeValid = widthSize >= 1
  let qualityPercentValid = qualityPercent >= 1.0 && qualityPercent <= 100.0
  let leftCropValid      = cropValid leftCrop
  let rightCropValid     = cropValid rightCrop
  let topCropValid       = cropValid topCrop
  let bottomCropValid    = cropValid bottomCrop
  let leftRightCropValid = cropValid (leftCrop + rightCrop)
  let topBottomCropValid = cropValid (topCrop + bottomCrop)
  let widthLeftRightCropSizeValid =
        (widthSize' - (widthSize' * (leftCrop / 100.0)) - (widthSize' * (rightCrop / 100.0))) >= 1.0
  unless inputFileExists              $ printError   "Input video file does not exist."
  unless outputFileValid              $ printInvalid "Output File"
  unless startTimeValid               $ printInvalid "Start Time"
  unless durationTimeValid            $ printInvalid "Duration Time"
  unless widthSizeValid               $ printInvalid "Width Size"
  unless qualityPercentValid          $ printInvalid "Quality Percent"
  unless leftCropValid                $ printInvalid "Left Crop"
  unless rightCropValid               $ printInvalid "Right Crop"
  unless topCropValid                 $ printInvalid "Top Crop"
  unless bottomCropValid              $ printInvalid "Bottom Crop"
  unless leftRightCropValid           $ printInvalid "Left and Right Crop"
  unless topBottomCropValid           $ printInvalid "Top and Bottom Crop"
  unless widthLeftRightCropSizeValid  $ printError   "Width Size too small with Left and Right Crop."
  let valid =
           inputFileExists
        && outputFileValid
        && startTimeValid
        && durationTimeValid
        && widthSizeValid
        && qualityPercentValid
        && leftCropValid
        && rightCropValid
        && topCropValid
        && bottomCropValid
        && widthLeftRightCropSizeValid
  return valid
  where
    cropValid :: Float -> Bool
    cropValid c = c >= 0.0 && c <= 100.0
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

-- | Returns the default font choice used if no font choice is specified.
defaultFontChoice :: String
defaultFontChoice = "sans-serif"

gifOutputFile :: String -> String
gifOutputFile outputFile =
  getOutputFileWithExtension $
  defaultGifParams { outputFile = outputFile, saveAsVideo = False }

videoOutputFile :: String -> String
videoOutputFile outputFile =
  getOutputFileWithExtension $
  defaultGifParams { outputFile = outputFile, saveAsVideo = True }

defaultFrameRate :: Float
defaultFrameRate = 15.0

validateAndAdjustFrameRate :: GifParams -> Maybe Float -> Float
validateAndAdjustFrameRate gifParams =
  frameRateBasedOnQualityPercent gifParams . maybeFrameRateOrDefaultFrameRate

maybeFrameRateOrDefaultFrameRate :: Maybe Float -> Float
maybeFrameRateOrDefaultFrameRate (Just frameRate) =
  if frameRate <= defaultFrameRate then defaultFrameRate else frameRate
maybeFrameRateOrDefaultFrameRate Nothing = defaultFrameRate

frameRateBasedOnQualityPercent :: GifParams -> Float -> Float
frameRateBasedOnQualityPercent GifParams { qualityPercent } frameRate =
  if result <= defaultFrameRate then defaultFrameRate else result
  where
    result :: Float
    result = frameRate * (qualityPercent / 100.0)

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
tryFfprobe params = try $ readProcess "ffprobe" params []

printGifParams :: GifParams -> IO ()
printGifParams
  gifParams@GifParams
    { inputFile
    , saveAsVideo
    , startTime
    , durationTime
    , widthSize
    , qualityPercent
    , fontChoice
    , topText
    , bottomText
    , leftCrop
    , rightCrop
    , topCrop
    , bottomCrop
    }
  =
  putStrLn $
    Prelude.unlines
      [ "[INFO] Here are your settings."
      , ""
      , "  - FILE IO:"
      , "    - Input File: " ++ inputFile
      , "    - Output File: " ++ getOutputFileWithExtension gifParams
      , "    - Save As Video: " ++ if saveAsVideo then "Yes" else "No"
      , "  - TIME:"
      , "    - Start Second: " ++ printFloat startTime
      , "    - Duration Time: " ++ printFloat durationTime ++ " seconds"
      , "  - OUTPUT FILE SIZE:"
      , "    - Width Size: " ++ show widthSize ++ "px"
      , "    - Quality Percent: " ++ show (qualityPercentClamp qualityPercent) ++ "%"
      , "  - TEXT:"
      , "    - Font Choice: " ++ fontChoice
      , "    - Top Text: " ++ topText
      , "    - Bottom Text: " ++ bottomText
      , "  - CROP:"
      , "    - Left Crop: " ++ printFloat leftCrop
      , "    - Right crop: " ++ printFloat rightCrop
      , "    - Top Crop: " ++ printFloat topCrop
      , "    - Bottom Crop: " ++ printFloat bottomCrop
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
    frameRate' = show $ maybeFrameRateOrDefaultFrameRate (Just frameRate)
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
        ++ show ((leftCrop + rightCrop) / 100.0)
        ++ "):h=ih*(1-"
        ++ show ((topCrop + bottomCrop) / 100.0)
        ++ "):x=iw*"
        ++ show (leftCrop / 100.0)
        ++ ":y=ih*"
        ++ show (topCrop / 100.0)
        ++ ":exact=1"
      , "-f"
      , "image2"
      , tempDir ++ "/%010d." ++ frameFileExtension
      ]

mergeFramesIntoGif :: GifParams -> String -> Float -> IO (Either IOError String)
mergeFramesIntoGif
  GifParams
    { outputFile
    , saveAsVideo
    , qualityPercent
    , fontChoice
    , topText
    , bottomText
    }
  tempDir
  frameRate
  = do
  maybeWidthHeight <-
    maybeGetFirstFrameFilePath tempDir >>=
      maybeGetFirstFrameWidthHeight
  let frameRate'  = maybeFrameRateOrDefaultFrameRate (Just frameRate)
  let delay       = show $ 100.0 / frameRate'
  let outputFile' =
        if saveAsVideo
          then tempDir ++ "/finished-result.gif"
          else gifOutputFile outputFile
  let params =
            [ "-quiet"
            , "-delay"
            , delay
            , tempDir ++ "/*." ++ frameFileExtension
            ]
        ++  annotate fontChoice maybeWidthHeight topText    "north"
        ++  annotate fontChoice maybeWidthHeight bottomText "south"
        ++  [ "+dither"
            , "-colors"
            , show $ numberOfColors qualityPercent
            , "-fuzz"
            , "2%"
            , "-layers"
            , "OptimizeFrame"
            , "-layers"
            , "OptimizeTransparency"
            , "-loop"
            , "0"
            , "+map"
            , outputFile'
            ]
  putStrLn $ "[INFO] Saving your GIF to: " ++ outputFile'
  result <- try $ readProcess "convert" params []
  if isLeft result
    then return result
    else return $ Right outputFile'

convertGifToVideo :: GifParams -> String -> IO (Either IOError String)
convertGifToVideo GifParams { outputFile } gifFilePath = do
  let outputFile' = videoOutputFile outputFile
  let params =
        [ "-nostats"
        , "-loglevel"
        , "error"
        , "-y"
        , "-i"
        , gifFilePath
        , "-c:v"
        , "libvpx-vp9"
        , "-pix_fmt"
        , "yuva420p"
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

qualityPercentClamp :: Float -> Float
qualityPercentClamp qualityPercent
  | qualityPercent > 100.0   = 100.0
  | qualityPercent < 0.0     = 1.0
  | otherwise                = qualityPercent

numberOfColors :: Float -> Int
numberOfColors qualityPercent
  | qualityPercentClamp qualityPercent <=   1.0 = 2
  | qualityPercentClamp qualityPercent >= 100.0 = floor maxColors
  | otherwise                                   = truncate $ (qualityPercent / 100.0) * maxColors
  where
    maxColors :: Float
    maxColors = 256.0

annotate :: String -> Maybe (Int, Int) -> String -> String -> [String]
annotate _             _                ""   _       = []
annotate fontChoiceArg maybeWidthHeight text gravity =
      [ "-gravity"
      , gravity
      ]
  ++  fontSetting fontChoiceArg
  ++  [ "-stroke"
      , "#000C"
      , "-strokewidth"
      , "10"
      , "-density"
      , "96"
      , "-pointsize"
      , pointsize
      , "-annotate"
      , "+0+10"
      , text
      , "-stroke"
      , "none"
      , "-fill"
      , "white"
      , "-density"
      , "96"
      , "-pointsize"
      , pointsize
      , "-annotate"
      , "+0+10"
      , text
      ]
  where
    pointsize :: String
    pointsize = show $ pointSize maybeWidthHeight text

-- @96 PPI: w 71 px x h 96 px
pointSize :: Maybe (Int, Int) -> String -> Int
pointSize Nothing _ = 0
pointSize (Just (width, height)) text
  | width <= 0 || height <= 0 = 0
  | textLength           <= 0 = 0
  | otherwise                 = Prelude.minimum [widthLTHeight, widthGTEHeight]
  where
    textLength :: Int
    textLength = Prelude.length text
    width' :: Double
    width'  = fromIntegral width
    height' :: Double
    height' = fromIntegral height
    textLength' :: Double
    textLength' = fromIntegral textLength
    widthLTHeight :: Int
    widthLTHeight  = truncate $ ((width' * (5.0 / 7.0)) / textLength') * (96.0 / 71.0)
    widthGTEHeight :: Int
    widthGTEHeight = truncate $ height' * (1.0 / 5.0)

fontSetting :: String -> [String]
fontSetting ""        = []
fontSetting font      = ["-font", font]

bestFontNameMatch :: String -> [Text] -> String
bestFontNameMatch _ []            = "default"
bestFontNameMatch _ [""]          = "default"
bestFontNameMatch query fontNames = Data.Text.unpack $ bestMatch $ maximumMatch $ Data.Text.pack query
  where
    bestMatch :: (Int, Text) -> Text
    bestMatch (s, f) = if s <= 0 then "default" else f
    maximumMatch :: Text -> (Int, Text)
    maximumMatch query' =
      maximumBy (\ (ls, _) (rs, _) -> if ls >= rs then GT else LT) $
        Prelude.map (\ fontName -> (score query' (Data.Text.toLower fontName), fontName)) fontNames
    score :: Text -> Text -> Int
    score query' fontName = sum $ Prelude.map tokenScore (queryTokens query')
      where
        queryTokens :: Text -> [Text]
        queryTokens = Prelude.map cleanQueryToken . Data.Text.splitOn " "
          where
            cleanQueryToken :: Text -> Text
            cleanQueryToken = Data.Text.replace "," "" . Data.Text.toLower . Data.Text.strip
        tokenScore :: Text -> Int
        tokenScore token
          | Data.Text.length token < 1 = 0
          | Data.Text.isInfixOf token fontName = isInfixOfFontName token
          | otherwise = 0
          where
            isInfixOfFontName :: Text -> Int
            isInfixOfFontName token'
              | token' `elem` ["bold", "medium", "light", "regular", "italic"] = 1
              | isNothing (readMaybe (Data.Text.unpack token') :: Maybe Int) = 3
              | otherwise = 0

getListOfFontNames :: IO [Text]
getListOfFontNames = do
  (_, stdout, _) <- readProcessWithExitCode "convert" ["-list", "font"] []
  let fontNames =
        Prelude.map (Data.Text.strip . Data.Text.drop 5 . Data.Text.strip) $
        Prelude.filter (Data.Text.isInfixOf "font:" . Data.Text.toLower) $
        Data.Text.splitOn "\n" $
        Data.Text.strip $
        Data.Text.pack stdout
  return fontNames

maybeGetFirstFrameFilePath :: String -> IO (Maybe FilePath)
maybeGetFirstFrameFilePath tempDir =
  try (makeAbsolute tempDir) >>= tryListDir >>= maybeFirstFilePath
  where
    tryListDir :: Either IOError FilePath -> IO (FilePath, Either IOError [FilePath])
    tryListDir (Left y) = return ("", Left y)
    tryListDir (Right dir) = try (listDirectory dir) >>= \ e -> return (dir, e)
    maybeFirstFilePath :: (FilePath, Either IOError [FilePath]) -> IO (Maybe FilePath)
    maybeFirstFilePath (_,   Left  _)     = return Nothing
    maybeFirstFilePath (_,   Right [])    = return Nothing
    maybeFirstFilePath (dir, Right (x:_)) = return (Just (normalise $ joinPath [dir, x]))

maybeGetFirstFrameWidthHeight :: Maybe FilePath -> IO (Maybe (Int, Int))
maybeGetFirstFrameWidthHeight Nothing = return Nothing
maybeGetFirstFrameWidthHeight (Just dir) =
  readProcessWithExitCode "identify" [dir] [] >>=
    \ (_, stdout, _) ->
      maybeConvertWidthHeightString $
      findWidthHeightString $
      splitOn " " $
      Data.Text.pack stdout
  where
    findWidthHeightString :: [Text] -> Text
    findWidthHeightString (_:_:c:_:_:_:_:_:_:_) = c
    findWidthHeightString _ = ""
    maybeConvertWidthHeightString :: Text -> IO (Maybe (Int, Int))
    maybeConvertWidthHeightString "" = return Nothing
    maybeConvertWidthHeightString s =
      if Prelude.length splitOnX == 2
        then return (Just (pluckWidth splitOnX, pluckHeight splitOnX))
        else return Nothing
      where
        splitOnX :: [Text]
        splitOnX = splitOn "x" $ Data.Text.toLower s
        pluckWidth :: [Text] -> Int
        pluckWidth (x:_:_) = read (Data.Text.unpack x) :: Int
        pluckWidth _        = 0
        pluckHeight :: [Text] -> Int
        pluckHeight (_:y:_) = read (Data.Text.unpack y) :: Int
        pluckHeight _       = 0

fontChoiceOrDefault :: GifParams -> String
fontChoiceOrDefault GifParams { fontChoice = fontName } =
  if Data.List.null cleanedFontName
    then defaultFontChoice
    else cleanedFontName
  where
    cleanedFontName :: String
    cleanedFontName = (Data.Text.unpack . Data.Text.strip . Data.Text.pack) fontName

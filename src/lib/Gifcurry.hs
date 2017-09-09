{-
  Gifcurry
  (C) 2016 David Lettier
  lettier.com
-}

{-# LANGUAGE OverloadedStrings #-}

-- | Produces GIFs using FFmpeg and ImageMagick.
-- The main function is 'gif'.
module Gifcurry (
      gif
    , GifParams(..)
    , defaultGifParams
    , gifParamsValid
    , versionNumber
  ) where

import System.Process
import System.IO.Temp
import System.Directory
import System.FilePath
import Text.Read
import Data.Maybe
import Data.List
import Data.Text
import Text.Printf
import Control.Exception
import Control.Monad

-- | The data type record required by 'gif'.
data GifParams = GifParams {
      inputFile :: String
    , outputFile :: String
    , startTime :: Float
    , durationTime :: Float
    , widthSize :: Int
    , qualityPercent :: Float
    , fontChoice :: String
    , topText :: String
    , bottomText :: String
  } deriving (Show, Read)

-- | The version number.
versionNumber :: String
versionNumber = "2.1.1.0"

-- | Specifies default parameters for 'startTime', 'durationTime', 'widthSize', 'qualityPercent', and 'fontChoice'.
defaultGifParams :: GifParams
defaultGifParams = GifParams {
      inputFile = ""
    , outputFile = ""
    , startTime = 0.0
    , durationTime = 1.0
    , widthSize = 500
    , qualityPercent = 100.0
    , fontChoice = "default"
    , topText = ""
    , bottomText = ""
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
gif gifParams =
  withTempDirectory "." "frames" $ \tmpdir -> do
    printGifParams gifParams tmpdir
    validParams <- gifParamsValid gifParams
    if validParams
      then do
        fFMpegResult  <- tryFfmpeg gifParams tmpdir
        let fFMpegSuccess = eitherBool fFMpegResult
        if fFMpegSuccess
          then do
            fontMatch <- getFontMatch gifParams
            let gifParams' = gifParams { fontChoice = fontMatch }
            putStrLn $ "Writing your GIF to... " ++ outputFile gifParams
            convertResult <- tryConvert gifParams' tmpdir
            let convertSuccess = eitherBool convertResult
            if convertSuccess
              then putStrLn "Done."
              else putStrLn "[Error] Something went wrong with ImageMagick."
            return convertResult
          else do
            putStrLn "[Error] Something went wrong with FFmpeg."
            return fFMpegResult
      else return $ Left (userError "[Error] Invalid params.")
  where
    runFontMatch :: GifParams -> Bool
    runFontMatch GifParams { fontChoice = _, topText = "", bottomText = "" } = False
    runFontMatch GifParams { fontChoice = "default", topText = _, bottomText = _ } = False
    runFontMatch _ = True
    getFontMatch :: GifParams -> IO String
    getFontMatch gifParams'
      | runFontMatch gifParams' = do
        fontNames <- getListOfFontNames
        let match = bestFontNameMatch (fontChoice gifParams') fontNames
        putStrLn $ "Font matched: " ++ match
        return match
      | otherwise = defaultAction
      where
        defaultAction :: IO String
        defaultAction = putStrLn "Using the default font." >> return "default"
    eitherBool :: Either a b -> Bool
    eitherBool = either (const False) (const True)

-- | Outputs True or False if the parameters in the GifParams record are valid.
-- Looks at 'inputFile', 'outputFile', 'startTime', 'durationTime', 'widthSize', and 'qualityPercent'.
gifParamsValid :: GifParams -> IO Bool
gifParamsValid GifParams {
      inputFile = ipf
    , outputFile = opf
    , startTime = st
    , durationTime = dt
    , widthSize = ws
    , qualityPercent = qp
    , topText = _
    , bottomText = _
  } = do
    inputFileExists <- case Prelude.length ipf of
      0 -> return False
      _ -> doesFileExist ipf
    unless inputFileExists $ putStrLn "\n[Error] Input video file does not exist."
    let outputFileValid = Prelude.length opf > 5
    unless outputFileValid $ putStrLn "\n[Error] Output GIF file is blank."
    let valid = inputFileExists && outputFileValid && (st >= 0.0) && (dt >= 0.0) && (ws > 0) && (qp > 0.0)
    unless valid $ putStrLn "\n[Error] Invalid params."
    return valid

printGifParams :: GifParams -> String -> IO ()
printGifParams
  GifParams {
      inputFile = ipf
    , outputFile = opf
    , startTime = st
    , durationTime = dt
    , widthSize = ws
    , qualityPercent = qp
    , fontChoice = fc
    , topText = tt
    , bottomText = bt
  }
  tmpdir = mapM_ putStrLn [
        "\nInput file: " ++ ipf
      , "Output file: " ++ opf
      , "Start second: " ++ printf "%.3f" st
      , "Duration: " ++ printf "%.3f" dt ++ " seconds"
      , "GIF width: " ++ show ws ++ "px"
      , "Quality: " ++ show (qualityPercentClamp qp) ++ "%"
      , "Font Choice: " ++ fc
      , "Top text: " ++ tt
      , "Bottom text: " ++ bt
      , "\nWriting temporary frames to... " ++ tmpdir
    ]

frameFileExtension :: String
frameFileExtension = "png"

tryFfmpeg :: GifParams -> String -> IO (Either IOError String)
tryFfmpeg
  GifParams {
      inputFile = ipf
    , startTime = st
    , durationTime = dt
    , widthSize = ws
  }
  tmpdir = try(readProcess "ffmpeg" params [])
  where
    sts = printf "%.3f" st
    dts = printf "%.3f" dt
    wss = show ws
    params = [
          "-nostats"
        , "-loglevel"
        , "panic"
        , "-an"
        , "-ss"
        , sts
        , "-i"
        , ipf
        , "-t"
        , dts
        , "-r"
        , "12"
        , "-q:v"
        , "31"
        , "-vf"
        , "scale=" ++ wss ++ ":-1"
        , "-f"
        , "image2"
        , tmpdir ++ "/%010d." ++ frameFileExtension
      ]

tryConvert :: GifParams -> String -> IO (Either IOError String)
tryConvert
  GifParams {
      outputFile = opf
    , qualityPercent = qp
    , fontChoice = fc
    , topText = tt
    , bottomText = bt
  }
  tmpdir = do
    maybeWidthHeight <- maybeGetFirstFrameFilePath tmpdir >>= maybeGetFirstFrameWidthHeight
    let params = [
                    "-quiet"
                  , "-delay"
                  , "8.3"
                  , tmpdir ++ "/*." ++ frameFileExtension
                  , "-coalesce"
                  , "-colors"
                  , show $ ncolors qp
                  , "-dither"
                  , "FloydSteinberg"
                  , "-layers"
                  , "remove-dups"
                  , "-layers"
                  , "compare-any"
                  , "-layers"
                  , "optimize-transparency"
                  , "-loop"
                  , "0"
                ]
                ++ annotate fc maybeWidthHeight tt "north"
                ++ annotate fc maybeWidthHeight bt "south"
                ++ [opf]
    try (readProcess "convert" params [])

qualityPercentClamp :: Float -> Float
qualityPercentClamp qp
  | qp > 100.0   = 100.0
  | qp < 0.0     = 2.0
  | otherwise    = qp

ncolors :: Float -> Int
ncolors qp
  | qpc < 0.0    = 1
  | qpc >= 100.0 = 256
  | otherwise  = truncate (qpc / 100.0 * 256.0)
  where
    qpc = qualityPercentClamp qp

annotate :: String -> Maybe (Int, Int) -> String -> String -> [String]
annotate fontChoiceArg maybeWidthHeight text topBottom = [
      "-gravity"
    , topBottom
  ] ++ fontSetting fontChoiceArg ++ [
      "-stroke"
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
fontSetting "default" = []
fontSetting fc        = ["-font", fc]

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
  let fontNames = Prelude.map (Data.Text.strip . Data.Text.drop 5 . Data.Text.strip) $
                    Prelude.filter (Data.Text.isInfixOf "font:" . Data.Text.toLower) $
                      Data.Text.splitOn "\n" $
                        Data.Text.strip $
                          Data.Text.pack stdout
  return fontNames

maybeGetFirstFrameFilePath :: String -> IO (Maybe FilePath)
maybeGetFirstFrameFilePath tmpdir = try (makeAbsolute tmpdir) >>= tryListDir >>= maybeFirstFilePath
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
    \ (_, stdout, _) -> maybeConvertWidthHeightString $ findWidthHeightString $ splitOn " " $ Data.Text.pack stdout
  where
    findWidthHeightString :: [Text] -> Text
    findWidthHeightString (_:_:c:_:_:_:_:_:_:_) = c
    findWidthHeightString _ = ""
    maybeConvertWidthHeightString :: Text -> IO (Maybe (Int, Int))
    maybeConvertWidthHeightString "" = return Nothing
    maybeConvertWidthHeightString s = if Prelude.length splitOnX == 2
                                        then return (Just (pluckWidth splitOnX, pluckHeight splitOnX))
                                        else return Nothing
      where
        splitOnX :: [Text]
        splitOnX = splitOn "x" $ Data.Text.toLower s
        pluckWidth :: [Text] -> Int
        pluckWidth  (x:_:_) = read (Data.Text.unpack x) :: Int
        pluckWidth _        = 0
        pluckHeight :: [Text] -> Int
        pluckHeight (_:y:_) = read (Data.Text.unpack y) :: Int
        pluckHeight _       = 0

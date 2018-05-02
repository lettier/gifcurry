{-
  Gifcurry
  (C) 2016 David Lettier
  lettier.com
-}

{-# LANGUAGE
    DeriveDataTypeable
  , NamedFieldPuns
#-}
{-# OPTIONS_GHC -fno-cse #-}

import Control.Monad
import System.Console.CmdArgs

import qualified Gifcurry

data CliArgs =
  CliArgs
    { input_file :: String
    , output_file :: String
    , save_as_video :: Bool
    , start_time :: Float
    , duration_time :: Float
    , width_size :: Int
    , quality_percent :: Float
    , font_choice :: String
    , top_text :: String
    , bottom_text :: String
    , left_crop :: Float
    , right_crop :: Float
    , top_crop :: Float
    , bottom_crop :: Float
    }
  deriving (Data, Typeable, Show, Eq)

programName :: String
programName = "gifcurry_cli"

info :: String -> String
info art =
  unlines
    [ art
    , "Gifcurry" ++ " " ++ Gifcurry.versionNumber
    , "(C) 2016 David Lettier"
    ,"lettier.com"
    ]

cliArgs :: CliArgs
cliArgs =
  CliArgs
    { input_file
        = ""
        &= groupname "FILE IO"
        &= typFile
        &= help "The input video file path and name."
    , output_file
        = ""
        &= groupname "FILE IO"
        &= typFile
        &= help "The output GIF file path and name."
    , save_as_video
        = False
        &= groupname "FILE IO"
        &= name "m"
        &= help "If present, saves the GIF as a video."
    , start_time
        = 0.0
        &= groupname "TIME"
        &= name "s"
        &= help "The start time (in seconds) for the first frame."
    , duration_time
        = 1.0
        &= groupname "TIME"
        &= help "How long the GIF lasts (in seconds) from the start time."
    , width_size
        = 500
        &= groupname "OUTPUT FILE SIZE"
        &= help "How wide the GIF needs to be. Height will scale to match."
    , quality_percent
        = 100.0
        &= groupname "OUTPUT FILE SIZE"
        &= help
          (  "From 1 (very low quality) to 100 (the best quality). "
          ++ "Controls how many colors are used and how many frames per second there are."
          )
    , font_choice
        = Gifcurry.defaultFontChoice
        &= groupname "TEXT"
        &= typ "TEXT"
        &= help "Choose your desired font for the top and bottom text."
    , top_text
        = ""
        &= groupname "TEXT"
        &= typ "TEXT"
        &= name "t"
        &= help "The text you wish to add to the top of the GIF."
    , bottom_text
        = ""
        &= groupname "TEXT"
        &= typ "TEXT"
        &= name "b"
        &= help "The text you wish to add to the bottom of the GIF."
    , left_crop
        = 0.0
        &= groupname "CROP"
        &= name "L"
        &= help "The amount you wish to crop from the left."
    , right_crop
        = 0.0
        &= groupname "CROP"
        &= name "R"
        &= help "The amount you wish to crop from the right."
    , top_crop
        = 0.0
        &= groupname "CROP"
        &= name "T"
        &= help "The amount you wish to crop from the top."
    , bottom_crop
        = 0.0
        &= groupname "CROP"
        &= name "B"
        &= help "The amount you wish to crop from the bottom."
    }
    &= summary (info icon)
    &= program programName
    &= details ["Visit https://github.com/lettier/gifcurry for more information.", ""]

main :: IO ()
main = do
  cliArgs' <- cmdArgs cliArgs
  let params = makeGifParams cliArgs'
  putStrLn $ info logo
  paramsValid <- Gifcurry.gifParamsValid params
  if paramsValid
    then void $ Gifcurry.gif params
    else
      putStrLn $
        "[INFO] Type \"" ++ programName ++ " -?\" for help."
  return ()

makeGifParams :: CliArgs -> Gifcurry.GifParams
makeGifParams
  CliArgs
    { input_file
    , output_file
    , save_as_video
    , start_time
    , duration_time
    , width_size
    , quality_percent
    , font_choice
    , top_text
    , bottom_text
    , left_crop
    , right_crop
    , top_crop
    , bottom_crop
    }
  =
  Gifcurry.GifParams
    { Gifcurry.inputFile      = input_file
    , Gifcurry.outputFile     = output_file
    , Gifcurry.saveAsVideo    = save_as_video
    , Gifcurry.startTime      = start_time
    , Gifcurry.durationTime   = duration_time
    , Gifcurry.widthSize      = width_size
    , Gifcurry.qualityPercent = quality_percent
    , Gifcurry.fontChoice     = font_choice
    , Gifcurry.topText        = top_text
    , Gifcurry.bottomText     = bottom_text
    , Gifcurry.leftCrop       = left_crop
    , Gifcurry.rightCrop      = right_crop
    , Gifcurry.topCrop        = top_crop
    , Gifcurry.bottomCrop     = bottom_crop
    }

logo :: String
logo =
  unlines
    [ ""
    , "         ppDPPPDbDDpp                                                                                    "
    , "      pDPPPP       )DPDp                      )                                                          "
    , "       PPPPP   )pp    DPPp          ppppp    PPP    pDbDD                                                "
    , "   p   )PPP    PPPD    PPPD      pDPDPPPDP         PPP                                                   "
    , "  bP    DPP   pPPP     )PPPb    (PPP         PPP )PPPPPP  pDPPPDb PPP   PPb  PPbpDPP PPbpPP ·DPb   pPD   "
    , " (PPb   )D   (PPD       bPPP    PPP   DDDDD  PPP   PPP   PPb      PPP   PPb  PPPP    PPPP    (PP  pPPC   "
    , " (PPPp       PPP    b   )PPP    DPPp    PPP  PPP   PPP  (PPb      PPP   PPb  PPP     PPP      DPb PPP    "
    , "  PPPb      DPPP   pPp   DPb     DPDp   PPP  PPP   PPP   DPPp  p  PPP  pPPb  PPP     PPP       PPpPP     "
    , "  )PPPp   (DPPP   )PPb    b       (PPDDPPP   PPP   PPP    (PDDDPC  PDDP PPC  PPP     PPP       )DPPP     "
    , "   )DPPp   )DD    DPPPb                                                                        pbPP      "
    , "     )DPbp       (PPPPPb                                                                      PPC        "
    , "        SPDbDppppPPDPC                                                                                   "
    , ""
    ]

icon :: String
icon =
  unlines
    [ ""
    , "         ppDPPPDbDDpp        "
    , "      pDPPPP       )DPDp     "
    , "       PPPPP   )pp    DPPp   "
    , "   p   )PPP    PPPD    PPPD  "
    , "  bP    DPP   pPPP     )PPPb "
    , " (PPb   )D   (PPD       bPPP "
    , " (PPPp       PPP    b   )PPP "
    , "  PPPb      DPPP   pPp   DPb "
    , "  )PPPp   (DPPP   )PPb    b  "
    , "   )DPPp   )DD    DPPPb      "
    , "     )DPbp       (PPPPPb     "
    , "        SPDbDppppPPDPC       "
    , ""
    ]

-- David Lettier (C) 2016. http://www.lettier.com/

import System.Environment
import System.Process
import System.IO.Temp
import System.Exit
import Data.List

import Gifcurry (gif)

main :: IO ()
main = do
  args <- getArgs
  let processed_args = process_args args
  prntheader
  if length processed_args == 0
    then do
      putStrLn "\n  Usage: $ gifcurry_cli input_file output_file start_sec duration width quality top_text bottom_text"
      putStrLn "Example: $ gifcurry_cli ./in.mp4 ./out.gif 3 10 600 80 'Top' 'Bottom'"
      putStrLn "Example: $ gifcurry_cli ./in.mp4 ./out.gif 3 10 600 80 'Top'"
      putStrLn "Example: $ gifcurry_cli ./in.mp4 ./out.gif 3 10 600 80"
      putStrLn "Example: $ gifcurry_cli ./in.mp4 ./out.gif 3 10 600"
      putStrLn "Example: $ gifcurry_cli ./in.mp4 ./out.gif 3 10"
      exitWith (ExitFailure 1)
    else do
      result <- gif $ take 8 processed_args
      return ()

prntheader = do
  putStrLn " _____ _  __                           "
  putStrLn "|  __ (_)/ _|                          "
  putStrLn "| |  \\/_| |_ ___ _   _ _ __ _ __ _   _ "
  putStrLn "| | __| |  _/ __| | | | '__| '__| | | |"
  putStrLn "| |_\\ \\ | || (__| |_| | |  | |  | |_| |"
  putStrLn " \\____/_|_| \\___|\\__,_|_|  |_|   \\__, |"
  putStrLn "                                  __/ |"
  putStrLn "                                 |___/ "
  putStrLn "\nGifcurry (C) 2016 David Lettier. http://www.lettier.com/"

process_args args
  | length args == 8 = args
  | length args == 7 = args ++ [""]
  | length args == 6 = args ++ ["", ""]
  | length args == 5 = args ++ ["100", "", ""]
  | length args == 4 = args ++ ["500", "100", "", ""]
  | otherwise        = []

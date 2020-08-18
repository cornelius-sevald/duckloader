module Main where

import           System.IO
import           System.Exit

import           DuckLoader

main :: IO ()
main = do
  contents       <- getContents
  (result, logs) <- playlistFromIDs contents
  hPutStr stderr (unlines $ map ("> " ++) logs)
  case result of
    Left errorMsg -> do
      hPutStrLn stderr "ERROR!"
      hPutStrLn stderr (show errorMsg)
      exitWith (ExitFailure 1)
    Right playlistStr -> do
      putStrLn playlistStr
      exitWith (ExitSuccess)

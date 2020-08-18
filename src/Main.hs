module Main where

import           System.IO
import           System.Exit
import qualified Data.ByteString.Lazy.Char8    as C

import           DuckLoader

main :: IO ()
main = do
  contents       <- C.getContents
  (result, logs) <- playlistFromIDs contents
  hPutStr stderr (unlines $ map ("> " ++) logs)
  case result of
    Left errorMsg -> do
      hPutStrLn stderr "ERROR!"
      hPutStrLn stderr (show errorMsg)
      exitWith (ExitFailure 1)
    Right playlistStr -> do
      C.putStrLn playlistStr
      exitWith (ExitSuccess)

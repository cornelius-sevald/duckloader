{-# LANGUAGE CPP #-}
module Main where

import System.Process (readProcess)
import System.Directory (listDirectory)
import Control.Monad (guard)
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Writer
import Control.Monad.IO.Class
import Data.List (isSuffixOf)
import Data.Maybe (fromMaybe)
import Data.Either (either)

import           Path

import qualified Parsing.WorkshopID            as WID
import qualified Parsing.SteamCMD              as CMD

#if defined(mingw32_HOST_OS)
playlistPrefix = ""
#else
playlistPrefix = "Z:"
#endif
steamcmd = "steamcmd"

args :: [String] -> [String]
args wids = ["+login", "anonymous"]
         ++ foldMap (\wid -> ["+workshop_download_item", "312530"] ++ [wid]) wids
         ++ ["+quit"]

levelPath :: Path Abs Dir -> IO (Maybe (Path Abs File))
levelPath dir = runMaybeT $ do
  levels <- liftIO $ listDirectory $ fromAbsDir dir
  [level] <- mapM parseRelFile levels
  guard $ ".lev" `isSuffixOf` (toFilePath level)
  return $ dir </> level

playlist :: [Path Abs File] -> String
playlist levels = "<playlist>\n"
               ++ foldMap (\lev ->
                  "  <element>"
               ++ playlistPrefix
               ++ toFilePath lev
               ++ "</element>\n") levels
               ++ "</playlist>"

playlistFromIDs :: MonadIO m => String -> WriterT [String] m String
playlistFromIDs contents = do
  tell ["Parsing workshop IDs."]
  let wids    = either (error . show) id (WID.parseWorkshopIDs contents)
  let cmdArgs = args wids
  tell ["Running steamcmd with args: '" ++ unwords cmdArgs ++ "'."]
  output     <- liftIO $ readProcess steamcmd cmdArgs ""
  tell ["Parsing steamcmd output."]
  let paths   = either (error . show) id (CMD.parseSteamCMD output)
  tell ["Getting level-file paths."]
  levPaths   <- liftIO $ mapM (fmap (fromMaybe (error "Not just single level file")) . levelPath) paths
  tell ["Building playlist string."]
  return $ playlist levPaths

main :: IO ()
main = do
  contents <- getContents
  (playlistStr, logs) <- runWriterT $ playlistFromIDs contents
  putStrLn playlistStr

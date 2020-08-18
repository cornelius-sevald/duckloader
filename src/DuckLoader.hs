{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module DuckLoader
  ( playlistFromIDs
  )
where

import           System.Exit
import           System.Process                 ( readProcessWithExitCode )
import           System.Directory               ( listDirectory )
import           Control.Error.Util
import           Control.Monad.Except
import           Control.Monad.Writer
import           Control.Monad.Trans
import           Control.Monad.IO.Class
import           Data.List                      ( isSuffixOf )
import           Data.EitherR                   ( fmapL )

import           Path
import qualified Text.Parsec                   as Parsec

import qualified Parsing.WorkshopID            as WID
import qualified Parsing.SteamCMD              as CMD

#if defined(mingw32_HOST_OS)
playlistPrefix = ""
steamcmd = "steamcmd.exe"
#else
playlistPrefix = "Z:"
steamcmd = "steamcmd"
#endif

data DuckError
 = InvalidPath String
 | NoLevelFile
 | TooManyLevelFiles Int
 | ParseError Parsec.ParseError
 | SteamCMDError String
 deriving Show

type DuckLog = [String]
type DuckIDs = String
type DuckPlaylist = String

args :: [String] -> [String]
args wids =
  ["+login", "anonymous"]
    ++ foldMap (\wid -> ["+workshop_download_item", "312530"] ++ [wid]) wids
    ++ ["+quit"]

levelPath :: MonadIO m => Path Abs Dir -> ExceptT DuckError m (Path Abs File)
levelPath dir = do
  levels <- liftIO $ listDirectory $ fromAbsDir dir
  level  <-
    mapM (\lev -> noteT (InvalidPath lev) $ hoistMaybe $ parseRelFile lev)
         levels
      >>= \case
            [level] -> return level
            []      -> throwError NoLevelFile
            lvs     -> throwError (TooManyLevelFiles $ length lvs)
  unless (".lev" `isSuffixOf` (toFilePath level)) (throwError NoLevelFile)
  return $ dir </> level


playlist :: [Path Abs File] -> DuckPlaylist
playlist levels =
  "<playlist>\n"
    ++ foldMap
         (\lev ->
           "  <element>" ++ playlistPrefix ++ toFilePath lev ++ "</element>\n"
         )
         levels
    ++ "</playlist>"

_playlistFromIDs
  :: DuckIDs -> ExceptT DuckError (WriterT DuckLog IO) DuckPlaylist
_playlistFromIDs contents = do
  tell ["Parsing workshop IDs."]
  wids <- liftEither $ fmapL ParseError $ WID.parseWorkshopIDs contents
  let cmdArgs = args wids
  tell ["Running steamcmd with args: '" ++ unwords cmdArgs ++ "'."]
  (exit, out, _) <- liftIO $ readProcessWithExitCode steamcmd cmdArgs ""
  case exit of
    ExitFailure e -> throwError
      (SteamCMDError $ "steamcmd exited with error code " ++ show e ++ ".")
    ExitSuccess -> tell ["steamcmd exited with success."]
  tell ["Parsing steamcmd output."]
  paths <- liftEither $ fmapL ParseError $ CMD.parseSteamCMD out
  tell ["Getting level-file paths."]
  levPaths <- mapM levelPath paths
  tell ["Building playlist string."]
  return $ playlist levPaths

playlistFromIDs :: DuckIDs -> IO (Either DuckError DuckPlaylist, DuckLog)
playlistFromIDs = runWriterT . runExceptT . _playlistFromIDs

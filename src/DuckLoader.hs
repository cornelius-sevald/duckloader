{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module DuckLoader
  ( playlistFromIDs
  )
where

import           System.Exit
import           System.Process.Typed
import           System.Directory               ( listDirectory )
import           Control.Error.Util
import           Control.Monad.Except
import           Control.Monad.Writer
import           Data.List                      ( isSuffixOf )
import           Data.EitherR                   ( fmapL )
import qualified Data.ByteString.Lazy          as L
import           Data.ByteString.Builder

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
type DuckIDs = L.ByteString
type DuckPlaylist = L.ByteString

args :: [WID.WorkshopID] -> [String]
args wids =
  ["+login", "anonymous"]
    ++ foldMap (\wid -> ["+workshop_download_item", "312530"] ++ [show wid])
               wids
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
  toLazyByteString
    $  stringUtf8 "<playlist>\n"
    <> foldMap
         (\lev ->
           stringUtf8 "  <element>"
             <> stringUtf8 playlistPrefix
             <> stringUtf8 (toFilePath lev)
             <> stringUtf8 "</element>\n"
         )
         levels
    <> stringUtf8 "</playlist>"

_playlistFromIDs
  :: DuckIDs -> ExceptT DuckError (WriterT DuckLog IO) DuckPlaylist
_playlistFromIDs contents = do
  tell ["Parsing workshop IDs."]
  wids <- liftEither $ fmapL ParseError $ WID.parseWorkshopIDs contents
  let cmdArgs = args wids
  tell ["Running steamcmd with args: '" ++ unwords cmdArgs ++ "'."]
  (exit, out, _) <- liftIO $ readProcess $ proc steamcmd cmdArgs
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

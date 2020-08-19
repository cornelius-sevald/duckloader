module Main where

import           System.IO
import           System.Exit
import           Data.Either                    ( isLeft )
import           Control.Monad                  ( when )
import qualified Data.ByteString.Lazy.Char8    as C

import           Options.Applicative

import           DuckLoader

-- Command line options

data Input
  = FileInput FilePath
  | StdInput

data Output
  = FileOutput FilePath
  | StdOutput

data Options = Options
  { inRead   :: Input
  , outWrite :: Output
  , verbose  :: Bool
  }

input :: Parser Input
input = (FileInput <$> argument str (metavar "IN")) <|> pure StdInput

output :: Parser Output
output =
  FileOutput
    <$> strOption
          (long "output" <> short 'o' <> metavar "OUT" <> help
            "Write output to OUT. By default write to stdout"
          )
    <|> pure StdOutput

verbosity :: Parser Bool
verbosity =
  switch (long "verbose" <> short 'v' <> help "Show progress to stderr")

options :: Parser Options
options = Options <$> input <*> output <*> verbosity

-- Main program

main :: IO ()
main = run =<< execParser opts
 where
  opts = info
    (options <**> helper)
    (  fullDesc
    <> progDesc
         (  "Turn a file of Steam Workshop IDs into a Duck Game playlist."
         ++ " By default reads from stdin."
         )
    <> header "duckloader - generate Duck Game playlists"
    )

run :: Options -> IO ()
run (Options inR outW verb) = do
  inHdl <- case inR of
    StdInput       -> return stdin
    FileInput path -> openFile path ReadMode
  outHdl <- case outW of
    StdOutput       -> return stdout
    FileOutput path -> openFile path WriteMode
  contents       <- C.hGetContents inHdl
  (result, logs) <- playlistFromIDs contents
  when verb $ putLogs logs stderr
  case result of
    Left errorMsg -> do
      hPutStrLn stderr "ERROR!"
      hPutStrLn stderr (show errorMsg)
    Right playlistStr -> do
      C.hPutStrLn outHdl playlistStr
  hClose inHdl
  hClose outHdl
  if isLeft result then exitWith $ ExitFailure 1 else exitWith ExitSuccess

putLogs :: [String] -> Handle -> IO ()
putLogs logs hdl = hPutStr hdl (unlines $ map ("> " ++) logs)

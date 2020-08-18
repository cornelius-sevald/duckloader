module Parsing.SteamCMD
  ( parseSteamCMD
  )
where

import           Control.Monad                  ( void )
import           Data.Maybe                     ( fromMaybe )
import qualified Data.ByteString.Lazy          as L

import           Path
import           Text.Parsec

safeParseAbsDir :: FilePath -> Maybe (Path Abs Dir)
safeParseAbsDir = parseAbsDir

steamCMDoutput = do
  initial
  login
  download `endBy1` space

initial = void $ manyTill anyChar (try (string "OK." >> eol >> eol))

login = do
  string "Connecting anonymously to Steam Public...Logged in OK"
  eol
  string "Waiting for user info...OK"
  eol
  return ()

download = do
  string "Downloading item "
  many1 digit
  string " ..."
  eol
  string "Success. Downloaded item "
  many1 digit
  string " to "
  dlPath <- between (char '"') (char '"') path
  space
  between (char '(') (char ')') (many1 digit >> string " bytes")
  return dlPath

path = do
  s <- parseAbsDir <$> many1 (noneOf ['"'])
  fromMaybe (fail "Invalid path") (return <$> s)

eol = endOfLine

parseSteamCMD :: L.ByteString -> Either ParseError [Path Abs Dir]
parseSteamCMD = parse steamCMDoutput "(steamcmd)"

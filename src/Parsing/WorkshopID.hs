module Parsing.WorkshopID
  ( WorkshopID
  , parseWorkshopIDs
  )
where

import           Control.Monad                  ( void )
import           Numeric.Natural
import qualified Data.ByteString.Lazy          as L

import           Text.Parsec

type WorkshopID = Natural

workshopIDsFile = concat <$> many1 line

line = (comment >> return []) <|> widLine

widLine = do
  wid <- (urlPrefix >> workshopID) <|> workshopID <?> "Workshop URL or ID"
  many (char ' ')
  comment <|> (void eol)
  return [wid]

workshopID = read <$> many1 digit

urlPrefix = string "https://steamcommunity.com/sharedfiles/filedetails/?id="

comment = void $ do
  char '#'
  manyTill anyChar (try eol)

eol = endOfLine

parseWorkshopIDs :: L.ByteString -> Either ParseError [WorkshopID]
parseWorkshopIDs = parse workshopIDsFile "(workshopIDs)"

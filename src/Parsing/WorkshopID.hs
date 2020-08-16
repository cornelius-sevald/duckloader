module Parsing.WorkshopID
  ( parseWorkshopIDs
  )
where

import Control.Monad (void)

import           Text.Parsec

workshopIDsFile = concat <$> many1 line

line = (comment >> return []) <|> widLine

widLine = do
  wid <- (urlPrefix >> workshopID) <|> workshopID <?> "Workshop URL or ID"
  many (char ' ')
  comment <|> (void eol)
  return [wid]

workshopID = many1 digit

urlPrefix = string "https://steamcommunity.com/sharedfiles/filedetails/?id="

comment = void $ do
  char '#'
  manyTill anyChar (try eol)

eol = endOfLine

parseWorkshopIDs :: String -> Either ParseError [String]
parseWorkshopIDs = parse workshopIDsFile "(workshopIDs)"

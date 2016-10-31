module Lib
    ( parseInput
    , DateNumOrDate
    , dateNumOrDateToString
    , dateToDateNum
    , dateNumToDate
    ) where

import Text.ParserCombinators.Parsec hiding ((<|>))
import Control.Applicative
import Data.Time

type DateNum = Integer
type Date = Day
data DateNumOrDate = DateNum DateNum | Date Date

cwEpoch :: Date
cwEpoch = fromGregorian 1999 12 31

dateNumToDate :: DateNum -> Date
dateNumToDate dateNum = addDays dateNum cwEpoch

dateToDateNum :: Date -> DateNum
dateToDateNum date = diffDays date cwEpoch

dateNumOrDateToString :: DateNumOrDate -> String
dateNumOrDateToString (DateNum datenum) = showGregorian $ dateNumToDate datenum
dateNumOrDateToString (Date date)       = show          $ dateToDateNum date

parseInput :: String -> Maybe DateNumOrDate
parseInput inp = case parse datenumOrDate "" inp of
                    Left error -> Nothing
                    Right parsed -> Just parsed

{- Parsers
 -
 - Grammar for valid inputs (along with parser types):
 -
 - datenumOrDate = datenum | date                        :: DateNumOrDate
 - datenum       = digit+ eof                            :: Integer
 - date          = mdy | ymd                             :: Date
 - mdy           = month sep day   sep year eof          :: Date
 - ymd           = year  sep month sep day  eof          :: Date
 - month         = shortint                              :: Int
 - day           = shortint                              :: Int
 - year          = digit digit digit digit | digit digit :: Integer
 - shortint      = digit digit | digit                   :: Int
 - sep           = [ /-]+                                :: ()
 -}
datenumOrDate = try (Date <$> date) <|> DateNum <$> datenum                    :: Parser DateNumOrDate
datenum       = read <$> many1 digit <* eof                                    :: Parser Integer
date          = try mdy <|> ymd                                                :: Parser Date
mdy           = (\m d y -> fromGregorian y m d) <$> month <* sep <*> day   <* sep <*> year <* eof :: Parser Date
ymd           = fromGregorian                   <$> year  <* sep <*> month <* sep <*> day  <* eof :: Parser Date
month         = shortint                                                       :: Parser Int
day           = shortint                                                       :: Parser Int
year          = read <$> (try (count 4 digit) <|> ("20" ++) <$> count 2 digit) :: Parser Integer
shortint      = read <$> (try (count 2 digit) <|> count 1 digit)               :: Parser Int
sep           = skipMany1 $ oneOf " /-"                                        :: Parser ()


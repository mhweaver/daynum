module Lib
    ( parseInput
    , DateNumOrDate
    , dateNumOrDateToString
    , dateToDateNum
    , dateNumToDate
    ) where

import Data.Time
import Text.ParserCombinators.Parsec

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
dateNumOrDateToString (Date date)      = show $ dateToDateNum date

parseInput :: String -> Maybe DateNumOrDate
parseInput inp = case parse datenumOrDate "" inp of
                    Left error -> Nothing
                    Right parsed -> Just parsed

{- Parsers
 -
 - Grammar for valid inputs (along with parser types):
 -
 - datenumOrDate :: DateNumOrDate = datenum | date
 - datenum       :: Integer       = digit+ eof
 - date          :: Date          = mdy | ymd
 - mdy           :: Date          = monthday sep year     eof
 - ymd           :: Date          = year     sep monthday eof
 - year          :: Integer       = digit digit digit digit | digit digit
 - monthday      :: (Int, Int)    = shortint sep shortint
 - shortint      :: Int           = digit digit | digit
 - sep           :: ()            = [ /-]+
 -}
datenumOrDate :: Parser DateNumOrDate
datenumOrDate = choice [ try $ do{ date <- date;  return $ Date date  }
                      ,       do{ num <- datenum; return $ DateNum num } ]
datenum :: Parser Integer
datenum = do
    raw <- many digit
    eof
    return $ read raw
date :: Parser Date
date = try mdy
       <|> ymd
ymd :: Parser Date
ymd = do
    y <- year
    sep
    (m, d) <- monthday
    eof
    return $ fromGregorian y m d
mdy :: Parser Date
mdy = do
    (m, d) <- monthday
    sep
    y <- year
    eof
    return $ fromGregorian y m d
year :: Parser Integer
year = do
    let year2 = do
                y <- count 2 digit
                return $ "20" ++ y
        year4 = count 4 digit
    y <- try year4
         <|> year2
    return $ read y
monthday :: Parser (Int, Int) -- (month, day)
monthday = do
    m <- shortint
    sep
    d <- shortint
    return (m, d)
shortint :: Parser Int
shortint = do
    i <- try $ count 2 digit
         <|>   count 1 digit
    return $ read i
sep :: Parser ()
sep = skipMany1 $ oneOf " /-"


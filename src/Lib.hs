module Lib
    ( dayNumOrDayToString
    , dayToDayNum
    , dayNumToDay
    , parseDateStr
    , Day -- Re-export from Data.Time
    , DayNum
    ) where

import Control.Monad ( msum )
import Data.Time
import Text.ParserCombinators.Parsec

type DayNum = Integer

cwEpoch :: Day
cwEpoch = fromGregorian 1999 12 31

dayNumToDay :: DayNum -> Day
dayNumToDay dayNum = addDays dayNum cwEpoch

dayToDayNum :: Day -> DayNum
dayToDayNum day = diffDays day cwEpoch

dayNumOrDayToString :: Either DayNum Day -> String
dayNumOrDayToString (Left daynum) = showGregorian $ dayNumToDay daynum
dayNumOrDayToString (Right day)   = show $ dayToDayNum day

parseDateStr :: String -> Maybe Day
parseDateStr s = case parse date "" s of
                    Left error -> Nothing
                    Right day  -> Just day

{- date     :: Day        -> mdy | ymd
 - mdy      :: Day        -> monthday sep year     eof
 - ymd      :: Day        -> year     sep monthday eof
 - year     :: Integer    -> digit digit digit digit | digit digit
 - monthday :: (Int, Int) -> shortint sep shortint
 - shortint :: Int        -> digit digit | digit
 - sep      :: ()         -> [ /-]+
 -}
date :: Parser Day
date = try mdy
       <|> ymd
ymd :: Parser Day
ymd = do
    y <- year
    sep
    (m, d) <- monthday
    eof
    return $ fromGregorian y m d
mdy :: Parser Day
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


module Lib
    ( dayNumOrDayToString
    , dayToDayNum
    , dayNumToDay
    , parseDateStr
    ) where

import Control.Monad ( msum )
import Data.Time

cwEpoch :: Day
cwEpoch = fromGregorian 1999 12 31

dayNumToDay :: Integer -> Day
dayNumToDay dayNum = addDays dayNum cwEpoch

dayToDayNum :: Day -> Integer
dayToDayNum day = diffDays day cwEpoch

dayNumOrDayToString :: Either Integer Day -> String
dayNumOrDayToString (Left daynum) = showGregorian $ dayNumToDay daynum
dayNumOrDayToString (Right day)   = show $ dayToDayNum day

parseDateStr :: String -> Maybe Day
parseDateStr s = msum [ parseWith "%m-%d-%Y" -- mplus the parsed Maybes together, to keep trying until we find a Just Day
                      , parseWith "%m/%d/%Y"
                      , parseWith "%m %d %Y"
                      , parseWith "%Y-%m-%d"
                      , parseWith "%Y/%m/%d"
                      , parseWith "%Y %m %d" ]
                 where parseWith fmt = parseTimeM True defaultTimeLocale fmt s :: Maybe Day

module Lib
    ( cwEpoch
    , dayNumToDay
    , dayToDayNum
    , dayNumOrDayToString
    ) where

import Data.Time

cwEpoch :: Day
cwEpoch = fromGregorian 1999 12 31

dayNumToDay :: Integer -> Day
dayNumToDay dayNum = addDays dayNum cwEpoch

dayToDayNum :: Day -> Integer
dayToDayNum day = diffDays day cwEpoch

dayNumOrDayToString :: Either Integer Day -> String
dayNumOrDayToString (Left daynum) = showGregorian $ dayNumToDay daynum
dayNumOrDayToString (Right day)   = show $ diffDays day cwEpoch 


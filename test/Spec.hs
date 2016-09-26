import Lib
import Data.Time
import Test.QuickCheck

main :: IO ()
main = do
    quickCheck propDual
    quickCheck propDual'

propDual i = dayToDayNum (dayNumToDay i) == i
propDual' i = dayNumToDay (dayToDayNum (ModifiedJulianDay i)) == ModifiedJulianDay i

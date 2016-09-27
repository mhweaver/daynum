import Lib
import Data.Time
import Test.QuickCheck
import Test.HUnit

main :: IO ()
main = do
    -- Verify dayNumToDay 1 == 2000-01-01. The dual test (dayToDayNum 2000-01-01 == 1) is ensured by the quickcheck specs
    runTestTT $ TestLabel "Base case dayNumToDay 1 == 2000-01-01" baseCaseTest
    quickCheck propDual
    quickCheck propDual'

propDual i = dayToDayNum (dayNumToDay i) == i
propDual' i = dayNumToDay (dayToDayNum (ModifiedJulianDay i)) == ModifiedJulianDay i

baseCaseTest = TestCase (assertEqual "" 1 (dayToDayNum (fromGregorian 2000 1 1)))
baseCaseTest' = TestCase (assertEqual "" (fromGregorian 2000 1 1) (dayNumToDay 1))

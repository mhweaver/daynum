import Lib
import Data.Time
import Test.QuickCheck
import Test.HUnit

main :: IO ()
main = do
    -- Verify dateNumToDate 1 == 2000-01-01. The dual test (dateToDateNum 2000-01-01 == 1) is ensured by the quickcheck specs
    runTestTT $ TestLabel "Base case dateNumToDate 1 == 2000-01-01" baseCaseTest
    quickCheck propDual
    quickCheck propDual'

propDual i = dateToDateNum (dateNumToDate i) == i
propDual' i = dateNumToDate (dateToDateNum (ModifiedJulianDay i)) == ModifiedJulianDay i

baseCaseTest = TestCase (assertEqual "" 1 (dateToDateNum (fromGregorian 2000 1 1)))
baseCaseTest' = TestCase (assertEqual "" (fromGregorian 2000 1 1) (dateNumToDate 1))

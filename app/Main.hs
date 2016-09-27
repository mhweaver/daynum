module Main where

import Lib                ( dayNumToDay
                          , dayToDayNum
                          , dayNumOrDayToString )
import Control.Monad      ( mplus
                          , msum )
import System.Environment ( getArgs )
import Text.Read          ( readMaybe )
import Data.Maybe         ( fromMaybe )
import Data.Time          ( Day 
                          , parseTimeM
                          , defaultTimeLocale )

main :: IO ()
main = do
    args <- getArgs
    let dayNumOrDay = parseArgs args
        out = dayNumOrDayToString dayNumOrDay
    putStrLn out

usage = "usage: daynum [day num | date string]"

parseArgs :: [String] -> Either Integer Day
parseArgs (arg:[]) = fromMaybe (error usage) $ parseArg arg
parseArgs _        = error usage

parseArg :: String -> Maybe (Either Integer Day)
parseArg arg = fmap Left (readMaybe arg ) -- :: Maybe Integer)
               `mplus` fmap Right (parseDateStr arg)
    
parseDateStr :: String -> Maybe Day
parseDateStr s = msum [ parseWith "%m-%d-%Y" -- mplus the parsed Maybes together, to keep trying until we find a Just Day
                      , parseWith "%m/%d/%Y"
                      , parseWith "%m %d %Y"
                      , parseWith "%Y-%m-%d"
                      , parseWith "%Y/%m/%d"
                      , parseWith "%Y %m %d" ]
                 where parseWith fmt = parseTimeM True defaultTimeLocale fmt s :: Maybe Day


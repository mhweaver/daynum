module Main where

import Lib
import Control.Monad
import System.Environment (getArgs)
import Text.Read          (readMaybe)
import Data.Maybe
import Data.Time

main :: IO ()
main = do
    args <- getArgs
    let dayNumOrDay = parseArgs args
        out = dayNumOrDayToString dayNumOrDay
    putStrLn out

parseArgs :: [String] -> Either Integer Day
parseArgs (arg:[]) = parseArg arg
parseArgs _        = error "usage: daynum [day num | date string]"

parseArg :: String -> Either Integer Day
parseArg arg = do
    let daynum = readMaybe arg :: Maybe Integer
    case daynum of
      Just num -> Left num
      Nothing  -> Right $ fromMaybe (error "Invalid date string") $ parseDateStr arg

    
parseDateStr :: String -> Maybe Day
parseDateStr s = parseWith "%Y-%m-%d" s
                 `mplus` parseWith "%Y/%m/%d" s
                 `mplus` parseWith "%Y %m %d" s
                 `mplus` parseWith "%m-%d-%Y" s
                 `mplus` parseWith "%m/%d/%Y" s
                 `mplus` parseWith "%m %d %Y" s
                 where parseWith fmt = parseTimeM True defaultTimeLocale fmt


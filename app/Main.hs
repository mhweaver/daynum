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
import System.IO          ( hPutStrLn
                          , stderr )

main :: IO ()
main = do
    args <- getArgs
    let wrappedDayNumOrDay = parseArgs args
    case wrappedDayNumOrDay of
        Left e -> hPutStrLn stderr e
        Right dayNumOrDay -> putStrLn . dayNumOrDayToString $ dayNumOrDay

usage = "usage: daynum [day num | date string]"

parseArgs :: [String] -> Either String (Either Integer Day)
parseArgs [arg] = do
    let parsed = parseArg arg
    case parsed of
        Nothing -> Left $ "Unable to parse argument: " ++ arg ++ "\n" ++ usage
        Just p  -> Right p
parseArgs _     = Left usage

parseArg :: String -> Maybe (Either Integer Day)
parseArg arg = fmap Left (readMaybe arg) -- :: Maybe Integer
               `mplus` fmap Right (parseDateStr arg)
    
parseDateStr :: String -> Maybe Day
parseDateStr s = msum [ parseWith "%m-%d-%Y" -- mplus the parsed Maybes together, to keep trying until we find a Just Day
                      , parseWith "%m/%d/%Y"
                      , parseWith "%m %d %Y"
                      , parseWith "%Y-%m-%d"
                      , parseWith "%Y/%m/%d"
                      , parseWith "%Y %m %d" ]
                 where parseWith fmt = parseTimeM True defaultTimeLocale fmt s :: Maybe Day


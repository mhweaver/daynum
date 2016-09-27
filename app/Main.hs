module Main where

import Lib                ( dayNumOrDayToString
                          , parseDateStr 
                          , Day
                          , DayNum )
import Control.Monad      ( mplus )
import System.Environment ( getArgs )
import System.IO          ( hPutStrLn
                          , stderr )
import Text.Read          ( readMaybe )

main :: IO ()
main = do
    args <- getArgs
    case parseArgs args of
        Left err          -> hPutStrLn stderr err
        Right dayNumOrDay -> putStrLn . dayNumOrDayToString $ dayNumOrDay

usage = "usage: daynum [ dddd | yyyy-mm-dd ]"
parseArgs :: [String] -> Either String (Either DayNum Day)
parseArgs [arg] = case parseArg arg of
                  Nothing -> Left $ "Unable to parse date: " ++ arg ++ "\n" ++ usage
                  Just p  -> Right p
parseArgs _     = Left usage

parseArg :: String -> Maybe (Either DayNum Day)
parseArg arg = fmap Left (readMaybe arg :: Maybe DayNum)
               `mplus` fmap Right (parseDateStr arg)

module Main where

import Lib                ( dayNumOrDayToString
                          , parseDateStr )
import Control.Monad      ( mplus )
import System.Environment ( getArgs )
import Text.Read          ( readMaybe )
import Data.Time          ( Day )
import System.IO          ( hPutStrLn
                          , stderr )

main :: IO ()
main = do
    args <- getArgs
    let wrappedDayNumOrDay = parseArgs args
    case wrappedDayNumOrDay of
        Left e -> hPutStrLn stderr e
        Right dayNumOrDay -> putStrLn . dayNumOrDayToString $ dayNumOrDay

usage = "usage: daynum [ dddd | yyyy-mm-dd ]"

parseArgs :: [String] -> Either String (Either Integer Day)
parseArgs [arg] = case parseArg arg of
                  Nothing -> Left $ "Unable to parse date: " ++ arg ++ "\n" ++ usage
                  Just p  -> Right p
parseArgs _     = Left usage

parseArg :: String -> Maybe (Either Integer Day)
parseArg arg = fmap Left (readMaybe arg :: Maybe Integer)
               `mplus` fmap Right (parseDateStr arg)

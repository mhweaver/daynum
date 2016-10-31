module Main where

import Lib                ( parseInput
                          , DateNumOrDate
                          , dateNumOrDateToString
                          )
import System.Environment ( getArgs )
import System.IO          ( hPutStrLn
                          , stderr )

main :: IO ()
main = do
    args <- getArgs
    let inp = parseInput $ unwords args:: Maybe DateNumOrDate
    case inp of
        Nothing -> hPutStrLn stderr "usage: daynum [ dddd | yyyy-mm-dd ]"
        Just dateNumOrDate -> putStrLn . dateNumOrDateToString $ dateNumOrDate


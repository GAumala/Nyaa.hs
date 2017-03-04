module Main where

import System.Environment
import Lib

concatWithPlus :: String -> String -> String
concatWithPlus "" rightHand = rightHand
concatWithPlus leftHand rightHand = leftHand ++ "+" ++ rightHand

main :: IO ()
main = do
    args <- getArgs
    searchWithNyaa $ foldl concatWithPlus "" args

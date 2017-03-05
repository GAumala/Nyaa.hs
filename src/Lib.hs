{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( searchWithNyaa
    ) where

import Data.Either
import Html
import Http


searchWithNyaa :: String -> IO ()
searchWithNyaa query = do
    let nyaaURL = getNyaaSearchUrl query
    resp <- openURL nyaaURL
    putStrLn $ "Response Status Code : " ++ show (getResponseStatusCode resp)
    let searchResults = getSearchResults $ getResponseBody resp
    let errors = lefts searchResults
    if null errors
        then displayResultsList $ rights searchResults
        else displayErrorsList nyaaURL errors 

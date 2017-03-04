{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc
    ) where


import qualified Data.ByteString.Lazy.Char8 as L8
import           Network.HTTP.Client        (newManager, Response)
import           Network.HTTP.Client.TLS
import           Network.HTTP.Simple

import Data.Either
import Html

openURL :: String -> IO (Response L8.ByteString)
openURL x = do
     manager <- newManager tlsManagerSettings
     req <- parseRequest x
     httpLbs (setRequestManager manager req)

getNyaaSearchUrl :: String -> String
getNyaaSearchUrl query = "https://www.nyaa.se/?page=search&term="
                     ++ query

openNyaaURL :: String -> IO (Response L8.ByteString)
openNyaaURL x = openURL $ getNyaaSearchUrl x

someFunc :: IO ()
someFunc = do
    resp <- openNyaaURL "Kuzu+no+Honkai+deadfish"
    putStrLn $ "Response Status Code 2: " ++ show (getResponseStatusCode resp)
    let body = getResponseBody resp
    let searchResults = getSearchResults body
    let errors = lefts searchResults
    if null errors then print $ rights searchResults
        else print errors

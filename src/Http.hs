module Http
    ( getNyaaSearchUrl,
      openURL,
      getResponseBody,
      getResponseStatusCode,
      Response
    ) where

import           Network.HTTP.Client        (newManager, Response)
import           Network.HTTP.Client.TLS
import           Network.HTTP.Simple
import qualified Data.ByteString.Lazy.Char8 as L8

openURL :: String -> IO (Response L8.ByteString)
openURL x = do
     putStrLn $ "Retreiving URL: " ++ x
     manager <- newManager tlsManagerSettings
     req <- parseRequest x
     httpLbs (setRequestManager manager req)

getNyaaSearchUrl :: String -> String
getNyaaSearchUrl query = "https://www.nyaa.se/?page=search&term="
                     ++ query

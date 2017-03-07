{-# LANGUAGE OverloadedStrings #-}

module Html
    ( displayErrorsList,
      displayResultsList,
      getSearchResults
    ) where

import Data.Maybe

import qualified Data.ByteString.Lazy.Char8 as L8
import Text.HTML.TagSoup

data NyaaParseErrorCause = TITLE_NOT_FOUND | SIZE_NOT_FOUND | URL_NOT_FOUND
    | SEEDERS_NOT_FOUND | LEECHERS_NOT_FOUND
    deriving (Eq, Show)

data NyaaParseError = NyaaParseError {
    errorCause :: NyaaParseErrorCause,
    errorHtml :: L8.ByteString
} deriving (Eq, Show)

nyaaErrorFromPartition :: NyaaParseErrorCause -> [Tag L8.ByteString] -> NyaaParseError
nyaaErrorFromPartition cause tags = NyaaParseError cause (renderTags tags)

data NyaaResult = NyaaResult {
    title :: L8.ByteString,
    url :: L8.ByteString,
    size :: L8.ByteString,
    seeders :: Int,
    leechers :: Int
} deriving (Eq, Show)

getTitleFromRow ::  [Tag L8.ByteString] -> Maybe L8.ByteString
getTitleFromRow rowTags
    | length editedTags > 2 = maybeTagText $ editedTags !! 2
    | otherwise = Nothing
    where titleCellTag = "<td class=\"tlistname\">" :: String
           -- td has 3 tags: TagOpen a, InnerText a, TagCloseA
           -- we need the inner text
          editedTags =  dropWhile (~/= titleCellTag) rowTags

getUrlFromRow ::  [Tag L8.ByteString] -> Maybe L8.ByteString
getUrlFromRow rowTags
    | null editedTags = Nothing
    | not (isTagOpen url_a_tag) = Nothing
    | L8.length res_url > 0 = Just $ L8.concat [L8.pack "https:", res_url]
    | otherwise = Nothing
    where urlCellTag = "<td class=\"tlistdownload\">" :: String
            -- td has one a tag. the href attribute should have our URL
          editedTags =  drop 1 $ dropWhile (~/= urlCellTag) rowTags
          url_a_tag = head editedTags
          res_url = fromAttrib "href" url_a_tag

getSizeFromRow ::  [Tag L8.ByteString] -> Maybe L8.ByteString
getSizeFromRow rowTags
    | not (null editedTags) = maybeTagText $ head editedTags
    | otherwise = Nothing
    where sizeCellTag = "<td class=\"tlistsize\">" :: String
            -- td has no children. We want its inner text
          editedTags =  drop 1 $ dropWhile (~/= sizeCellTag) rowTags


getSeedersFromRow ::  [Tag L8.ByteString] -> Maybe Int
getSeedersFromRow rowTags
    | null editedTags = Nothing
    | isNothing maybeSeeders = Nothing
    | otherwise = Just $ fst $ fromJust seedersInt
    where seedCellTag = "<td class=\"tlistsn\">" :: String
            -- td has no children. We want its inner text
          editedTags =  drop 1 $ dropWhile (~/= seedCellTag) rowTags
          maybeSeeders = maybeTagText $ head editedTags
          seedersInt = L8.readInt $ fromJust maybeSeeders

getLeechersFromRow ::  [Tag L8.ByteString] -> Maybe Int
getLeechersFromRow rowTags
    | null editedTags = Nothing
    | isNothing maybeLeechers = Nothing
    | otherwise = Just $ fst $ fromJust leechersInt
    where leechCellTag = "<td class=\"tlistln\">" :: String
            -- td has no children. We want its inner text
          editedTags =  drop 1 $ dropWhile (~/= leechCellTag) rowTags
          maybeLeechers = maybeTagText $ head editedTags
          leechersInt = L8.readInt $ fromJust maybeLeechers

parseResultRow ::  [Tag L8.ByteString] -> Either NyaaParseError NyaaResult
parseResultRow rowTags
    | isNothing _title = Left $ nyaaErrorFromPartition TITLE_NOT_FOUND rowTags
    | isNothing _url = Left $ nyaaErrorFromPartition URL_NOT_FOUND rowTags
    | isNothing _size = Left $ nyaaErrorFromPartition SIZE_NOT_FOUND rowTags
    | isNothing _seeders = Left $ nyaaErrorFromPartition SEEDERS_NOT_FOUND rowTags
    | isNothing _leechers = Left $ nyaaErrorFromPartition LEECHERS_NOT_FOUND rowTags
    | otherwise = Right $ NyaaResult (fromJust _title) (fromJust _url)
        (fromJust _size) (fromJust _seeders) (fromJust _leechers)
    where _title = getTitleFromRow rowTags
          _url = getUrlFromRow rowTags
          _size = getSizeFromRow rowTags
          _seeders = getSeedersFromRow rowTags
          _leechers = getLeechersFromRow rowTags

getSearchResults :: L8.ByteString -> [Either NyaaParseError NyaaResult]
getSearchResults responseBody = map parseResultRow $ mconcat [
    partitions (~== tlistrowNormal) tags,
    partitions (~== tlistrowRemake) tags,
    partitions (~== tlistrowTrusted) tags,
    partitions (~== tlistrowAplus) tags ]
    where tags = parseTags responseBody
          tlistrowNormal = "<tr class=\"tlistrow\">" :: String
          tlistrowRemake = "<tr class=\"tlistrow remake\">" :: String
          tlistrowTrusted = "<tr class=\"tlistrow trusted\">" :: String
          tlistrowAplus = "<tr class=\"tlistrow aplus\">" :: String

displaySearchResult :: NyaaResult -> IO ()
displaySearchResult result = L8.putStrLn $ L8.concat [L8.pack "* ", _title,
    L8.pack "\n", _size, L8.pack " ", L8.pack $ show _seeders, L8.pack " seeders, ",
    L8.pack $ show _leechers, L8.pack " leechers. ", _url, L8.pack "\n"]
    where _title = title result
          _url = url result
          _size = size result
          _seeders = seeders result
          _leechers = leechers result

displaySearchError :: NyaaParseError -> IO()
displaySearchError (NyaaParseError TITLE_NOT_FOUND tags) = L8.putStrLn $
    L8.concat ["Title for torrent file not found.\nHTML:\n", tags, "\n"]
displaySearchError (NyaaParseError SIZE_NOT_FOUND tags) = L8.putStrLn $
    L8.concat ["Size for torrent file not found.\nHTML:\n", tags, "\n"]
displaySearchError (NyaaParseError URL_NOT_FOUND tags) = L8.putStrLn $
    L8.concat ["URL for torrent file not found.\nHTML:\n", tags, "\n"]
displaySearchError (NyaaParseError SEEDERS_NOT_FOUND tags) = L8.putStrLn $
    L8.concat ["Number of seeders for torrent file not found.\nHTML:\n", tags, "\n"]
displaySearchError (NyaaParseError LEECHERS_NOT_FOUND tags) = L8.putStrLn $
    L8.concat ["Number of leechers for torrent file not found.\nHTML:\n", tags, "\n"]


displayResultsList :: [NyaaResult] -> IO()
displayResultsList [] = putStrLn "No torrents found."
displayResultsList results = mapM_ displaySearchResult results

displayErrorsList :: String -> [NyaaParseError] -> IO()
displayErrorsList errorURL errors = do
    putStrLn "Error: Some results could not be parsed."
    putStrLn "This is most likely due to changes in Nyaa's HTML code."
    putStrLn $ "Please submit an issue at " ++ issueURL ++
        " with the following information:\n"
    mapM_ displaySearchError errors
    putStrLn $ "The complete HTML can be downloaded from " ++ errorURL
    where issueURL = "https://github.com/GAumala/nyaa.hs/issues/new"

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module PublicationParserXmlConduitNodeStream (parsePublications) where

import Conduit (ConduitT, runConduit, sinkList, (.|))
import Control.Monad (void)
import Control.Monad.Trans.Resource (MonadThrow)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import Data.XML.Types (Event (..))
import PublicationData (Author (..), Publication (..))
import Text.XML.Cursor (Cursor, attribute, content, element, ($/), ($|), (&/))
import Text.XML.Stream.Node (buildElementCursor)
import Text.XML.Stream.Parse (def, manyYield, parseLBS, tagIgnoreAttrs)

parseAuthorRecord :: Cursor -> Author
parseAuthorRecord cursor = author
  where
    names = cursor $/ element "name" &/ content
    uuids = cursor $| attribute "uuid"
    author =
      Author
        { uuid = read $ T.unpack . head $ uuids,
          name = T.unpack . head $ names
        }

parsePublicationRecord :: Cursor -> Publication
parsePublicationRecord cursor = publication
  where
    titles = cursor $/ element "title" &/ content
    urls = cursor $/ element "url" &/ content
    uuids = cursor $| attribute "uuid"
    authors = cursor $/ element "author"
    publication =
      Publication
        { uuid = read $ T.unpack . head $ uuids,
          title = T.unpack . head $ titles,
          authors = map parseAuthorRecord authors,
          url = Just . T.unpack . head $ urls
        }

parsePublication :: MonadThrow m => ConduitT Event o m (Maybe Publication)
parsePublication = fmap parsePublicationRecord <$> buildElementCursor

parsePublications :: FilePath -> IO [Publication]
parsePublications file = do
  xml <- LBS.readFile file
  runConduit $
    parseLBS def xml
      .| void (tagIgnoreAttrs "bib" (manyYield parsePublication))
      .| sinkList

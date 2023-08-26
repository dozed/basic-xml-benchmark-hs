{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}

module PublicationParser (PublicationParser (..), parsePublications) where

import PublicationData (Author (..), Publication (..))
import Data.Maybe (mapMaybe)

-- e: Element Type
-- n: Element Name / Attribute Name
data PublicationParser e n = PublicationParser {
  getAttrValue :: n -> e -> Maybe String,
  atTags :: n -> e -> [e],
  atTag :: n -> e -> Maybe e,
  getText :: e -> String,
  getTextAtTag :: n -> e -> Maybe String,
  uuidName :: n,
  titleName :: n,
  nameName :: n,
  urlName :: n,
  authorName :: n,
  publicationName :: n,
  parseBibElement :: FilePath -> IO e
}

parsePublication :: PublicationParser e n -> e -> Maybe Publication
parsePublication p e =
  let Just uuid = p.getAttrValue p.uuidName e
      Just title = p.getTextAtTag p.titleName e
      url = p.getTextAtTag p.urlName e
      authors = mapMaybe (parseAuthor p) $ p.atTags p.authorName e
   in Just
        Publication
          { uuid = read uuid,
            title = title,
            authors = authors,
            url = url
          }

parseAuthor :: PublicationParser e n -> e -> Maybe Author
parseAuthor p e =
  let Just uuid = p.getAttrValue p.uuidName e
      Just name = p.getTextAtTag p.nameName e
   in Just
        Author
          { uuid = read uuid,
            name = name
          }

parsePublications :: PublicationParser e n -> FilePath -> IO [Publication]
parsePublications p file = do
  bibElement <- p.parseBibElement file
  let publications = mapMaybe (parsePublication p) $ p.atTags p.publicationName bibElement
  return publications

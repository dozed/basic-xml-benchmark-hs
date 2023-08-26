{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module PublicationParserHexml (parser) where

import qualified Data.ByteString as BS
import Data.List (find)
import Data.Maybe (listToMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import PublicationParser (PublicationParser (..))
import Text.XML.Hexml

bsToString :: BS.ByteString -> String
bsToString = T.unpack . T.decodeUtf8

getAttrValue :: BS.ByteString -> Node -> Maybe String
getAttrValue attrName n = bsToString . attributeValue <$> attributeBy n attrName

atTag :: BS.ByteString -> Node -> Maybe Node
atTag tag n = listToMaybe $ atTags tag n

atTags :: BS.ByteString -> Node -> [Node]
atTags tag n = childrenBy n tag

getText :: Node -> String
getText = bsToString . inner

getTextAtTag :: BS.ByteString -> Node -> Maybe String
getTextAtTag tag n = getText <$> atTag tag n

parseElement :: FilePath -> IO Node
parseElement file = do
  txt <- BS.readFile file
  let Right node = parse txt
      Just bibElement = find (\c -> name c == "bib") $ children node
  return bibElement

parser :: PublicationParser Node BS.ByteString
parser =
  PublicationParser
    { getAttrValue = getAttrValue,
      atTags = atTags,
      atTag = atTag,
      getText = getText,
      getTextAtTag = getTextAtTag,
      uuidName = "uuid",
      titleName = "title",
      nameName = "name",
      urlName = "url",
      authorName = "author",
      publicationName = "publication",
      parseBibElement = parseElement
    }

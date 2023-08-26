{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module PublicationParserXmlConduitDom (parser) where

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as M
import Data.Maybe (listToMaybe, mapMaybe)
import qualified Data.Text as T
import PublicationParser (PublicationParser (..))
import Text.XML hiding (parseText)
import qualified Text.XML as X

localName :: Element -> T.Text
localName = nameLocalName . elementName

getAttr :: T.Text -> Element -> Maybe T.Text
getAttr attrName n = M.lookup (Name attrName Nothing Nothing) $ elementAttributes n

getAttrValue :: T.Text -> Element -> Maybe String
getAttrValue attrName n = T.unpack <$> getAttr attrName n

getElement :: Node -> Maybe Element
getElement (NodeElement e) = Just e
getElement _ = Nothing

getContent :: Node -> Maybe T.Text
getContent (NodeContent txt) = Just txt
getContent _ = Nothing

children :: Element -> [Element]
children e = mapMaybe getElement $ elementNodes e

atTags :: T.Text -> Element -> [Element]
atTags tag n = filter (\c -> localName c == tag) $ children n

atTag :: T.Text -> Element -> Maybe Element
atTag tag n = listToMaybe $ atTags tag n

getText :: Element -> String
getText n = T.unpack . T.concat . mapMaybe getContent $ elementNodes n

getTextAtTag :: T.Text -> Element -> Maybe String
getTextAtTag tag n = getText <$> atTag tag n

parseElement :: FilePath -> IO Element
parseElement file = do
  txt <- LBS.readFile file
  let Right doc = X.parseLBS def txt
      bibElement = documentRoot doc
  return bibElement

parser :: PublicationParser Element T.Text
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

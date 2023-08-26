{-# LANGUAGE DuplicateRecordFields #-}

module PublicationParserXml (parser) where

import qualified Data.ByteString as BS
import Data.List (find)
import PublicationParser (PublicationParser (..))
import Text.XML.Light.Input (parseXML)
import Text.XML.Light.Output (showContent)
import Text.XML.Light.Proc (findAttr, findChild, findChildren, onlyElems)
import Text.XML.Light.Types (Element, QName (..), elContent, elName)

mkName :: String -> QName
mkName name = QName name Nothing Nothing

getAttrValue :: QName -> Element -> Maybe String
getAttrValue attrName e = findAttr attrName e

atTags :: QName -> Element -> [Element]
atTags tag e = findChildren tag e

atTag :: QName -> Element -> Maybe Element
atTag tag el = findChild tag el

getText :: Element -> String
getText e = concatMap showContent $ elContent e

getTextAtTag :: QName -> Element -> Maybe String
getTextAtTag tag e = getText <$> atTag tag e

parseElement :: FilePath -> IO Element
parseElement file = do
  txt <- BS.readFile file
  let contents = parseXML txt
      Just bibElement = find (\e -> elName e == mkName "bib") . onlyElems $ contents
  return bibElement

parser :: PublicationParser Element QName
parser =
  PublicationParser
    { getAttrValue = getAttrValue,
      atTags = atTags,
      atTag = atTag,
      getText = getText,
      getTextAtTag = getTextAtTag,
      uuidName = mkName "uuid",
      titleName = mkName "title",
      nameName = mkName "name",
      urlName = mkName "url",
      authorName = mkName "author",
      publicationName = mkName "publication",
      parseBibElement = parseElement
    }

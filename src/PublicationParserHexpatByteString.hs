{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module PublicationParserHexpatByteString (parser) where

import qualified Data.ByteString as BS
import Data.Maybe (listToMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import PublicationParser (PublicationParser (..))
import Text.XML.Expat.Format (formatNode')
import Text.XML.Expat.Tree (UNode, defaultParseOptions, getAttribute, getChildren, getName, parse')

type WNode = UNode BS.ByteString

bsToString :: BS.ByteString -> String
bsToString = T.unpack . T.decodeUtf8

atTags :: BS.ByteString -> WNode -> [WNode]
atTags tag n = filter (\c -> getName c == tag) $ getChildren n

atTag :: BS.ByteString -> WNode -> Maybe WNode
atTag tag n = listToMaybe $ atTags tag n

getText' :: WNode -> String
getText' n = concatMap (bsToString . formatNode') $ getChildren n

getTextAtTag :: BS.ByteString -> WNode -> Maybe String
getTextAtTag tag n = getText' <$> atTag tag n

getAttrValue :: BS.ByteString -> WNode -> Maybe String
getAttrValue attrName n = bsToString <$> getAttribute n attrName

parseElement :: FilePath -> IO WNode
parseElement file = do
  txt <- BS.readFile file
  let Right bibElement = parse' defaultParseOptions txt
  return bibElement

parser :: PublicationParser WNode BS.ByteString
parser =
  PublicationParser
    { getAttrValue = getAttrValue,
      atTags = atTags,
      atTag = atTag,
      getText = getText',
      getTextAtTag = getTextAtTag,
      uuidName = "uuid",
      titleName = "title",
      nameName = "name",
      urlName = "url",
      authorName = "author",
      publicationName = "publication",
      parseBibElement = parseElement
    }

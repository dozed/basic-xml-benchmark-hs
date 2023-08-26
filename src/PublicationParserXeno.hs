{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module PublicationParserXeno (parser) where

import qualified Data.ByteString as BS
import Data.List (find)
import Data.Maybe (listToMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import PublicationParser (PublicationParser (..))
import Xeno.DOM (Node, attributes, children, contents, name, parse)
import Xeno.Output (showContent)

bsToString :: BS.ByteString -> String
bsToString = T.unpack . T.decodeUtf8

getAttr :: BS.ByteString -> Node -> Maybe (BS.ByteString, BS.ByteString)
getAttr attrName n = find (\(a, _) -> a == attrName) $ attributes n

getAttrValue :: BS.ByteString -> Node -> Maybe String
getAttrValue attrName n = bsToString . snd <$> getAttr attrName n

atTag :: BS.ByteString -> Node -> Maybe Node
atTag tag n = listToMaybe $ atTags tag n

atTags :: BS.ByteString -> Node -> [Node]
atTags tag n = filter (\c -> name c == tag) $ children n

getText :: Node -> String
getText n = concatMap showContent $ contents n

getTextAtTag :: BS.ByteString -> Node -> Maybe String
getTextAtTag tag n = getText <$> atTag tag n

parseElement :: FilePath -> IO Node
parseElement file = do
  txt <- BS.readFile file
  let Right bibElement = parse txt
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

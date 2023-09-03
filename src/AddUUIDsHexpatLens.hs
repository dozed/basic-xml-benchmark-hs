{-# LANGUAGE OverloadedStrings #-}

module AddUUIDsHexpatLens (addUUIDs) where

import Control.Lens (mapMOf, transformM)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.UUID.V4 (nextRandom)
import Text.XML.Expat.Format
import Text.XML.Expat.Lens (attributes)
import Text.XML.Expat.Tree (NodeG (..), UAttributes, UNode, defaultParseOptions, parse')

uuidName :: ByteString
uuidName = "uuid"

uuidTags :: Set ByteString
uuidTags =
  S.fromList
    [ "publication",
      "author"
    ]

ensureUUIDinAttrs :: UAttributes ByteString -> IO (UAttributes ByteString)
ensureUUIDinAttrs attrs =
  case break (\(a, _) -> a == uuidName) attrs of
    (_, _ : _) -> pure attrs
    (_, []) -> do
      uuid <- nextRandom
      let attrs' = (uuidName, T.encodeUtf8 . T.pack . show $ uuid) : attrs
      pure attrs'

addUUID :: UNode ByteString -> IO (UNode ByteString)
addUUID e@(Element n _ _) =
  if S.member n uuidTags
    then mapMOf attributes ensureUUIDinAttrs e
    else pure e
addUUID x = pure x

addUUIDs :: FilePath -> FilePath -> IO ()
addUUIDs inFile outFile = do
  txt <- BS.readFile inFile
  let bibElement = case parse' defaultParseOptions txt of
        Right x -> x
        Left msg -> error $ "Could not parse xml: " <> show msg
  bibElement' <- transformM addUUID bibElement
  let txt' = format' bibElement'
  BS.writeFile outFile txt'
  return ()

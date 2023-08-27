module AddUUIDsXmlDom (addUUIDs) where

import qualified Data.ByteString.Lazy as LBS
import Data.Set (Set)
import qualified Data.Set as S
import Data.UUID.V4 (nextRandom)
import Text.XML.Light.Input (parseXML)
import Text.XML.Light.Output (showContent)
import Text.XML.Light.Types (Attr (..), Content (..), Element (..), QName (..))

mkName :: String -> QName
mkName tag = QName tag Nothing Nothing

uuidName :: QName
uuidName = mkName "uuid"

uuidTags :: Set QName
uuidTags =
  S.fromList
    [ mkName "publication",
      mkName "author"
    ]

ensureUUIDinAttrs :: QName -> [Attr] -> IO [Attr]
ensureUUIDinAttrs name attrs =
  if S.member name uuidTags
    then case break (\a -> attrKey a == uuidName) attrs of
      (_, _ : _) -> pure attrs
      (_, []) -> do
        uuid <- nextRandom
        pure $ Attr uuidName (show uuid) : attrs
    else pure attrs

goContent :: Content -> IO Content
goContent (Elem e) = Elem <$> goElement e
goContent n = pure n

goElement :: Element -> IO Element
goElement (Element name attrs ns line) = do
  attrs' <- ensureUUIDinAttrs name attrs
  ns' <- traverse goContent ns
  let e' = Element name attrs' ns' line
  return e'

addUUIDs :: FilePath -> FilePath -> IO ()
addUUIDs inFile outFile = do
  txt <- LBS.readFile inFile
  contents <- traverse goContent $ parseXML txt
  let txt' = concatMap showContent contents
  writeFile outFile txt'

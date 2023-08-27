{-# LANGUAGE LambdaCase #-}

module AddUUIDsXmlCursor (addUUIDs) where

import qualified Data.ByteString.Lazy as LBS
import Data.Set (Set)
import qualified Data.Set as S
import Data.UUID.V4 (nextRandom)
import Text.XML.Light.Cursor
import Text.XML.Light.Input (parseXML)
import Text.XML.Light.Output (showContent)
import Text.XML.Light.Types (Attr (..), Content (..), Element (..), QName (..))

mkName :: String -> QName
mkName tag = QName tag Nothing Nothing

modifyElementM :: Monad m => (Element -> m Element) -> Cursor -> m Cursor
modifyElementM f = modifyContentM $ \case
  Elem e -> do
    e' <- f e
    pure $ Elem e'
  x -> pure x

modifyAllM :: Monad m => (Cursor -> m Cursor) -> Cursor -> m Cursor
modifyAllM f c = do
  c' <- f c
  case nextDF c' of
    Just next -> modifyAllM f next
    Nothing -> pure c'

uuidName :: QName
uuidName = mkName "uuid"

uuidTags :: Set QName
uuidTags =
  S.fromList
    [ mkName "publication",
      mkName "author"
    ]

ensureUUIDinAttrs :: [Attr] -> IO [Attr]
ensureUUIDinAttrs attrs =
  case break (\a -> attrKey a == uuidName) attrs of
    (_, _ : _) -> pure attrs
    (_, []) -> do
      uuid <- nextRandom
      pure $ Attr uuidName (show uuid) : attrs

maybeAddUUID :: Element -> IO Element
maybeAddUUID e@(Element name attrs content line) =
  if S.member name uuidTags
    then do
      attrs' <- ensureUUIDinAttrs attrs
      let e' = Element name attrs' content line
      return e'
    else return e

addUUIDs :: FilePath -> FilePath -> IO ()
addUUIDs inFile outFile = do
  txt <- LBS.readFile inFile
  let xml = parseXML txt
      c = case fromForest xml of
        Just x -> x
        Nothing -> error "Could not create Cursor from xml"
  c' <- modifyAllM (modifyElementM maybeAddUUID) c
  let xml' = toForest c'
  let txt' = concatMap showContent xml'
  writeFile outFile txt'

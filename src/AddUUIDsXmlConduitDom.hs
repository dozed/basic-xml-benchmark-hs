{-# LANGUAGE OverloadedStrings #-}

module AddUUIDsXmlConduitDom (addUUIDs) where

import Control.Exception (throw)
import qualified Data.ByteString.Lazy as LBS
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import Data.UUID.V4 (nextRandom)
import Text.XML (Document (..), Element (..), Name (..), Node (..), def, documentRoot, parseLBS)
import qualified Text.XML as X

mkName :: T.Text -> Name
mkName tag = Name tag Nothing Nothing

uuidName :: Name
uuidName = mkName "uuid"

uuidTags :: Set Name
uuidTags =
  S.fromList
    [ mkName "publication",
      mkName "author"
    ]

ensureUUIDinAttrs :: Name -> Map Name T.Text -> IO (Map Name T.Text)
ensureUUIDinAttrs name attrs =
  if S.member name uuidTags
    then case M.lookup uuidName attrs of
      Just _ -> pure attrs
      Nothing -> do
        uuid <- nextRandom
        let attrs' = M.insert uuidName (T.pack . show $ uuid) attrs
        pure attrs'
    else pure attrs

goNode :: Node -> IO Node
goNode (NodeElement e) = NodeElement <$> goElement e
goNode n = pure n

goElement :: Element -> IO Element
goElement (Element name attrs ns) = do
  attrs' <- ensureUUIDinAttrs name attrs
  ns' <- traverse goNode ns
  let e' = Element name attrs' ns'
  return e'

addUUIDs :: FilePath -> FilePath -> IO ()
addUUIDs inFile outFile = do
  txt <- LBS.readFile inFile
  doc <- case parseLBS def txt of
    Left err -> throw err
    Right doc -> pure doc
  root' <- goElement . documentRoot $ doc
  let doc' = Document (documentPrologue doc) root' (documentEpilogue doc)
  X.writeFile def outFile doc'

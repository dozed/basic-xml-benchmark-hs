{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}

module AddUUIDsXmlConduitLens (addUUIDs) where

import Control.Lens (Traversal', mapMOf, transformM)
import qualified Data.ByteString.Lazy as LBS
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import Data.UUID.V4 (nextRandom)
import Text.XML (def, parseLBS)
import qualified Text.XML as X
import Text.XML.Lens (Document (..), Element (..), Name (..), attrs, documentRoot)

nameIn :: Set Name -> Traversal' Element Element
nameIn ns f s
  | S.member (elementName s) ns = f s
  | otherwise = pure s
{-# INLINE nameIn #-}

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

ensureUUIDinAttrs :: Map Name T.Text -> IO (Map Name T.Text)
ensureUUIDinAttrs attributes =
  case M.lookup uuidName attributes of
    Just _ -> pure attributes
    Nothing -> do
      uuid <- nextRandom
      let attributes' = M.insert uuidName (T.pack . show $ uuid) attributes
      pure attributes'

addUUID :: Element -> IO Element
addUUID = mapMOf (nameIn uuidTags . attrs) ensureUUIDinAttrs

addUUIDs :: FilePath -> FilePath -> IO ()
addUUIDs inFile outFile = do
  txt <- LBS.readFile inFile
  let doc = case parseLBS def txt of
        Left err -> error $ "Could not parse XML: " <> show err
        Right x -> x
  let root = documentRoot doc
  root' <- transformM addUUID root
  let doc' = Document (documentPrologue doc) root' (documentEpilogue doc)
  X.writeFile def outFile doc'

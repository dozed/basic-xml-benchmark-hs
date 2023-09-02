{-# LANGUAGE OverloadedStrings #-}

module AddUUIDsXmlConduitLens (addUUIDs) where

import Control.Lens (transformM)
import qualified Data.ByteString.Lazy as LBS
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import Data.UUID.V4 (nextRandom)
import Text.XML (def, parseLBS)
import qualified Text.XML as X
import Text.XML.Lens (Document (..), Element (..), Name (..), documentRoot)

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
      let attrs' = M.insert uuidName (T.pack . show $ uuid) attributes
      pure attrs'

addUUID :: Element -> IO Element
addUUID (Element n a cs) =
  if S.member n uuidTags
    then do
      a' <- ensureUUIDinAttrs a
      return $ Element n a' cs
    else pure $ Element n a cs

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

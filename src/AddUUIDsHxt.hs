module AddUUIDsHxt (addUUIDs) where

import Control.Monad (void)
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.UUID.V4 as UUIDv4
import Text.XML.HXT.Core

uuidTags :: Set String
uuidTags =
  S.fromList
    [ "publication",
      "author"
    ]

addUUIDs' :: FilePath -> FilePath -> IOSArrow a Int
addUUIDs' inFile outFile =
  readDocument [withParseHTML no, withWarnings yes, withErrors yes, withValidate no] inFile
    >>> processTopDown (addUUID `when` shouldReceiveUUID)
    >>> writeDocument [withIndent yes, withOutputEncoding utf8] outFile
    >>> getErrStatus
  where
    shouldReceiveUUID = isElem >>> hasNameWith (\n -> S.member (localPart n) uuidTags)

addUUID :: (ArrowXml a, ArrowIO a) => a XmlTree XmlTree
addUUID = (addAttr "uuid" . show) $< arrIO0 UUIDv4.nextRandom

addUUIDs :: FilePath -> FilePath -> IO ()
addUUIDs inFile outFile = void $ runX $ addUUIDs' inFile outFile

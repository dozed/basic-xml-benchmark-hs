{-# LANGUAGE Arrows #-}
{-# LANGUAGE DuplicateRecordFields #-}

module PublicationParserHxt (parsePublications) where

import PublicationData (Author (..), Publication (..))
import Text.XML.HXT.Core

atTag :: ArrowXml a => String -> a XmlTree XmlTree
atTag tag = getChildren >>> isElem >>> hasName tag

textAtTag :: ArrowXml a => String -> a XmlTree String
textAtTag tag = (getChildren >>> hasName tag) `guards` xshowEscapeXml (atTag tag >>> getChildren)

textAtTagOpt :: ArrowXml a => String -> a XmlTree (Maybe String)
textAtTagOpt tag = (textAtTag tag >>> arr Just) `orElse` constA Nothing

childListA :: (ArrowTree a, Tree t) => a (t b) c -> a (t b) [c]
childListA a = listA (getChildren >>> a)

getAuthor :: ArrowXml a => a XmlTree Author
getAuthor =
  hasName "author"
    >>> proc x -> do
      uuid <- getAttrValue "uuid" -< x
      name <- textAtTag "name" -< x
      let author =
            Author
              { uuid = read uuid,
                name = name
              }
      returnA -< author

getPublication :: ArrowXml a => a XmlTree Publication
getPublication =
  hasName "publication"
    >>> proc x -> do
      uuid <- getAttrValue "uuid" -< x
      title <- textAtTag "title" -< x
      url <- textAtTagOpt "url" -< x
      authors <- childListA getAuthor -< x
      let publication =
            Publication
              { uuid = read uuid,
                title = title,
                authors = authors,
                url = url
              }
      returnA -< publication

parsePublications :: FilePath -> IO [Publication]
parsePublications file = do
  let xml = readDocument [withValidate no, withRemoveWS yes] file
  runX (xml >>> getChildren >>> getChildren >>> getPublication)

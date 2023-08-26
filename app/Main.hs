module Main (main) where

import Criterion.Main
import PublicationParser (parsePublications)
import qualified PublicationParserHexml
import qualified PublicationParserHexpatByteString
import qualified PublicationParserHexpatString
import qualified PublicationParserHexpatText
import qualified PublicationParserHxt
import qualified PublicationParserXeno
import qualified PublicationParserXml
import qualified PublicationParserXmlConduitDom
import qualified PublicationParserXmlConduitNodeStream

doBenchmark :: FilePath -> IO ()
doBenchmark file =
  defaultMain
    [ bgroup
        "tp"
        [ bench "xml"                     $ nfIO $ parsePublications PublicationParserXml.parser file,
          bench "hxt"                     $ nfIO $ PublicationParserHxt.parsePublications file,
          bench "hexml"                   $ nfIO $ parsePublications PublicationParserHexml.parser file,
          bench "xeno"                    $ nfIO $ parsePublications PublicationParserXeno.parser file,
          bench "hexpat (ByteString)"     $ nfIO $ parsePublications PublicationParserHexpatByteString.parser file,
          bench "hexpat (String)"         $ nfIO $ parsePublications PublicationParserHexpatString.parser file,
          bench "hexpat (Text)"           $ nfIO $ parsePublications PublicationParserHexpatText.parser file,
          bench "xml-conduit (DOM)"       $ nfIO $ parsePublications PublicationParserXmlConduitDom.parser file,
          bench "xml-conduit (StreamDOM)" $ nfIO $ PublicationParserXmlConduitNodeStream.parsePublications file
        ]
    ]

main :: IO ()
main = do
  doBenchmark "data/neurips2022.xml"

module Main (main) where

import qualified AddUUIDsHxt
import qualified AddUUIDsXmlCursor
import qualified AddUUIDsXmlDom
import qualified AddUUIDsHexpatLens
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
import System.IO (hClose, openTempFile)

doBenchmark :: IO ()
doBenchmark = do
  (tmpFile, h) <- openTempFile "/tmp" "bench.xml"
  hClose h

  defaultMain
    [ parseBenchmark "data/neurips2022.xml",
      addUUIDsBenchmark "data/neurips2022-without-uuid.xml" tmpFile
    ]

parseBenchmark :: FilePath -> Benchmark
parseBenchmark file =
  bgroup
    "parse"
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

addUUIDsBenchmark :: FilePath -> FilePath -> Benchmark
addUUIDsBenchmark xmlFile tmpFile =
  bgroup
    "add-uuids"
    [ bench "xml (DOM)"         $ nfIO $ AddUUIDsXmlDom.addUUIDs xmlFile tmpFile,
      bench "xml (Cursor)"      $ nfIO $ AddUUIDsXmlCursor.addUUIDs xmlFile tmpFile,
      bench "hxt"               $ nfIO $ AddUUIDsHxt.addUUIDs xmlFile tmpFile,
      bench "hexpat (Lens)"     $ nfIO $ AddUUIDsHxt.addUUIDs xmlFile tmpFile,
      bench "xml-conduit (DOM)" $ nfIO $ AddUUIDsHexpatLens.addUUIDs xmlFile tmpFile
    ]

main :: IO ()
main = do
  doBenchmark

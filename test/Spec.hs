{-# LANGUAGE DuplicateRecordFields #-}

import PublicationData
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
import Test.Hspec

expected :: [Publication]
expected =
  [ Publication
      { uuid = read "ab929494-2cd9-4ba0-9762-2b92781a12be",
        title = "On Kernelized Multi-Armed Bandits with Constraints.",
        authors =
          [ Author
              { uuid = read "fb6536ec-6108-43dd-afc4-c66784a5361b",
                name = "Xingyu Zhou"
              },
            Author
              { uuid = read "c263a753-c2e2-4429-99fb-9e78490d4331",
                name = "Bo Ji"
              }
          ],
        url = Just "http://papers.nips.cc/paper_files/paper/2022/hash/00295cede6e1600d344b5cd6d9fd4640-Abstract-Conference.html"
      },
    Publication
      { uuid = read "fa771a3c-ddfc-42bc-b91c-3ad305e3bc9e",
        title = "Fast Bayesian Coresets via Subsampling and Quasi-Newton Refinement.",
        authors =
          [ Author
              { uuid = read "d583ffce-73de-4939-8ac5-0ece79b297cf",
                name = "Cian Naik"
              },
            Author
              { uuid = read "f665ec49-9e91-4c90-a810-37efb498eeed",
                name = "Judith Rousseau"
              },
            Author
              { uuid = read "ea7f4a22-0e56-4656-a13d-9961ff247267",
                name = "Trevor Campbell"
              }
          ],
        url = Just "http://papers.nips.cc/paper_files/paper/2022/hash/005413e90d003d13886019607b037f52-Abstract-Conference.html"
      }
  ]

testFile :: String
testFile = "data/neurips2022-test.xml"

spec :: Spec
spec = do
  describe "xml" $
    it "should parse publications" $ do
      res <- parsePublications PublicationParserXml.parser testFile
      res `shouldBe` expected

  describe "hxt" $
    it "should parse publications" $ do
      res <- PublicationParserHxt.parsePublications testFile
      res `shouldBe` expected

  describe "hexml" $
    it "should parse publications" $ do
      res <- parsePublications PublicationParserHexml.parser testFile
      res `shouldBe` expected

  describe "xeno" $
    it "should parse publications" $ do
      res <- parsePublications PublicationParserXeno.parser testFile
      res `shouldBe` expected

  describe "hexpat (ByteString)" $
    it "should parse publications" $ do
      res <- parsePublications PublicationParserHexpatByteString.parser testFile
      res `shouldBe` expected

  describe "hexpat (String)" $
    it "should parse publications" $ do
      res <- parsePublications PublicationParserHexpatString.parser testFile
      res `shouldBe` expected

  describe "hexpat (Text)" $
    it "should parse publications" $ do
      res <- parsePublications PublicationParserHexpatText.parser testFile
      res `shouldBe` expected

  describe "xml-conduit (DOM)" $
    it "should parse publications" $ do
      res <- parsePublications PublicationParserXmlConduitDom.parser testFile
      res `shouldBe` expected

  describe "xml-conduit (StreamDOM)" $
    it "should parse publications" $ do
      res <- PublicationParserXmlConduitNodeStream.parsePublications testFile
      res `shouldBe` expected

main :: IO ()
main = hspec spec

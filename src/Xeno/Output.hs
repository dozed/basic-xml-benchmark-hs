{-# LANGUAGE OverloadedStrings #-}

module Xeno.Output where

import qualified Data.ByteString as BS
import Data.Char
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Xeno.DOM

-- Adapted from: Text.XML.Light.Output

type QName = BS.ByteString
type CData = BS.ByteString

prettify :: Bool
prettify = True

-- | The XML 1.0 header
xmlHeader :: String
xmlHeader = "<?xml version='1.0' ?>"

bsToString :: BS.ByteString -> String
bsToString = T.unpack . T.decodeUtf8


--------------------------------------------------------------------------------


-- | Pretty printing renders XML documents faithfully,
-- with the exception that whitespace may be added\/removed
-- in non-verbatim character data.
ppTopElement :: Node -> String
ppTopElement = ppcTopElement

-- | Pretty printing elements
ppElement :: Node -> String
ppElement = ppcElement

-- | Pretty printing content
ppContent :: Content -> String
ppContent = ppcContent


-- | Pretty printing renders XML documents faithfully,
-- with the exception that whitespace may be added\/removed
-- in non-verbatim character data.
ppcTopElement :: Node -> String
ppcTopElement e = unlines [xmlHeader, ppcElement e]

-- | Pretty printing elements
ppcElement :: Node -> String
ppcElement e = ppElementS "" e ""

-- | Pretty printing content
ppcContent :: Content -> String
ppcContent x = ppContentS "" x ""



-- | Pretty printing content using ShowS
ppContentS :: String -> Content -> ShowS
ppContentS i x xs = case x of
                      Element n -> ppElementS i n xs
                      Text t -> ppCDataS i t xs
                      CData r -> showCRefS r xs

ppElementS :: String -> Node -> ShowS
ppElementS i n xs = i ++ (tagStart tag (attributes n) $
  case contents n of
    [] | "?" `BS.isPrefixOf` tag -> " ?>" ++ xs
       | True  -> " />" ++ xs
    [Text t] -> ">" ++ ppCDataS "" t (tagEnd tag xs)
    cs -> '>' : nl ++ foldr ppSub (i ++ tagEnd tag xs) cs
      where ppSub e1 = ppContentS (sp ++ i) e1 . showString nl
            (nl,sp)  = if prettify then ("\n","  ") else ("","")
  )
  where tag = name n

ppCDataS :: String -> CData -> ShowS
ppCDataS i t xs   = i ++ showCDataS t xs

--ppCDataS           :: ConfigPP -> String -> CData -> ShowS
--ppCDataS c i t xs   = i ++ if cdVerbatim t /= CDataText || not (prettify c)
--                             then showCDataS t xs
--                             else foldr cons xs (showCData t)
--
--  where cons         :: Char -> String -> String
--        cons '\n' ys  = "\n" ++ i ++ ys
--        cons y ys     = y : ys



--------------------------------------------------------------------------------

-- -- | Adds the <?xml?> header.
showTopElement     :: Node -> String
showTopElement c    = xmlHeader ++ showElement c

showContent        :: Content -> String
showContent c       = ppContentS "" c ""

showElement :: Node -> String
showElement c = ppElementS "" c ""

showCData :: CData -> String
showCData c = ppCDataS "" c ""

-- Note: crefs should not contain '&', ';', etc.
showCRefS :: BS.ByteString -> ShowS
showCRefS r xs = '&' : bsToString r ++ ';' : xs

-- | Convert a text element to characters.
showCDataS :: CData -> ShowS
showCDataS cd = escStr (bsToString cd)
-- case cdVerbatim cd of
--   CDataText     -> escStr (cdData cd)
--   CDataVerbatim -> showString "<![CDATA[" . escCData (cdData cd)
--                                           . showString "]]>"
--   CDataRaw      -> \ xs -> cdData cd ++ xs

--------------------------------------------------------------------------------
escCData :: String -> ShowS
escCData (']' : ']' : '>' : cs) = showString "]]]]><![CDATA[>" . escCData cs
escCData (c : cs)               = showChar c . escCData cs
escCData []                     = id

escChar :: Char -> ShowS
escChar c = case c of
  '<' -> showString "&lt;"
  '>' -> showString "&gt;"
  '&' -> showString "&amp;"
  '"' -> showString "&quot;"
  -- we use &#39 instead of &apos; because IE apparently has difficulties
  -- rendering &apos; in xhtml.
  -- Reported by Rohan Drape <rohan.drape@gmail.com>.
  '\'' -> showString "&#39;"
  -- NOTE: We escape '\r' explicitly because otherwise they get lost
  -- when parsed back in because of then end-of-line normalization rules.
  _
    | isPrint c || c == '\n' -> showChar c
    | otherwise -> showString "&#" . shows oc . showChar ';'
    where
      oc = ord c

escStr :: String -> ShowS
escStr cs rs = foldr escChar rs cs

tagEnd :: QName -> ShowS
tagEnd qn rs = '<':'/':bsToString qn ++ '>':rs

tagStart :: QName -> [(QName, BS.ByteString)] -> ShowS
tagStart qn as rs = '<' : bsToString qn ++ as_str ++ rs
  where
    as_str = if null as then "" else ' ' : unwords (map showAttr as)

showAttr :: (QName, QName) -> String
showAttr (qn, v) = bsToString qn ++ '=' : '"' : escStr (bsToString v) "\""

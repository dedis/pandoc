{-# LANGUAGE OverloadedStrings #-}
module Tests.Writers.MinML (tests) where

import Data.Text (Text)
import qualified Data.Text as T
import Test.Tasty (TestTree)
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Tests.Helpers (purely)
import Text.Pandoc
import qualified Text.Pandoc.Builder as B

options :: WriterOptions
options = def{ writerWrapText = WrapNone }

readerOptions :: ReaderOptions
readerOptions = def{ readerExtensions = enableExtension Ext_raw_html $
                                      getDefaultExtensions "minml" }

roundTrip :: Pandoc -> Bool
roundTrip doc =
  purely (readMinML readerOptions) (purely (writeMinML options) doc) ==
  purely (readHtml readerOptions) (purely (writeHtml5String options) doc)

check :: String -> B.Blocks -> TestTree
check name blocks = testCase name $
  assertBool "MinML and HTML should read to the same document" $ roundTrip (B.doc blocks)

tests :: [TestTree]
tests =
  [ testCase "paragraph output" $
      purely (writeMinML options) (B.doc $ B.para "hello") @?= "p[hello]"
  , testCase "adjacent text and markup" $
      purely (writeMinML options) (B.doc $ B.para $ "a" <> B.emph "b" <> "c")
        @?= "p[a <em[b]c]"
  , check "reserved characters" $ B.para $ B.text "a[b]{c}(d) > < & \" '"
  , check "unmatched matchers" $ B.para $ B.text ") ] } ( [ {"
  , check "literal MinML and character references" $
      B.para $ B.text "em[hello] [amp] [#13] +[raw] -[comment]"
  , check "whitespace controls remain text" $
      B.codeBlock "> begin\nend <\na <b[]> c\n"
  , check "attributes" $ B.para $
      B.link "https://example.org/a(b)?x=[1]&y=2" "A ] title" "link"
  , check "images and breaks" $ B.para $
      "a" <> B.linebreak <> B.image "cat.png" "cat" "cat" <> "b"
  , check "nested lists" $
      B.bulletList [B.para "one", B.para "two" <> B.orderedList [B.para "nested"]]
  , check "Unicode and quotations" $ B.para $
      B.doubleQuoted (B.text "é λ 😀") <> B.singleQuoted "quote"
  , check "footnotes" $ B.para $ "text" <> B.note (B.para "a note")
  , check "raw HTML" $ B.rawBlock "html"
      "<div data-value='a]b'><custom-element>text</custom-element></div>"
  , check "custom elements containing blocks" $ B.rawBlock "html"
      "<custom-widget><div>text</div></custom-widget>"
  , check "SVG foreign content" $ B.rawBlock "html"
      "<svg><foreignObject><div><p>text</p></div></foreignObject></svg>"
  , check "mixed-case HTML and void elements" $ B.rawBlock "html"
      "<DIV><INPUT value='hello'><SPAN>one</span><br>two</DIV>"
  , check "omitted HTML end tags" $ B.rawBlock "html"
      "<ul><li>one<li>two</ul><p>first<p>second"
  , check "raw script and stylesheet" $ B.rawBlock "html"
      "<script>if (a < b) { alert('x[y]'); }</script><style>p { color: red; }</style>"
  , check "raw comments" $ B.para $ B.rawInline "html" "<!-- unmatched ) ] [ ( -->"
  , testCase "balanced comments use MinML syntax" $
      purely (writeMinML options)
        (B.doc $ B.para $ "a" <> B.rawInline "html" "<!-- x [--] (y) -->" <> "b")
        @?= "p[a <-[ x [--] (y) ]b]"
  , check "balanced comment containing --]" $ B.para $
      "a" <> B.rawInline "html" "<!-- x [--] (y) -->" <> "b"
  , testCase "unmatched comments use XML syntax" $
      purely (writeMinML options)
        (B.doc $ B.para $ "a" <> B.rawInline "html" "<!-- ) -->" <> "b")
        @?= "p[a <![-- ) --]b]"
  , testCase "unrepresentable comment" $
      case runPure $ writeMinML options
             (B.doc $ B.para $ B.rawInline "html" "<!-- ) --] -->") of
        Left PandocAppError{} -> return ()
        Left err -> assertFailure $ show err
        Right result -> assertFailure $ "Expected failure, got " <> T.unpack result
  , testCase "invalid attribute name" $
      case runPure $ writeMinML options
             (B.doc $ B.rawBlock "html" "<div @click='x'>t</div>") of
        Left PandocAppError{} -> return ()
        Left err -> assertFailure $ show err
        Right result -> assertFailure $ "Expected failure, got " <> T.unpack result
  , testCase "ascii output" $
      purely (writeMinML options{ writerPreferAscii = True })
        (B.doc $ B.para $ B.text "café λ" <> B.emph "😀")
        @?= "p[caf <[#233] [#955]em[[#128512]]]"
  , check "ascii output reads back" $ B.para $ B.text "café λ 😀 [x]"
  , testCase "raw MinML block" $
      purely (writeMinML options) (B.doc $ B.rawBlock "minml" "p[hello]")
        @?= "p[hello]"
  , testCase "raw MinML inline" $
      purely (writeMinML options)
        (B.doc $ B.para $ "a" <> B.rawInline "minml" "em[b]" <> "c")
        @?= "p[a <em[b]c]"
  , testCase "XML input to MinML and back to XML" $ do
      let xml = "<Pandoc><meta/><blocks><Para>Hello <Emph>world</Emph>.</Para>" <>
                "</blocks></Pandoc>" :: Text
          doc = purely (readXML def) xml
          back = purely (readMinML readerOptions) (purely (writeMinML options) doc)
      purely (writeXML def) back @?= purely (writeXML def) doc
  , testCase "standalone HTML template" $ do
      let doc = B.doc $ B.para "hello"
          convert = do
            template <- compileDefaultTemplate "minml"
            let opts = options{ writerTemplate = Just template }
            minml <- writeMinML opts doc
            html <- writeHtml5String opts doc
            (,) <$> readMinML readerOptions minml <*> readHtml readerOptions html
      case runPure convert of
        Left err -> assertFailure $ show err
        Right (actual, expected) -> actual @?= expected
  , testCase "maximum nesting" $ do
      let html = T.replicate 256 "<span>" <> "x" <> T.replicate 256 "</span>"
      case runPure $ writeMinML options (B.doc $ B.rawBlock "html" html)
                     >>= readMinML readerOptions of
        Left err -> assertFailure $ show err
        Right _ -> return ()
  , testCase "nested escaped attributes" $ do
      let html = T.replicate 253 "<span>" <> "<span title='['>x</span>" <>
                 T.replicate 253 "</span>"
      case runPure $ writeMinML options (B.doc $ B.rawBlock "html" html)
                     >>= readMinML readerOptions of
        Left err -> assertFailure $ show err
        Right _ -> return ()
  , testCase "nesting limits include references and attributes" $
      mapM_ (\html -> case runPure $ writeMinML options (B.doc $ B.rawBlock "html" html) of
               Left err -> assertBool (show err) $ "nesting" `T.isInfixOf` renderError err
               Right result -> assertFailure $ "Expected nesting limit, got " <> T.unpack result)
        [ T.replicate 257 "<span>" <> "x" <> T.replicate 257 "</span>"
        , T.replicate 256 "<span>" <> "[" <> T.replicate 256 "</span>"
        , T.replicate 254 "<span>" <> "<span title='['>x</span>" <>
          T.replicate 254 "</span>"
        ]
  , testProperty "arbitrary code text survives serialization" $
      forAll (listOf $ elements ['\t', '\n', ' ', 'a', 'Z', 'é', 'λ', '😀',
                                '(', ')', '[', ']', '{', '}', '<', '>', '&',
                                '\'', '"', '#', '+', '-', '=', ':', '/']) $ \s ->
        roundTrip $ B.doc $ B.codeBlock (T.pack s)
  ]

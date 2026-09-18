{-# LANGUAGE OverloadedStrings #-}
module Tests.Readers.MinML (tests) where

import Data.Text (Text)
import qualified Data.Text as T
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import Tests.Helpers (purely)
import Text.Pandoc
import Text.Pandoc.Format (FlavoredFormat (formatName), formatFromFilePaths)

options :: ReaderOptions
options = def{ readerExtensions = enableExtension Ext_raw_html $
                                getDefaultExtensions "minml" }

equivalent :: String -> Text -> Text -> TestTree
equivalent name minml html = testCase name $
  purely (readMinML options) minml @?= purely (readHtml options) html

equivalentXML :: String -> Text -> Text -> TestTree
equivalentXML name minml xml = testCase name $
  purely (readMinML options) minml @?= purely (readXML def) xml

invalid :: Text -> TestTree
invalid input = testCase (T.unpack input) $
  case runPure $ readMinML options input of
    Left PandocParseError{} -> return ()
    Left err -> assertFailure $ show err
    Right doc -> assertFailure $ "Expected parse failure, got " <> show doc

tests :: [TestTree]
tests =
  [ testGroup "extension detection"
      [ testCase path $
          (formatName <$> formatFromFilePaths [path]) @?= Just "minml"
      | path <- ["document.m", "document.minml", "document.M"]
      ]
  , testGroup "elements"
    [ equivalent "empty document" "" ""
    , equivalent "paragraph" "p[Hello]" "<p>Hello</p>"
    , equivalent "nested inline elements" "p[Hello em[strong[world]].]"
        "<p>Hello <em><strong>world</strong></em>.</p>"
    , equivalent "adjacent elements" "p[one]p[two]" "<p>one</p><p>two</p>"
    , equivalent "headings and identifiers" "h1{ id=title }[Title]"
        "<h1 id=title>Title</h1>"
    , equivalent "void elements" "p[a br[]b img{src=cat.png alt=cat}[]]hr[]"
        "<p>a <br>b <img src=cat.png alt=cat></p><hr>"
    , equivalent "lists" "ul[li[one]li[em[two]]]"
        "<ul><li>one</li><li><em>two</em></li></ul>"
    , equivalent "table" "table[tr[th[A]th[B]]tr[td[1]td[2]]]"
        "<table><tr><th>A</th><th>B</th></tr><tr><td>1</td><td>2</td></tr></table>"
    , equivalent "namespace-qualified elements" "h:p{xmlns:h=urn:test}[text]"
        "<h:p xmlns:h=urn:test>text</h:p>"
    , equivalent "custom elements" "p[custom-widget{data-value=42}[text]]"
        "<p><custom-widget data-value=42>text</custom-widget></p>"
    , equivalent "MathML"
        "math{xmlns=http://www.w3.org/1998/Math/MathML}[msup[mi[x]mn[2]]]"
        "<math xmlns='http://www.w3.org/1998/Math/MathML'><msup><mi>x</mi><mn>2</mn></msup></math>"
    , equivalent "SVG"
        "svg{xmlns=http://www.w3.org/2000/svg}[circle{cx=10 cy=10 r=5}[]]"
        "<svg xmlns='http://www.w3.org/2000/svg'><circle cx='10' cy='10' r='5'></circle></svg>"
    ]
  , testGroup "attributes"
    [ equivalent "quoted attributes and entities"
        "a{href=[https://example.org/?x=1[amp]y=2] title=[A [quot]quote[quot]]}[link]"
        "<a href='https://example.org/?x=1&amp;y=2' title='A &quot;quote&quot;'>link</a>"
    , equivalent "empty attributes" "input{disabled= value=[]}[]"
        "<input disabled='' value=''>"
    , equivalent "quoted whitespace controls"
        "span{title=[> a <] data-x=[x <[> y <]> ]}[text]"
        "<span title='a' data-x='x[y]'>text</span>"
    , equivalent "literal element names in attribute values"
        "span{title=[em[text]]}[x]" "<span title='em&text;'>x</span>"
    , equivalent "unquoted nested matchers"
        "span{title=(x y) data-value={a b}}[text]"
        "<span title='(x y)' data-value='{a b}'>text</span>"
    , equivalent "Unicode attribute names"
        "span{é=[value] x́=1}[text]" "<span é='value' x́='1'>text</span>"
    ]
  , testGroup "references and literal matchers"
    [ equivalent "named and numeric references"
        "p[[reg] [#174] [#xAE] [#x1F600] [NotEqualTilde]]"
        "<p>® ® ® 😀 ≂̸</p>"
    , equivalent "symbolic matcher references"
        "pre[[(<)][(>)][[<]][[>]][{<}][{>}]]" "<pre>()[]{}</pre>"
    , equivalent "symbolic math references"
        "p[[--] [---] [+-] [x] [2rt] [<=] [<->] [1/2]]"
        "<p>– — ± × √ ≤ ↔ ½</p>"
    , equivalent "numeric references with leading zeroes"
        "p[[#0000000000000000000065] [#x0000000000000000000041]]" "<p>A A</p>"
    , equivalent "unknown reference" "p[[unknown]]" "<p>&unknown;</p>"
    , equivalent "empty and whitespace-containing brackets"
        "pre[[] [ x ] [xx ] [ xx] (){}]" "<pre>[] [ x ] [xx ] [ xx] (){}</pre>"
    , equivalent "nested literal matchers" "p[([{x}])]" "<p>([{x}])</p>"
    , equivalent "references inside literal matchers"
        "p[([amp]) [[amp]] {[amp]}]" "<p>(&amp;) [&amp;] {&amp;}</p>"
    , equivalent "literal angle references" "p[[<] [>]]" "<p>[&lt;] [&gt;]</p>"
    , equivalent "single and double quotations" "p[\"[A '[nested] quote]]"
        "<p>“A ‘nested’ quote”</p>"
    ]
  , testGroup "whitespace"
    [ equivalent "both sides" "p[bee <em[yoo]> tiful]" "<p>bee<em>yoo</em>tiful</p>"
    , equivalent "left only" "p[mark <em[up] now]" "<p>mark<em>up</em> now</p>"
    , equivalent "right only" "p[now em[mark]> up]" "<p>now <em>mark</em>up</p>"
    , equivalent "inner boundaries" "p[a <b[> b <]> c]" "<p>a<b>b</b>c</p>"
    , equivalent "literal square brackets" "p[b <[1 <[hellip]> 10]]"
        "<p>b[1…10]</p>"
    , equivalent "literal braces" "p[set <{a,b,c}]" "<p>set{a,b,c}</p>"
    , equivalent "escaped reference" "p[[> star <]]" "<p>[star]</p>"
    , equivalent "controls need whitespace" "pre[a <b[><]>x]"
        "<pre>a<b>&gt;&lt;</b>&gt;x</pre>"
    , equivalent "controls do not affect parentheses"
        "pre[a <(> x <)> b]" "<pre>a &lt;(&gt; x &lt;)&gt; b</pre>"
    , equivalent "empty controlled content" "p[> <]" "<p></p>"
    , equivalent "newlines and tabs" "pre[x\n\t <em[> \n y \t<]> \nz]"
        "<pre>x<em>y</em>z</pre>"
    ]
  , testGroup "raw text and directives"
    [ equivalent "raw nested matchertext"
        "pre[+[example +[matchertext] and em[text] (x) {y}]]"
        "<pre>example +[matchertext] and em[text] (x) {y}</pre>"
    , equivalent "raw HTML is text" "p[+[<b>text</b> &amp;]]"
        "<p>&lt;b&gt;text&lt;/b&gt; &amp;amp;</p>"
    , equivalent "raw controls stay literal" "pre[+[> a <]]" "<pre>&gt; a &lt;</pre>"
    , equivalent "comment" "p[a -[ > ({[]}) < ] b]"
        "<p>a <!-- > ({[]}) < --> b</p>"
    , equivalent "XML-style comment with unmatched matchers"
        "p[a ![-- unmatched ) ] [ ( --] b]"
        "<p>a <!-- unmatched ) ] [ ( --> b</p>"
    , equivalent "declaration and processing instruction"
        "?[xml version=\"1.0\"]![DOCTYPE html]p[text]"
        "<?xml version=\"1.0\"?><!DOCTYPE html><p>text</p>"
    , equivalent "quoted delimiters in declarations"
        "![DOCTYPE html SYSTEM \"a(b.dtd\"]p[text]"
        "<!DOCTYPE html SYSTEM \"a(b.dtd\"><p>text</p>"
    , equivalent "CDATA with unmatched matchers and quotes"
        "p[![[CDATA[unmatched ) ] [ ( ']]]]" "<p><![CDATA[unmatched ) ] [ ( ']]></p>"
    , equivalent "declaration internal subset"
        "![DOCTYPE html [<!ENTITY test 'a]b'>]]p[text]"
        "<!DOCTYPE html [<!ENTITY test 'a]b'>]><p>text</p>"
    , equivalent "script and stylesheet"
        "script[+[if (a < b) { alert('x[y]'); }]]style[+[p { color: red; }]]"
        "<script>if (a < b) { alert('x[y]'); }</script><style>p { color: red; }</style>"
    ]
  , testGroup "invalid syntax" $ map invalid
    [ "a(b", "b)c", "a[b", "a]b", "a{b", "a}b", "a(]b", "a{)b"
    , "p{}", "p{}[", "p{a}[]", "p{a=x b}[]", "p{a=[x]y}[]"
    , "p{ a= <[]}[]", "p{ a=[]> }[]", "p{1x=y}[]", "p{x =y}[]"
    , "p{} []", "+[)]", "-[)]", "+[?[)]]", "![DOCTYPE html"
    , "[#0]", "[#xD800]", "[#1114112]", "[#xg]", "[#x]"
    ]
  , testCase "source position in syntax errors" $
      case runPure $ readMinML options
             ([("broken.minml", "p[\ntext")] :: [(FilePath, Text)]) of
        Left err -> assertBool (show err) $
          "broken.minml" `T.isInfixOf` renderError err &&
          "line 3" `T.isInfixOf` renderError err
        Right _ -> assertFailure "Expected parse failure"
  , testCase "bounded nesting" $
      case runPure $ readMinML options (T.replicate 257 "p[" <> T.replicate 257 "]") of
        Left err -> assertBool (show err) $ "nesting" `T.isInfixOf` renderError err
        Right _ -> assertFailure "Expected nesting limit"
  , testCase "declarations obey nesting limit" $
      case runPure $ readMinML options
             (T.replicate 256 "span[" <> "![DOCTYPE html]" <> T.replicate 256 "]") of
        Left err -> assertBool (show err) $ "nesting" `T.isInfixOf` renderError err
        Right _ -> assertFailure "Expected nesting limit"
  , testCase "large raw text" $
      let text = T.replicate 100000 "abc "
      in purely (readMinML options) ("pre[+[" <> text <> "]]") @?=
           purely (readHtml options) ("<pre>" <> text <> "</pre>")
  , testGroup "Pandoc XML vocabulary"
    [ equivalentXML "document"
        "?[xml version='1.0']Pandoc[meta[]blocks[Para[Hello Emph[world].]]]"
        "<Pandoc><meta/><blocks><Para>Hello <Emph>world</Emph>.</Para></blocks></Pandoc>"
    , equivalentXML "namespace-qualified root"
        "p:Pandoc{xmlns:p=urn:pandoc}[meta[]blocks[Para[text]]]"
        "<p:Pandoc xmlns:p='urn:pandoc'><meta/><blocks><Para>text</Para></blocks></p:Pandoc>"
    , equivalentXML "metadata"
        "Pandoc[meta[entry{key=title}[MetaString[Title [amp] subtitle]]]blocks[]]"
        "<Pandoc><meta><entry key='title'><MetaString>Title &amp; subtitle</MetaString></entry></meta><blocks/></Pandoc>"
    , equivalentXML "CDATA and raw text"
        "Pandoc[blocks[CodeBlock[![[CDATA[a]b ']]]]RawBlock{format=html}[+[<b>text</b>]]]]"
        "<Pandoc><blocks><CodeBlock><![CDATA[a]b ']]></CodeBlock><RawBlock format='html'>&lt;b&gt;text&lt;/b&gt;</RawBlock></blocks></Pandoc>"
    ]
  ]

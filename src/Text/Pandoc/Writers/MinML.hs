{-# LANGUAGE OverloadedStrings #-}
-- | Write MinML using the HTML writer's document mapping and templates.
module Text.Pandoc.Writers.MinML (writeMinML) where

import Control.Monad.Except (throwError)
import Data.List (intersperse)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as B
import Text.HTML.TagSoup
import Text.Pandoc.Class (PandocMonad)
import Text.Pandoc.Definition (Pandoc, Block (..), Inline (..), Format (..))
import Text.Pandoc.Error (PandocError)
import Text.Pandoc.MinML (checkNesting, escapeMinML, parseMinML, toHtmlTags)
import Text.Pandoc.Options (WriterOptions)
import Text.Pandoc.Readers.HTML.Parsing (closes)
import Text.Pandoc.Readers.HTML.TagCategories (voidTags)
import Text.Pandoc.Shared (renderTags')
import Text.Pandoc.Walk (walkM)
import Text.Pandoc.Writers.HTML (writeHtml5String)

writeMinML :: PandocMonad m => WriterOptions -> Pandoc -> m Text
writeMinML opts doc = do
  doc' <- walkM rawBlock doc >>= walkM rawInline
  html <- writeHtml5String opts doc'
  either throwError (return . TL.toStrict . B.toLazyText) $
    render [] False $ parseTags html

rawBlock :: PandocMonad m => Block -> m Block
rawBlock (RawBlock (Format "minml") text) = RawBlock (Format "html") <$> rawHtml text
rawBlock block = return block

rawInline :: PandocMonad m => Inline -> m Inline
rawInline (RawInline (Format "minml") text) = RawInline (Format "html") <$> rawHtml text
rawInline inline = return inline

rawHtml :: PandocMonad m => Text -> m Text
rawHtml = either throwError (return . renderTags' . toHtmlTags) . parseMinML

-- Close still-open elements at EOF, including raw HTML with omitted end tags.
render :: [Text] -> Bool -> [Tag Text] -> Either PandocError B.Builder
render stack previousName tags = case tags of
  [] -> return $ foldMap (const "]") stack
  TagText text : rest -> do
    (lastName, escaped) <- escapeMinML depth previousName text
    (escaped <>) <$> render stack lastName rest
  TagOpen name attrs : rest
    | top : outer <- stack, top `elem` optionalEndTags
    , T.toLower name `closes` top
    , not (any (`elem` ["svg", "math"]) stack) ->
        ("]" <>) <$> render outer False tags
    | "!" `T.isPrefixOf` name || "?" `T.isPrefixOf` name -> do
        checkNesting (depth + 1)
        let raw = renderTags [TagOpen name attrs]
            body = T.dropEnd 1 (T.drop 1 raw)
            (prefix, content) = T.splitAt 1 body
            payload = if prefix == "?" then T.dropEnd 1 content else content
            text = prefix <> "[" <> payload <> "]"
        ((padding <> B.fromText text) <>) <$> render stack False rest
    | otherwise -> do
        checkNesting (depth + 1)
        attrs' <- attributes attrs
        let opening = padding <> B.fromText name <> attrs' <> "["
        if T.toLower name `Set.member` voidTags
           then ((opening <> "]") <>) <$> render stack False rest
           else (opening <>) <$> render (T.toLower name : stack) False rest
  TagClose name : rest -> case break (== T.toLower name) stack of
    (_, []) -> render stack previousName rest
    (inner, _ : outer) ->
      (foldMap (const "]") (name : inner) <>) <$> render outer False rest
  TagComment text : rest -> do
    checkNesting (depth + 1)
    ((padding <> "![--" <> B.fromText text <> "--]") <>) <$>
      render stack False rest
  TagWarning _ : rest -> render stack previousName rest
  TagPosition _ _ : rest -> render stack previousName rest
 where
  depth = length stack
  padding = if previousName then " <" else mempty
  attributes [] = return mempty
  attributes attrs = do
    checkNesting (depth + 2)
    rendered <- mapM attribute attrs
    return $ "{" <> mconcat (intersperse " " rendered) <> "}"
  attribute (name, value) = do
    (_, escaped) <- escapeMinML (depth + 2) False value
    return $ B.fromText name <> "=[" <> escaped <> "]"

-- Only infer omitted end tags where HTML permits them. In particular, do
-- not apply the HTML reader's recovery rules to arbitrary custom elements.
optionalEndTags :: [Text]
optionalEndTags =
  [ "head", "p", "li", "dt", "dd", "rb", "rt", "rtc", "rp"
  , "option", "optgroup", "colgroup", "thead", "tbody", "tfoot", "tr", "td", "th"
  ]

{-# LANGUAGE OverloadedStrings #-}
-- | Read MinML markup using HTML or Pandoc XML document semantics.
module Text.Pandoc.Readers.MinML (readMinML) where

import Control.Monad.Except (throwError)
import qualified Data.Text as T
import Text.Pandoc.Class (PandocMonad)
import Text.Pandoc.Definition (Pandoc)
import Text.Pandoc.MinML
import Text.Pandoc.Options (ReaderOptions)
import Text.Pandoc.Readers.HTML (readHtmlTokens)
import Text.Pandoc.Readers.XML (readXML)
import Text.Pandoc.Sources (ToSources)

readMinML :: (PandocMonad m, ToSources a)
          => ReaderOptions -> a -> m Pandoc
readMinML opts input = do
  nodes <- either throwError return $ parseMinML input
  case [T.takeWhileEnd (/= ':') name | Element name _ _ <- nodes] of
    "Pandoc" : _ -> readXML opts (renderXML nodes)
    _ -> readHtmlTokens opts (toHtmlTags nodes)

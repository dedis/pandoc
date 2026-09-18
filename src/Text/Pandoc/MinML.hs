{-# LANGUAGE OverloadedStrings #-}
-- | MinML syntax shared by the reader and writer.
module Text.Pandoc.MinML
  ( Node (..)
  , parseMinML
  , toHtmlTags
  , renderXML
  , escapeMinML
  , checkNesting
  , validAttributeName
  , validElementName
  ) where

import Control.Monad (unless, void, when)
import Data.Bifunctor (first)
import Data.Char (chr, isHexDigit, ord)
import Data.Functor.Identity (Identity)
import qualified Data.Map.Strict as M
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as B
import Numeric (readHex)
import Text.HTML.TagSoup (Tag (..), parseTags)
import Text.Pandoc.Error (PandocError (..))
import Text.Pandoc.Parsing
import Text.Pandoc.Readers.HTML.TagCategories (voidTags)
import Text.Pandoc.XML (escapeStringForXML, lookupEntity)
import Text.Read (readMaybe)

data Node
  = Element Text [(Text, Text)] [Node]
  | Literal Text
  | Comment Text
  | Declaration Text
  | Instruction Text
  deriving (Eq, Show)

-- First check matchertext once. Interpreting the resulting tokens never
-- backtracks over nested markup or reparses a failed character reference.
data Token = Chunk SourcePos Text | Pair SourcePos Char [Token]

type Parser = ParsecT Sources () Identity

maxNesting :: Int
maxNesting = 256

nestingMessage :: Text
nestingMessage = "MinML nesting exceeds " <> T.pack (show maxNesting) <> " levels"

checkNesting :: Int -> Either PandocError ()
checkNesting depth = when (depth > maxNesting) $ Left $ PandocParseError nestingMessage

parseMinML :: ToSources a => a -> Either PandocError [Node]
parseMinML input = do
  tokens <- readWith (tokenize True 0 Nothing <* eof) () input
  interpret True False False tokens

isSpaceChar :: Char -> Bool
isSpaceChar c = c `elem` (" \t\r\n" :: String)

isNameChar :: Char -> Bool
isNameChar c = not (isSpaceChar c || c `elem` ("()[]{}" :: String))

closing :: Char -> Char
closing '(' = ')'
closing '[' = ']'
closing _ = '}'

-- All repetitions consume input. Bound recursion independently of input size.
tokenize :: Bool -> Int -> Maybe Char -> Parser [Token]
tokenize directives depth end = do
  pos <- getPosition
  text <- takeWhileP (`notElem` ("()[]{}" :: String))
  next <- optionMaybe (lookAhead anyChar)
  let prefix = [Chunk pos text | not (T.null text)]
  case next of
    Nothing -> case end of
      Nothing -> return prefix
      Just c -> fail $ "expected " <> [c]
    Just c | Just c == end -> return prefix
    Just c | c `elem` ("([{" :: String) -> do
      when (depth >= maxNesting) $ fail (T.unpack nestingMessage)
      start <- getPosition
      void anyChar
      let name = snd (starter text)
          innerDirectives = directives && name `notElem` ["+", "-"] &&
                            (c /= '{' || T.null name)
      inside <- if directives && c == '[' && name `elem` ["!", "?"]
                   then do
                     raw <- directive (depth + 1)
                     return [Chunk start raw]
                   else tokenize innerDirectives (depth + 1) (Just (closing c))
      void $ char (closing c)
      rest <- tokenize directives depth end
      return $ prefix ++ Pair start c inside : rest
    Just c -> unexpected $ "unmatched " <> [c]

-- Declarations retain XML's syntax, including quoted system identifiers and
-- internal subsets. Parentheses and braces inside XML quotes are literal.
directive :: Int -> Parser Text
directive depth = do
  raw <- optionMaybe $ try (string "--") <|> try (string "[CDATA[")
  case raw of
    Just opener -> do
      let end = if opener == "--" then "--" else "]]"
      body <- manyTillChar anyChar (try $ string end <* lookAhead (char ']'))
      return $ T.pack opener <> body <> T.pack end
    Nothing -> T.concat <$> many piece
 where
  piece = takeWhile1P (`notElem` ("[]\"'" :: String))
      <|> do
        q <- oneOf "\"'"
        body <- manyTillChar anyChar (char q)
        return $ T.singleton q <> body <> T.singleton q
      <|> do
        when (depth >= maxNesting) $ fail (T.unpack nestingMessage)
        void $ char '['
        body <- directive (depth + 1)
        void $ char ']'
        return $ "[" <> body <> "]"

starter :: Text -> (Text, Text)
starter t =
  let prefix = T.dropWhileEnd isNameChar t
      name = T.takeWhileEnd isNameChar t
  in case T.uncons name of
    Just ('<', rest) -> (prefix <> "<", rest)
    _ -> (prefix, name)

-- Space controls apply at square/curly matcher boundaries, but never inside
-- raw text and comments. A consumed control cannot create a new reference.
trimControls :: Bool -> Bool -> Text -> Text
trimControls after before text =
  let (removed, t) = case T.uncons text of
        Just ('>', rest) | after, Just (c, _) <- T.uncons rest
                        , isSpaceChar c -> (True, T.dropWhile isSpaceChar rest)
        _ -> (False, text)
  in if before && removed && t == "<"
        then ""
        else case T.unsnoc t of
          Just (rest, '<') | before, Just (_, c) <- T.unsnoc rest
                          , isSpaceChar c -> T.dropWhileEnd isSpaceChar rest
          _ -> t

literal :: Text -> [Node]
literal t = [Literal t | not (T.null t)]

interpret :: Bool -> Bool -> Bool -> [Token] -> Either PandocError [Node]
interpret markup after before tokens = case tokens of
  [] -> return []
  Chunk _ text : Pair pos open body : rest
    | markup, open /= '(', let (prefix, name) = starter text
    , not (T.null name) -> do
        (node, remaining) <- element pos name open body rest
        tailNodes <- interpret markup True before remaining
        return $ literal (trimControls after True prefix) ++ node ++ tailNodes
  Chunk _ text : rest -> do
    let sensitive = case rest of
          Pair _ c _ : _ -> c /= '('
          _ -> before
    tailNodes <- interpret markup False before rest
    return $ literal (trimControls after sensitive text) ++ tailNodes
  Pair pos open body : rest -> do
    nodes <- case reference open body of
      Just ref -> literal <$> decodeReference pos ref
      Nothing -> do
        inner <- interpret markup (open /= '(') (open /= '(') body
        return $ Literal (T.singleton open) : inner ++
                 [Literal (T.singleton (closing open))]
    tailNodes <- interpret markup (open /= '(') before rest
    return $ nodes ++ tailNodes

element :: SourcePos -> Text -> Char -> [Token] -> [Token]
        -> Either PandocError ([Node], [Token])
element pos name open body rest
  | name `elem` ["+", "-", "!", "?"] = do
      unless (open == '[') $ syntaxError pos "expected '['"
      node <- case name of
        "+" -> return $ Literal (tokenText body)
        "-" -> Comment . T.concat . map literalText <$> interpret False False False body
        "!" -> return $ Declaration (tokenText body)
        _ -> return $ Instruction (tokenText body)
      return ([node], rest)
  | otherwise = do
      (attrs, contents, remaining) <- if open == '{'
        then do
          attrs <- attributes pos body
          case rest of
            Pair _ '[' contents : remaining -> return (attrs, contents, remaining)
            _ -> syntaxError pos "expected '[' after attributes"
        else return ([], body, rest)
      nodes <- interpret True True True contents
      return (case name of
                "\"" -> Literal "\x201c" : nodes ++ [Literal "\x201d"]
                "'" -> Literal "\x2018" : nodes ++ [Literal "\x2019"]
                _ -> [Element name attrs nodes], remaining)

attributes :: SourcePos -> [Token] -> Either PandocError [(Text, Text)]
attributes pos tokens = case tokens of
  [] -> return []
  Chunk p text : rest | T.null (T.dropWhile isSpaceChar text) -> attributes pos rest
                     | otherwise -> do
      let (name, value) = T.break (== '=') (T.dropWhile isSpaceChar text)
      unless (validAttributeName name && not (T.null value)) $
        syntaxError p "expected attribute name followed by '='"
      let input = [Chunk p (T.drop 1 value) | T.length value > 1] ++ rest
      (nodes, remaining) <- case input of
        Pair _ '[' body : following -> do
          unless (separated following) $
            syntaxError p "expected whitespace after attribute value"
          nodes <- interpret False True True body
          return (nodes, following)
        _ -> do
          let (body, following) = unquoted input
          nodes <- interpret False False False body
          return (nodes, following)
      more <- attributes pos remaining
      return $ (name, T.concat (map literalText nodes)) : more
  _ -> syntaxError pos "expected attribute name"
 where
  separated [] = True
  separated (Chunk _ t : _) = maybe False (isSpaceChar . fst) (T.uncons t)
  separated _ = False
  unquoted [] = ([], [])
  unquoted (Chunk p t : rest) =
    let (value, suffix) = T.break isSpaceChar t
        prefix = [Chunk p value | not (T.null value)]
    in if T.null suffix
          then first (prefix ++) (unquoted rest)
          else (prefix, Chunk p suffix : rest)
  unquoted (t : ts) = first (t :) (unquoted ts)

-- Names are liberal so that every HTML element and attribute name is
-- representable. The reference implementation requires XML attribute names.
validElementName :: Text -> Bool
validElementName t = not (T.null t) && T.all isNameChar t
                     && t `notElem` ["+", "-", "!", "?", "\"", "'"]

validAttributeName :: Text -> Bool
validAttributeName t = not (T.null t) && T.all (\c -> isNameChar c && c /= '=') t

literalText :: Node -> Text
literalText (Literal t) = t
literalText _ = ""

reference :: Char -> [Token] -> Maybe Text
reference '[' [Chunk _ t]
  | not (T.null t), not (T.any isSpaceChar t), t /= "<", t /= ">" = Just t
reference '[' [Pair _ c [Chunk _ t]]
  | t == "<" || t == ">" = Just $ T.singleton c <> t <> T.singleton (closing c)
reference _ _ = Nothing

decodeReference :: SourcePos -> Text -> Either PandocError Text
decodeReference pos ref
  | Just digits <- T.stripPrefix "#" ref = do
      let (hexadecimal, number) = case T.uncons digits of
            Just (x, rest) | x == 'x' || x == 'X' -> (True, rest)
            _ -> (False, digits)
          significant = T.dropWhile (== '0') number
          validDigit = if hexadecimal then isHexDigit else \c -> c >= '0' && c <= '9'
      unless (not (T.null number) && T.all validDigit number) $
        syntaxError pos "invalid numeric character reference"
      when (T.length significant > if hexadecimal then 6 else 7) $
        syntaxError pos "numeric character reference is too large"
      let value
            | T.null significant = Just 0
            | hexadecimal = case readHex (T.unpack significant) of
                [(n, "")] -> Just n
                _ -> Nothing
            | otherwise = readMaybe (T.unpack significant)
      case value :: Maybe Integer of
        Just n | n > 0, n <= 0x10ffff, n < 0xd800 || n > 0xdfff ->
          return $ T.singleton (chr (fromInteger n))
        _ -> syntaxError pos "invalid numeric character reference"
  | Just t <- lookupEntity (ref <> ";") = return t
  | Just t <- M.lookup ref symbolicReferences = return t
  | otherwise = return $ "&" <> ref <> ";"

symbolicReferences :: M.Map Text Text
symbolicReferences = M.fromList
  [ ("(<)", "("), ("(>)", ")"), ("[<]", "["), ("[>]", "]")
  , ("{<}", "{"), ("{>}", "}"), ("--", "–"), ("---", "—")
  , ("+-", "±"), ("-+", "∓"), ("x", "×"), ("d", "÷")
  , (".", "⋅"), (":", "∶"), ("::", "∷"), ("2rt", "√")
  , ("3rt", "∛"), ("4rt", "∜"), ("<=", "≤"), (">=", "≥")
  , ("<>", "≶"), ("><", "≷"), ("<<", "≪"), (">>", "≫")
  , ("<<<", "⋘"), (">>>", "⋙"), ("~~", "≈"), ("~~=", "≊")
  , ("def=", "≝"), ("/=", "≠"), ("/<", "≮"), ("/>", "≯")
  , ("/<=", "≰"), ("/>=", "≱"), ("/<>", "≸"), ("/><", "≹")
  , ("/~~", "≉"), ("<--", "←"), ("-->", "→"), ("<->", "↔")
  , ("<==", "⇐"), ("==>", "⇒"), ("<=>", "⇔"), ("<---", "⟵")
  , ("--->", "⟶"), ("<-->", "↔"), ("<===", "⟸"), ("===>", "⟹")
  , ("<==>", "⟺"), ("/<--", "↚"), ("/-->", "↛"), ("/<->", "↮")
  , ("/<==", "⇍"), ("/==>", "⇏"), ("/<=>", "⇎"), ("|--", "⊢")
  , ("--|", "⊣"), ("~|~", "⊤"), ("_|_", "⊥"), ("|-", "⊦")
  , ("|=", "⊧"), ("|==", "⊨"), ("||-", "⊩"), ("||=", "⊫")
  , ("/|--", "⊬"), ("/|==", "⊭"), ("/||-", "⊮"), ("/||=", "⊯")
  , ("-.", "¬"), ("^", "∧"), ("v", "∨"), ("v-", "⊻")
  , ("-^", "⊼"), ("-v", "⊽"), ("1/4", "¼"), ("1/2", "½")
  , ("3/4", "¾"), ("1/7", "⅐"), ("1/9", "⅑"), ("1/10", "⅒")
  , ("1/3", "⅓"), ("2/3", "⅔"), ("1/5", "⅕"), ("2/5", "⅖")
  , ("3/5", "⅗"), ("4/5", "⅘"), ("1/6", "⅙"), ("5/6", "⅚")
  , ("1/8", "⅛"), ("3/8", "⅜"), ("5/8", "⅝"), ("7/8", "⅞")
  ]

syntaxError :: SourcePos -> Text -> Either PandocError a
syntaxError pos message = Left $ PandocParseError $
  T.pack (show pos) <> ": " <> message

tokenText :: [Token] -> Text
tokenText = TL.toStrict . B.toLazyText . foldMap go
 where
  go (Chunk _ t) = B.fromText t
  go (Pair _ c ts) = B.singleton c <> foldMap go ts <> B.singleton (closing c)

toHtmlTags :: [Node] -> [Tag Text]
toHtmlTags = foldr go []
 where
  go (Element name attrs body) rest =
    let end = if T.toLower (T.takeWhileEnd (/= ':') name) `Set.member` voidTags
                 then rest
                 else TagClose name : rest
    in TagOpen name attrs : foldr go end body
  go (Literal t) rest = TagText t : rest
  go (Comment t) rest = TagComment t : rest
  go (Declaration t) rest = parseTags ("<!" <> t <> ">") ++ rest
  go (Instruction t) rest = parseTags ("<?" <> t <> "?>") ++ rest

renderXML :: [Node] -> Text
renderXML = TL.toStrict . B.toLazyText . foldMap go
 where
  esc = B.fromText . escapeStringForXML
  go (Element name attrs body) =
    "<" <> B.fromText name <> foldMap attr attrs <> ">" <>
    foldMap go body <> "</" <> B.fromText name <> ">"
  go (Literal t) = esc t
  go (Comment t) = "<!--" <> B.fromText t <> "-->"
  go (Declaration t) = "<!" <> B.fromText t <> ">"
  go (Instruction t) = "<?" <> B.fromText t <> "?>"
  attr (name, value) = " " <> B.fromText name <> "=\"" <> esc value <> "\""

-- Escape matchers and whitespace-control characters, and non-ASCII characters
-- when requested. Keep track of the last output character so a reference
-- cannot attach to preceding text as a tag.
escapeMinML :: Bool -> Int -> Bool -> Text -> Either PandocError (Bool, B.Builder)
escapeMinML ascii depth initial text = do
  when (depth >= maxNesting && T.any needsEscape text) $ checkNesting (depth + 1)
  return $ T.foldl' step (initial, mempty) text
 where
  needsEscape c = c `elem` ("()[]{}<>" :: String) || ascii && c > '\x7f'
  step (previousName, out) c
    | needsEscape c =
        (False, out <> (if previousName then " <" else mempty) <>
         "[#" <> B.fromString (show (ord c)) <> "]")
    | otherwise = (isNameChar c, out <> B.singleton c)

-----------------------------------------------------------------------------
-- |
-- Module      :  Haddock.Parser.Parser
-- Description :  Parses the list of tokens produced by "Haddock.Parser.Lexer"
-- Maintainer  :  haddock@projects.haskell.org
-- Stability   :  experimental
-----------------------------------------------------------------------------
module Haddock.Parser.Parser (parseString, parseParas) where

import Control.Applicative
import Control.Monad ((>=>), liftM2, ap)
import Data.Attoparsec.ByteString.Char8 hiding (take, string)
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import DynFlags
import Haddock.Doc (docAppend, docParagraph)
import Haddock.Parser.Lexer
import Haddock.Types (Doc(..), Hyperlink(..), Example(..), Picture(..))
import RdrName

data MTStack s r = MTStack { runMTS :: s -> Maybe (r, s) }

type MTS = MTStack [Token] (Doc RdrName)

instance Monad (MTStack t) where
  return x = MTStack (\s -> Just (x, s))
  MTStack h >>= f = MTStack $ h >=> \(a, ns) -> let MTStack g = f a
                                                in g ns

instance Applicative (MTStack t) where
  pure = return
  (<*>) = ap

instance Functor (MTStack t) where
  fmap f g = pure f <*> g

instance Alternative (MTStack t) where
  empty = MTStack $ const Nothing
  MTStack f <|> MTStack g = MTStack $ liftM2 (`maybe` Just) g f

-- | In general, this is the parser you want to start with.
-- 'parseParas' starts here.
doc :: MTS
doc = appop
       <|> tp TokPara *> doc
       <|> paragraph
       <|> empty
  where appop = do
          d <- paragraph
          t <- pop
          case t of
            TokPara -> docAppend d <$> doc
            _ -> empty

-- | Parses a paragraph, giving priority to 'listParagraph'
-- over 'elementParagraph'
paragraph :: MTS
paragraph = foldr1 docAppend <$> some listParagraph <|> elementParagraph

-- | Parses lists.
listParagraph :: MTS
listParagraph = DocUnorderedList . return <$> unorderedList
                <|> DocOrderedList . return <$> orderedList
                <|> DocDefList . return <$> definitionList

-- | Parses unordered (bullet) lists
unorderedList :: MTS
unorderedList = tp TokBullet *> elementParagraph

-- | Parses ordered lists (numbered or dashed)
orderedList :: MTS
orderedList = tp TokNumber *> elementParagraph

-- | Parses definition lists.
definitionList :: MTStack [Token] (Doc RdrName, Doc RdrName)
definitionList = (,) <$> (tp TokDefStart *> consecutiveElements) <*> (tp TokDefEnd *> consecutiveElements)

-- | Parses a paragraph of elements and if that fails, it tries
-- 'birdtrackBlock', 'property' and 'examples'.
elementParagraph :: MTS
elementParagraph = docParagraph <$> consecutiveElements
       <|> DocCodeBlock <$> birdtrackBlock
       <|> property
       <|> examples

-- | Parses (possibly consecutive) birdtracks.
birdtrackBlock :: MTS
birdtrackBlock = do
  t <- pop
  case t of
    TokBirdTrack bt -> do
      r <- many birdtrackBlock
      case r of
        [] -> return $ DocString bt
        _ -> return $ docAppend (DocString bt) (foldr1 docAppend r)
    _ -> empty

-- | Parses properties and runs 'makeProperty' on their content.
property :: MTS
property = do
  t <- pop
  case t of
    TokProperty x -> return $ makeProperty x
    _ -> empty

-- | Parses (possibly consecutive) 'example's
examples :: MTS
examples = DocExamples <$> some example

-- | Parses an example, where an example consists of a prompt, an expression
-- and possibly some results on following lines, terminated by an empty line.
-- Uses 'makeExample'.
example :: MTStack [Token] Example
example = do
  p <- pop
  e <- pop
  case p of
    TokExamplePrompt p' -> case e of
      TokExampleExpression e' -> makeExample p' e' . lines . concat <$> many result
      _ -> empty
    _ -> empty

-- | Results used in 'example's.
result :: MTStack [Token] String
result = do
  r <- pop
  case r of
    TokExampleResult r' -> return r'
    _ -> empty

-- | Parser a consecutive 'elements'. 'parseString' starts here.
consecutiveElements :: MTS
consecutiveElements = foldr1 docAppend <$> some elements

-- | Parses an 'elementSequence', possibly monospaced.
elements :: MTS
elements = singleElement
         <|> tp (TokSpecial '@') *> (DocMonospaced <$> elementSequence) <* tp (TokSpecial '@')

-- | Consumes a matching token from the stack
tp :: Token -> MTS
tp tok = do
  t <- pop
  if t == tok
    then return DocEmpty
    else empty

-- | Parses multiple elements with 'singleElement'.
elementSequence :: MTS
elementSequence = leadPara
         <|> melems
         <|> singleElement
  where
    leadPara = docAppend (DocString "\n") <$> tp TokPara *> elementSequence
    melems = docAppend <$> singleElement <*> elementSequence

-- | Takes off the first 'Token' from a stack.
-- Fails with 'empty' if the stack is empty.
pop :: MTStack [Token] Token
pop = MTStack pper
  where pper [] = empty
        pper (x:xs) = Just (x, xs)

-- | Parses the smallest elements that are of interest to us such as
-- URLs, pictures, anchors, emphasis &c
singleElement :: MTS
singleElement = do
  t <- pop
  case t of
    TokString str -> return $ DocString str
    TokEmphasis str -> return $ DocEmphasis (DocString str)
    TokURL url -> return $ DocHyperlink (makeHyperlink url)
    TokPic pic -> return $ DocPic (makePicture pic)
    TokAName an -> return $ DocAName an
    TokIdent it -> return $ DocIdentifier it
    TokSpecial '\"' -> do
      s <- strings <* tp (TokSpecial '\"')
      return $ DocModule s
    _ -> empty

-- | Concatenates consecutive strings on a stack into one.
strings :: MTStack [Token] String
strings = concat <$> some strings'
  where
    strings' :: MTStack [Token] String
    strings' = do
      t <- pop
      case t of
        TokString s -> return s
        _ -> empty

-- | Makes a property that can be used by other programs for assertions.
-- Drops whitespace around the property.
makeProperty :: String -> Doc RdrName
makeProperty s = case strip s of
  'p':'r':'o':'p':'>':xs ->
    DocProperty (dropWhile isSpace xs)
  xs ->
    error $ "makeProperty: invalid input " ++ show xs


-- | Creates a 'Hyperlink', deciding on whether the label exists or not.
makeHyperlink :: String -> Hyperlink
makeHyperlink input = case break isSpace $ strip input of
  (url, "")    -> Hyperlink url Nothing
  (url, label) -> Hyperlink url (Just . dropWhile isSpace $ label)

makePicture :: String -> Picture
makePicture input = case break isSpace $ strip input of
  (uri, "")    -> Picture uri Nothing
  (uri, label) -> Picture uri (Just . dropWhile isSpace $ label)

-- | Create an 'Example', stripping superfluous characters as appropriate.
-- Remembers the amount of indentation used for the prompt.
makeExample :: String -> String -> [String] -> Example
makeExample prompt expression res =
  Example (strip expression) result'       -- drop whitespace in expressions
  where (prefix, _) = span isSpace prompt
        result' = map (substituteBlankLine . tryStripPrefix prefix) res
          where tryStripPrefix xs ys = fromMaybe ys $ stripPrefix xs ys
                substituteBlankLine "<BLANKLINE>" = ""
                substituteBlankLine line          = line

-- | Remove all leading and trailing whitespace
strip :: String -> String
strip = (\f -> f . f) $ dropWhile isSpace . reverse

-- | Parses a string of text, stringi at 'consecutiveElements'
parseString :: DynFlags -> String -> Maybe (Doc RdrName)
parseString dflags str = case dropParas $ runMTS consecutiveElements $ lexL dflags str of
    Just (r, []) -> Just r
    _ -> Nothing

-- | Parses a paragraph of text, starting at 'doc'
parseParas :: DynFlags -> String -> Maybe (Doc RdrName)
parseParas dflags str = case dropParas $ runMTS doc $ lexL dflags str of
  Just (r, []) -> Just r
  _ -> Nothing

-- | Drops superflous whitespace from the final result
dropParas :: Maybe (Doc RdrName, [Token]) -> Maybe (Doc RdrName, [Token])
dropParas s@(Just (_, [])) = s
dropParas (Just (d, TokPara : xs)) = dropParas $ Just (d, xs)
dropParas xs = xs

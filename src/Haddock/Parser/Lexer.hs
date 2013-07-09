{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Haddock.Parser.Lexer
-- Description :  Lexes an input string to '[Token]' for "Haddock.Parser.Parser"
-- Maintainer  :  haddock@projects.haskell.org
-- Stability   :  experimental
-----------------------------------------------------------------------------
module Haddock.Parser.Lexer (lexL, Token(..)) where

import           Data.Monoid
import           Control.Applicative
import           Data.Attoparsec.ByteString hiding (takeWhile1, take, inClass)
import qualified Data.Attoparsec.ByteString.Char8 as A8
import           Data.Attoparsec.ByteString.Char8 hiding (take, string)
import           Data.Bits        ((.|.),(.&.),shiftL,shiftR)
import qualified Data.ByteString as BS
import           Data.Char        (chr,ord)
import           Data.List (intercalate)
import           Data.Word        (Word8)
import           DynFlags
import           FastString (mkFastString)
import           Lexer (mkPState, unP, ParseResult(POk))
import           Numeric
import           Parser (parseIdentifier)
import           RdrName
import           SrcLoc (mkRealSrcLoc, unLoc)
import           StringBuffer (stringToStringBuffer)

default(Int)

-- | Represents parts of the input string, without any real structure.
data Token
  = TokPara
  | TokNumber
  | TokBullet
  | TokDefStart
  | TokDefEnd
  | TokSpecial Char
  | TokIdent RdrName
  | TokString String
  | TokURL String
  | TokPic String
  | TokEmphasis String
  | TokAName String
  | TokBirdTrack String
  | TokProperty String
  | TokExamplePrompt String
  | TokExampleExpression String
  | TokExampleResult String
    deriving Eq


-- | We use this to consume any whitespace in front of more interesting strings
optWs :: Parser BS.ByteString
optWs = A8.takeWhile (`elem` " \t\f\v\r")

-- | Takes all characters until and including a newline character.
untilEofInc :: Parser String
untilEofInc = (++ "\n") . decodeB <$> A8.takeWhile (/= '\n') <* string "\n"

-- | Helper that takes all characters until any of the specified ones.
allUntilAnyOf :: String -> Parser String
allUntilAnyOf cs = decodeB <$> A8.takeWhile (not . (`elem` cs))

-- | Lexes a string into '[Token]' to ease up parsing later. Entry point.
lexL :: DynFlags -- ^ Passed in by GHC at runtime for identifier resolution.
        -> String -> [Token]
lexL dflags str = case parseOnly (paraL dflags) (encodeB $ str ++ "\n") of
  Right r@(_:_) -> foldl1 f $ map return r
  _ -> []
  where f xs [TokString s] = case last xs of
          TokString str' -> init xs ++ [TokString $ str' ++ s]
          _ -> xs ++ [TokString s]
        f xs y = xs ++ y

type DDocP = DynFlags -> Parser [Token]

-- | Main lexer we find ourselves in at the very beginning of the lexing.
-- | 'lexL' starts here.
paraL :: DDocP
paraL dflags =  many' (optWs *> char '\n') *> endOfInput *> return []
                <|> optWs *> char '\n' *> paraL dflags
                <|> (TokExamplePrompt <$> promptP ~:> exampleexprL dflags)
                <|> optWs *> char '>' *> birdtrackL dflags
                <|> (TokProperty <$> liftA2 (++) ((++) . decodeB <$> optWs <*> (decodeB <$> string "prop>"))
                     untilEofInc ~:> propertyL dflags)
                <|> listL dflags
                <|> optWs *> stringdefL dflags

-- | Lexes lists, even if they are directly following each other.
listL :: DDocP
listL dflags =  (optWs *> (char '*' <|> char '-') *>
                     return TokBullet ~:> listStringdefL dflags)
                <|> ((optWs *> char '[' *> return TokDefStart) ~:> listDefstringL dflags)
                <|> (numberL ~:> listStringdefL dflags)

-- | Lexes a prompt.
promptP :: Parser String
promptP = do
  w <- optWs
  _ <- string ">>>"
  return $ decodeB w ++ ">>>"

-- | Lexes a beginning of ordered lists.
numberL :: Parser Token
numberL = optWs *> (const TokNumber <$> (paren <|> dot))
  where dot = decimal <* char '.'
        paren :: Parser Integer
        paren = char '(' *> decimal <* char ')'

-- | Lexes a closing bracket when we aren't in the middle of a definition list.
stringL :: DDocP
stringL dflags = char ']' *> return (TokString "]") ~:> listStringdefL dflags

-- | Lexes a closing bracket when we're inside of a definition list.
defL :: DDocP
defL dflags = char ']' *> return TokDefEnd ~:> listStringdefL dflags

-- | A template for 'stringdefL' and 'defstringL'. It lexes the bulk of
-- the smallest elments we care about such as URLs, pictures and anchors.
-- We use this to solve an issue of a lexer trying the same function when
-- in more than one state at once.
--
-- In hindsight, this is a horrible way to do it.
stringdefTempl :: DDocP -> DDocP -> DDocP -> DDocP
stringdefTempl self trail lineFollow dflags =
  ((TokSpecial <$> (char '"' <|> char '@')) ~:> self dflags)
   <|> ((string "<<" *> (TokPic <$> allUntilAnyOf ">\n") <* string ">>") ~:> self dflags)
   <|> (++) <$> ((char '<' *> (return <$> TokURL <$> allUntilAnyOf ">\n") <* char '>') <|> autoUrl)
                 <*> (self dflags <|> return [])
   <|> ((char '#' *> (TokAName <$> allUntilAnyOf "#\n") <* char '#') ~:> self dflags)
   <|> ((char '/' *> (TokEmphasis <$> allUntilAnyOf "/\n") <* char '/') ~:> self dflags)
   <|> (identL dflags ~:> self dflags)
   <|> (char '\\' *> (TokString . return <$> A8.satisfy (/= '\n')) ~:> self dflags)
   <|> (string "&#" *> ((\ s -> TokString [chr s])
                        <$> decimal <* char ';') ~:> self dflags)
   <|> (string "&#" *> (char 'x' <|> char 'X') *>
        (rh <$> many1 hexdigit <* char ';') ~:> self dflags)
   <|> ((TokString . return <$> A8.satisfy (`elem` "/'`<#&\\")) ~:> self dflags)
   <|> ((TokString <$> liftA2 (++) (allUntilAnyOf "\"@/<#\n'`&]\\") (return <$> char '\n')) ~:> lineFollow dflags)
   <|> ((TokString <$> (allUntilAnyOf "\"@/<#\n'`&]\\" >>= gotSome)) ~:> self dflags)
   <|> trail dflags
  where rh s = case readHex s of -- just to silence the compiler
          [(n, _)] -> TokString [chr n]
          _ -> error $ "impossible happened while reading hex: " ++ s
        hexdigit = A8.satisfy (`elem` "0123456789ABCDEF")
        gotSome "" = fail "empty"
        gotSome x = return x

autoUrl :: Parser [Token]
autoUrl = do
  u <- decodeB <$> url
  case last u of
    '.' -> return [TokURL $ init u, TokString "."]
    _ -> return [TokURL u]
  where
    url = mappend <$> choice ["http://", "https://", "ftp://", "ssh://", "gopher://"]
          <*> takeWhile1 (not . isSpace)

stringdefL, defstringL, listStringdefL, listDefstringL :: DDocP
propertyL, birdtrackL :: DDocP
exampleexprL, exampleresultL, lineL, exampleL :: DDocP

-- | Attempts to lex a string with 'stringdefTempl', voming to 'stringL'
-- if the template fails.
stringdefL = stringdefTempl stringdefL stringL lineL

-- | Attempts to lex a string with 'stringdefTempl', moving to 'defL'
-- if the template fails.
defstringL = stringdefTempl defstringL defL lineL

-- | Like 'defstringL' but allows accept a list to follow straight away.
listDefstringL = stringdefTempl defstringL defL (\x -> listL x <|> lineL x)

-- | Like 'stringdefL' but allows accept a list to follow straight away.
listStringdefL = stringdefTempl stringdefL stringL (\x -> listL x <|> lineL x)

-- | Adds a 'TokPara' 'Token' to the lexer stack and moves to 'paraL'
propertyL dflags = return TokPara ~:> paraL dflags

-- | Helper that consumes all characters until (and including) end of the line.
tokdL :: (String -> Token) -- ^ Function to create the token from the characters.
         -> DDocP -- ^ Lexer to move onto afterwards
         -> DynFlags -- ^ 'DynFlags' to pass into the next lexer.
         -> Parser [Token]
tokdL t c d = t <$> untilEofInc ~:> c d

-- | Creates a 'TokBirdTrack' token from all characters until end of the line.
birdtrackL = tokdL TokBirdTrack lineL

-- | Creates a 'TokExampleExpression' token from all characters until end of the line.
exampleexprL = tokdL TokExampleExpression exampleL

-- | Creates a 'TokExampleResult' token from all characters until end of the line.
exampleresultL = tokdL TokExampleResult exampleL

-- | Lexer used after an empty line, often indicating a code block &c
lineL dflags = (TokExamplePrompt <$> promptP ~:> exampleexprL dflags)
               <|> optWs *> char '>' *> birdtrackL dflags
               <|> ((optWs *> char '\n' *> return TokPara) ~:> paraL dflags)
               <|> stringdefL dflags



-- | Lexer for for examples, including the prompt and results.
exampleL dflags = optWs *> (char '\n' *> return TokPara ~:> paraL dflags)
                  <|> (TokExamplePrompt <$> promptP ~:> exampleexprL dflags)
                  <|> exampleresultL dflags

infix 3 ~:>
-- | A combinator that combines lexer results until failure. This
-- effectively acts as an accumulator with state transition. A better
-- data structure will get rid of this throughout the program.
(~:>) :: Parser Token -> Parser [Token] -> Parser [Token]
x ~:> y = (:) <$> x <*> (y <|> return [])

-- | Lexes strings between identifier delimiters. Consumes all input that it
-- deems to be valid in an identifier. Note that it simply blindly consumes
-- characters and does no actual validation itself.
parseValid :: Parser String
parseValid = do
  vs <- many' (A8.satisfy (`elem` "_.!#$%&*+/<=>?@\\?|-~:") <|> digit <|> letter_ascii)
  c <- peekChar
  case c of
    Just '`' -> return vs
    Just '\'' -> (do {c'' <- char '\''; y'' <- parseValid; return $ vs ++ [c''] ++ y''}) <|> return vs
    _ -> fail "outofvalid"

-- | Lexes identifiers with help of 'parseValid'. Asks GHC for 'RdrName' from the
-- string it lexes. In case of failure, we get a simple quoted string back in
-- form of a 'TokString' as opposed to a 'TokIdent'.
identL :: DynFlags -> Parser Token
identL dflags = do
  o <- idDelim
  vid <- parseValid
  e <- idDelim
  return $ validIdentifier $ o : (vid ++ [e])
  where idDelim = char '\'' <|> char '`'
        validIdentifier str = case parseIdent (tail $ init str) of
          Just identName -> TokIdent identName
          Nothing -> TokString str
        parseIdent :: String -> Maybe RdrName
        parseIdent str0 =
          let buffer = stringToStringBuffer str0
              realSrcLc = mkRealSrcLoc (mkFastString "<unknown file>") 0 0
              pstate = mkPState dflags buffer realSrcLc
          in case unP parseIdentifier pstate of
            POk _ name -> Just (unLoc name)
            _ -> Nothing

-- | Helper that encodes and packs a 'String' into a 'BS.ByteString'
encodeB :: String -> BS.ByteString
encodeB = BS.pack . encode

-- | Helper that unpacks and decodes a 'BS.ByteString' into a 'String'
decodeB :: BS.ByteString -> String
decodeB = decode . BS.unpack

-- Copy/pasted functions from Codec.Binary.UTF8.String for encoding/decoding
-- | Character to use when 'encode' or 'decode' fail for a byte.
replacementCharacter :: Char
replacementCharacter = '\xfffd'

-- | Encode a Haskell String to a list of Word8 values, in UTF8 format.
encode :: String -> [Word8]
encode = concatMap (map fromIntegral . go . ord)
 where
  go oc
   | oc <= 0x7f       = [oc]

   | oc <= 0x7ff      = [ 0xc0 + (oc `shiftR` 6)
                        , 0x80 + oc .&. 0x3f
                        ]

   | oc <= 0xffff     = [ 0xe0 + (oc `shiftR` 12)
                        , 0x80 + ((oc `shiftR` 6) .&. 0x3f)
                        , 0x80 + oc .&. 0x3f
                        ]
   | otherwise        = [ 0xf0 + (oc `shiftR` 18)
                        , 0x80 + ((oc `shiftR` 12) .&. 0x3f)
                        , 0x80 + ((oc `shiftR` 6) .&. 0x3f)
                        , 0x80 + oc .&. 0x3f
                        ]

--
-- | Decode a UTF8 string packed into a list of Word8 values, directly to String
--
decode :: [Word8] -> String
decode [    ] = ""
decode (c:cs)
  | c < 0x80  = chr (fromEnum c) : decode cs
  | c < 0xc0  = replacementCharacter : decode cs
  | c < 0xe0  = multi1
  | c < 0xf0  = multi_byte 2 0xf  0x800
  | c < 0xf8  = multi_byte 3 0x7  0x10000
  | c < 0xfc  = multi_byte 4 0x3  0x200000
  | c < 0xfe  = multi_byte 5 0x1  0x4000000
  | otherwise = replacementCharacter : decode cs
  where
    multi1 = case cs of
      c1 : ds | c1 .&. 0xc0 == 0x80 ->
        let d = ((fromEnum c .&. 0x1f) `shiftL` 6) .|.  fromEnum (c1 .&. 0x3f)
        in if d >= 0x000080 then toEnum d : decode ds
                            else replacementCharacter : decode ds
      _ -> replacementCharacter : decode cs

    multi_byte :: Int -> Word8 -> Int -> String
    multi_byte i mask overlong = aux i cs (fromEnum (c .&. mask))
      where
        aux 0 rs acc
          | overlong <= acc && acc <= 0x10ffff &&
            (acc < 0xd800 || 0xdfff < acc)     &&
            (acc < 0xfffe || 0xffff < acc)      = chr acc : decode rs
          | otherwise = replacementCharacter : decode rs

        aux n (r:rs) acc
          | r .&. 0xc0 == 0x80 = aux (n-1) rs
                               $ shiftL acc 6 .|. fromEnum (r .&. 0x3f)

        aux _ rs     _ = replacementCharacter : decode rs

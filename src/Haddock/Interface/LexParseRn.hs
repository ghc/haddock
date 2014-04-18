{-# OPTIONS_GHC -Wwarn #-}
{-# LANGUAGE BangPatterns #-}
  -----------------------------------------------------------------------------
-- |
-- Module      :  Haddock.Interface.LexParseRn
-- Copyright   :  (c) Isaac Dupree 2009,
--                    Mateusz Kowalczyk 2013
-- License     :  BSD-like
--
-- Maintainer  :  haddock@projects.haskell.org
-- Stability   :  experimental
-- Portability :  portable
-----------------------------------------------------------------------------
module Haddock.Interface.LexParseRn
  ( processDocString
  , processDocStringParas
  , processDocStrings
  , processModuleHeader
  ) where

import qualified Data.IntSet as IS
import Haddock.Types
import Haddock.Parser
import Haddock.Interface.ParseModuleHeader
import Haddock.Doc

import Control.Applicative
import Data.List
import Data.Maybe
import FastString
import GHC
import DynFlags (ExtensionFlag(..), languageExtensions)
import Name
import Outputable
import RdrName

processDocStrings :: DynFlags -> GlobalRdrEnv -> [HsDocString] -> ErrMsgM (Maybe (Doc Name))
processDocStrings dflags gre strs = do
  docs <- catMaybes <$> mapM (processDocStringParas dflags gre) strs
  let doc = foldl' docAppend DocEmpty docs
  case doc of
    DocEmpty -> return Nothing
    _ -> return (Just doc)


processDocStringParas :: DynFlags -> GlobalRdrEnv -> HsDocString -> ErrMsgM (Maybe (Doc Name))
processDocStringParas = process parseParasMaybe


processDocString :: DynFlags -> GlobalRdrEnv -> HsDocString -> ErrMsgM (Maybe (Doc Name))
processDocString = process parseStringMaybe

process :: (DynFlags -> String -> Maybe (Doc RdrName))
        -> DynFlags
        -> GlobalRdrEnv
        -> HsDocString
        -> ErrMsgM (Maybe (Doc Name))
process parse dflags gre (HsDocString fs) = do
   let str = unpackFS fs
   case parse dflags str of
     Nothing -> do
       tell [ "doc comment parse failed: " ++ str ]
       return Nothing
     Just doc -> do
       return (Just (rename dflags gre doc))


processModuleHeader :: DynFlags -> GlobalRdrEnv -> SafeHaskellMode -> Maybe LHsDocString
                    -> ErrMsgM (HaddockModInfo Name, Maybe (Doc Name))
processModuleHeader dflags gre safety mayStr = do
  (hmi, doc) <-
    case mayStr of

      Nothing -> return failure
      Just (L _ (HsDocString fs)) -> do
        let str = unpackFS fs
        case parseModuleHeader dflags str of
          Left msg -> do
            tell ["haddock module header parse failed: " ++ msg]
            return failure
          Right (hmi, doc) -> do
            let !descr = rename dflags gre <$> hmi_description hmi
                hmi' = hmi { hmi_description = descr }
                doc' = rename dflags gre doc
            return (hmi', Just doc')

  let flags :: [ExtensionFlag]
      -- We remove the flags implied by the language setting and we display the language instead
      flags = map toEnum (IS.toList $ extensionFlags dflags) \\ languageExtensions (language dflags)
  return (hmi { hmi_safety = Just $ showPpr dflags safety
              , hmi_language = language dflags
              , hmi_extensions = flags
              } , doc)
  where
    failure = (emptyHaddockModInfo, Nothing)


rename :: DynFlags -> GlobalRdrEnv -> Doc RdrName -> Doc Name
rename dflags env = rn
  where
    rn d = case d of
      DocAppend a b -> DocAppend (rn a) (rn b)
      DocParagraph doc -> DocParagraph (rn doc)
      DocIdentifier x -> do
        let choices = dataTcOccs' x
        let gres = concatMap (\c -> lookupGRE_RdrName c env) choices
        case gres of
          [] ->
            case choices of
              [] -> DocMonospaced (DocString (showPpr dflags x))
              [a] -> outOfScope a
              a:b:_ | isRdrTc a -> outOfScope a
                    | otherwise -> outOfScope b
          [a] -> greIdentifier a
          a:b:_ | isTyConName (gre_name a) -> greIdentifier a
                | otherwise                -> greIdentifier b
              -- If an id can refer to multiple things, we give precedence to type
              -- constructors.

      DocWarning doc -> DocWarning (rn doc)
      DocEmphasis doc -> DocEmphasis (rn doc)
      DocBold doc -> DocBold (rn doc)
      DocMonospaced doc -> DocMonospaced (rn doc)
      DocUnorderedList docs -> DocUnorderedList (map rn docs)
      DocOrderedList docs -> DocOrderedList (map rn docs)
      DocDefList list -> DocDefList [ (rn a, rn b) | (a, b) <- list ]
      DocCodeBlock doc -> DocCodeBlock (rn doc)
      DocIdentifierUnchecked x -> DocIdentifierUnchecked x
      DocModule str -> DocModule str
      DocHyperlink l -> DocHyperlink l
      DocPic str -> DocPic str
      DocAName str -> DocAName str
      DocProperty p -> DocProperty p
      DocExamples e -> DocExamples e
      DocEmpty -> DocEmpty
      DocString str -> DocString str
      DocHeader (Header l t) -> DocHeader $ Header l (rn t)

    -- Render fields compiled with -XOverloadedRecordFields in
    -- monospace, because the name is an internal representation
    -- rather than the field label.
    greIdentifier gre = case gre_par gre of
        FldParent { par_lbl = Just lbl } -> monospaced lbl
        _                                -> DocIdentifier (gre_name gre)

    outOfScope :: RdrName -> Doc a
    outOfScope x =
      case x of
        Unqual occ -> monospaced occ
        Qual mdl occ -> DocIdentifierUnchecked (mdl, occ)
        Orig _ occ -> monospaced occ
        Exact name -> monospaced name  -- Shouldn't happen since x is out of scope

    monospaced a = DocMonospaced (DocString (showPpr dflags a))

dataTcOccs' :: RdrName -> [RdrName]
-- If the input is a data constructor, return both it and a type
-- constructor.  This is useful when we aren't sure which we are
-- looking at.
--
-- We use this definition instead of the GHC's to provide proper linking to
-- functions accross modules. See ticket #253 on Haddock Trac.
dataTcOccs' rdr_name
  | isDataOcc occ             = [rdr_name, rdr_name_tc]
  | otherwise                 = [rdr_name]
  where
    occ = rdrNameOcc rdr_name
    rdr_name_tc = setRdrNameSpace rdr_name tcName

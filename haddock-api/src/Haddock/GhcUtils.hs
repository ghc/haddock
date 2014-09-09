{-# LANGUAGE FlexibleInstances, ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_HADDOCK hide #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Haddock.GhcUtils
-- Copyright   :  (c) David Waern 2006-2009
-- License     :  BSD-like
--
-- Maintainer  :  haddock@projects.haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Utils for dealing with types from the GHC API
-----------------------------------------------------------------------------
module Haddock.GhcUtils where


import Data.Version
import Control.Applicative  ( (<$>) )
import Control.Arrow
import Data.Foldable hiding (concatMap)
import Data.Function
import Data.Traversable

import Exception
import Outputable
import Name
import Lexeme
import Packages
import Module
import RdrName (GlobalRdrEnv)
import GhcMonad (withSession)
import HscTypes
import UniqFM
import GHC
import Class


moduleString :: Module -> String
moduleString = moduleNameString . moduleName


-- return the (name,version) of the package
modulePackageInfo :: DynFlags -> Module -> (PackageName, Version)
modulePackageInfo dflags modu =
    (packageName pkg, packageVersion pkg)
  where
    pkg = getPackageDetails dflags (modulePackageKey modu)


lookupLoadedHomeModuleGRE  :: GhcMonad m => ModuleName -> m (Maybe GlobalRdrEnv)
lookupLoadedHomeModuleGRE mod_name = withSession $ \hsc_env ->
  case lookupUFM (hsc_HPT hsc_env) mod_name of
    Just mod_info      -> return (mi_globals (hm_iface mod_info))
    _not_a_home_module -> return Nothing


isNameSym :: Name -> Bool
isNameSym = isSymOcc . nameOccName


isVarSym :: OccName -> Bool
isVarSym = isLexVarSym . occNameFS

isConSym :: OccName -> Bool
isConSym = isLexConSym . occNameFS


getMainDeclBinder :: HsDecl name -> [name]
getMainDeclBinder (TyClD d) = [tcdName d]
getMainDeclBinder (ValD d) =
  case collectHsBindBinders d of
    []       -> []
    (name:_) -> [name]
getMainDeclBinder (SigD d) = sigNameNoLoc d
getMainDeclBinder (ForD (ForeignImport name _ _ _)) = [unLoc name]
getMainDeclBinder (ForD (ForeignExport _ _ _ _)) = []
getMainDeclBinder _ = []

-- Extract the source location where an instance is defined. This is used
-- to correlate InstDecls with their Instance/CoAxiom Names, via the
-- instanceMap.
getInstLoc :: InstDecl name -> SrcSpan
getInstLoc (ClsInstD (ClsInstDecl { cid_poly_ty = L l _ })) = l
getInstLoc (DataFamInstD (DataFamInstDecl { dfid_tycon = L l _ })) = l
getInstLoc (TyFamInstD (TyFamInstDecl
  -- Since CoAxioms' Names refer to the whole line for type family instances
  -- in particular, we need to dig a bit deeper to pull out the entire
  -- equation. This does not happen for data family instances, for some reason.
  { tfid_eqn = L _ (TyFamEqn { tfe_rhs = L l _ })})) = l

-- Useful when there is a signature with multiple names, e.g.
--   foo, bar :: Types..
-- but only one of the names is exported and we have to change the
-- type signature to only include the exported names.
filterLSigNames :: (name -> Bool) -> LSig name -> Maybe (LSig name)
filterLSigNames p (L loc sig) = L loc <$> (filterSigNames p sig)

filterSigNames :: (name -> Bool) -> Sig name -> Maybe (Sig name)
filterSigNames p orig@(SpecSig n _ _)          = ifTrueJust (p $ unLoc n) orig
filterSigNames p orig@(InlineSig n _)          = ifTrueJust (p $ unLoc n) orig
filterSigNames p (FixSig (FixitySig ns ty)) =
  case filter (p . unLoc) ns of
    []       -> Nothing
    filtered -> Just (FixSig (FixitySig filtered ty))
filterSigNames _ orig@(MinimalSig _ _)      = Just orig
filterSigNames p (TypeSig ns ty nwcs) =
  case filter (p . unLoc) ns of
    []       -> Nothing
    filtered -> Just (TypeSig filtered ty nwcs)
filterSigNames _ _                           = Nothing

ifTrueJust :: Bool -> name -> Maybe name
ifTrueJust True  = Just
ifTrueJust False = const Nothing

sigName :: LSig name -> [name]
sigName (L _ sig) = sigNameNoLoc sig

sigNameNoLoc :: Sig name -> [name]
sigNameNoLoc (TypeSig   ns _ _)        = map unLoc ns
sigNameNoLoc (PatSynSig n _ _ _ _)     = [unLoc n]
sigNameNoLoc (SpecSig   n _ _)         = [unLoc n]
sigNameNoLoc (InlineSig n _)           = [unLoc n]
sigNameNoLoc (FixSig (FixitySig ns _)) = map unLoc ns
sigNameNoLoc _                         = []


isTyClD :: HsDecl a -> Bool
isTyClD (TyClD _) = True
isTyClD _ = False


isClassD :: HsDecl a -> Bool
isClassD (TyClD d) = isClassDecl d
isClassD _ = False


isDocD :: HsDecl a -> Bool
isDocD (DocD _) = True
isDocD _ = False


isInstD :: HsDecl a -> Bool
isInstD (InstD _) = True
isInstD _ = False


isValD :: HsDecl a -> Bool
isValD (ValD _) = True
isValD _ = False


declATs :: HsDecl a -> [a]
declATs (TyClD d) | isClassDecl d = map (unL . fdLName . unL) $ tcdATs d
declATs _ = []


pretty :: Outputable a => DynFlags -> a -> String
pretty = showPpr


trace_ppr :: Outputable a => DynFlags -> a -> b -> b
trace_ppr dflags x y = trace (pretty dflags x) y


-------------------------------------------------------------------------------
-- * Located
-------------------------------------------------------------------------------


unL :: Located a -> a
unL (L _ x) = x


reL :: a -> Located a
reL = L undefined


before :: Located a -> Located a -> Bool
before = (<) `on` getLoc


instance Foldable (GenLocated l) where
  foldMap f (L _ x) = f x

-------------------------------------------------------------------------------
-- * NamedThing instances
-------------------------------------------------------------------------------


instance NamedThing (TyClDecl Name) where
  getName = tcdName

-------------------------------------------------------------------------------
-- * Subordinates
-------------------------------------------------------------------------------


class Parent a where
  children :: a -> [Name]


instance Parent (ConDecl Name) where
  children con =
    case con_details con of
      RecCon fields -> map unL $ concatMap (cd_fld_names . unL) (unL fields)
      _             -> []

instance Parent (TyClDecl Name) where
  children d
    | isDataDecl  d = map unL $ concatMap (con_names . unL)
                              $ (dd_cons . tcdDataDefn) $ d
    | isClassDecl d =
        map (unL . fdLName . unL) (tcdATs d) ++
        [ unL n | L _ (TypeSig ns _ _) <- tcdSigs d, n <- ns ]
    | otherwise = []


-- | A parent and its children
family :: (NamedThing a, Parent a) => a -> (Name, [Name])
family = getName &&& children


familyConDecl :: ConDecl Name -> [(Name, [Name])]
familyConDecl d = zip (map unL (con_names d)) (repeat $ children d)

-- | A mapping from the parent (main-binder) to its children and from each
-- child to its grand-children, recursively.
families :: TyClDecl Name -> [(Name, [Name])]
families d
  | isDataDecl  d = family d : concatMap (familyConDecl . unL) (dd_cons (tcdDataDefn d))
  | isClassDecl d = [family d]
  | otherwise     = []


-- | A mapping from child to parent
parentMap :: TyClDecl Name -> [(Name, Name)]
parentMap d = [ (c, p) | (p, cs) <- families d, c <- cs ]


-- | The parents of a subordinate in a declaration
parents :: Name -> HsDecl Name -> [Name]
parents n (TyClD d) = [ p | (c, p) <- parentMap d, c == n ]
parents _ _ = []


-------------------------------------------------------------------------------
-- * Utils that work in monads defined by GHC
-------------------------------------------------------------------------------


modifySessionDynFlags :: (DynFlags -> DynFlags) -> Ghc ()
modifySessionDynFlags f = do
  dflags <- getSessionDynFlags
  _ <- setSessionDynFlags (f dflags)
  return ()


-- | A variant of 'gbracket' where the return value from the first computation
-- is not required.
gbracket_ :: ExceptionMonad m => m a -> m b -> m c -> m c
gbracket_ before_ after thing = gbracket before_ (const after) (const thing)

-- Extract the minimal complete definition of a Name, if one exists
minimalDef :: GhcMonad m => Name -> m (Maybe ClassMinimalDef)
minimalDef n = do
  mty <- lookupGlobalName n
  case mty of
    Just (ATyCon (tyConClass_maybe -> Just c)) -> return . Just $ classMinimalDef c
    _ -> return Nothing

-------------------------------------------------------------------------------
-- * DynFlags
-------------------------------------------------------------------------------


setObjectDir, setHiDir, setStubDir, setOutputDir :: String -> DynFlags -> DynFlags
setObjectDir  f d = d{ objectDir  = Just f}
setHiDir      f d = d{ hiDir      = Just f}
setStubDir    f d = d{ stubDir    = Just f, includePaths = f : includePaths d }
  -- -stubdir D adds an implicit -I D, so that gcc can find the _stub.h file
  -- \#included from the .hc file when compiling with -fvia-C.
setOutputDir  f = setObjectDir f . setHiDir f . setStubDir f


{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ViewPatterns #-}

-- | This module contains the logic to convert ghc-lib-parser into SimpleHaskell.
module GHCHaskell (parseGHCModule, decodeModule, showPpr) where

import Data.Maybe (mapMaybe)
import GHC.Data.Bag (bagToList)
import GHC.Data.FastString (FastString, fsLit, mkFastString)
import GHC.Data.StringBuffer (stringToStringBuffer)
import GHC.Hs (HsConDetails (..), HsModule (..), Pat (..))
import GHC.Hs.Extension (GhcPs)
import GHC.Hs.ImpExp (ImportDecl (..))
import GHC.Parser (parseModuleNoHaddock)
import GHC.Parser.Lexer (ParseResult (..), getPsErrorMessages, initParserState, mkParserOpts, unP)
import GHC.Types.Name (occName, pprOccName)
import GHC.Types.SrcLoc (GenLocated (..), mkRealSrcLoc)
import GHC.Unit (moduleNameString)
import GHC.Utils.Error (DiagOpts (..))
import GHC.Utils.Outputable (Outputable (ppr), defaultSDocContext, runSDoc)
import GHC.Utils.Ppr (renderStyle, style)
import Language.Haskell.Syntax (IdP)
import Language.Haskell.Syntax.Binds (HsBindLR (..), HsLocalBindsLR (..), HsValBindsLR (..))
import Language.Haskell.Syntax.Decls (HsDecl (..))
import Language.Haskell.Syntax.Expr (ArithSeqInfo (..), GRHS (..), GRHSs (..), HsDoFlavour (..), HsExpr (..), HsTupArg (..), LHsExpr, Match (..), MatchGroup (..), StmtLR (..))

import SimpleHaskell

-- | Call ghc-lib-parser 'parseModuleNoHaddock'
parseGHCModule :: (FilePath, String) -> HsModule
parseGHCModule (fp, source) =
    case unP GHC.Parser.parseModuleNoHaddock parseState of
        POk _ res -> rl res
        PFailed s -> error $ "parse failure: " <> showPpr (getPsErrorMessages s)
  where
    srcLoc = mkRealSrcLoc (mkFastString fp) 0 0
    diagOpts = DiagOpts mempty mempty False False Nothing defaultSDocContext
    opts = mkParserOpts mempty diagOpts [] False False False False
    sb = stringToStringBuffer source
    parseState = initParserState opts sb srcLoc

-- | Convert ghc-lib-parser 'HsModule' to SimpleHaskell.Module
decodeModule :: HsModule -> Module
decodeModule hsm = Module imports decls
  where
    imports = map (decodeImports . rl) hsm.hsmodImports
    decls = map (decodeDecls . rl) hsm.hsmodDecls

decodeImports :: ImportDecl GhcPs -> Import
decodeImports = \case
    ImportDecl{ideclName, ideclHiding} ->
        let names = case ideclHiding of
                Nothing -> []
                Just (True, _) -> error "hidding not supported"
                Just (False, xs) -> map (fromPpr . rl) (rl xs)
         in Import (fsLit $ moduleNameString (rl ideclName)) names

decodeExpr :: HsExpr GhcPs -> Expr
decodeExpr = \case
    HsVar _x (rl -> lidp) -> EVar (decodeIdp lidp)
    HsLit _x l -> ELit (fromPpr l)
    HsOverLit _x l -> ELit (fromPpr l)
    HsApp _x (rl -> l) (rl -> r) -> EApp (decodeExpr l) (decodeExpr r)
    OpApp _ (rl -> a) (rl -> b) (rl -> c) -> EOp (decodeExpr a) (decodeExpr b) (decodeExpr c)
    NegApp _ (rl -> e) _ -> case decodeExpr e of
        ELit v -> ELit ("-" <> v)
        _ -> error ("Unknown neg app: " <> showPpr e)
    HsPar _ _leftPar (rl -> e) _rightPar -> EPar (decodeExpr e)
    HsLam _ mg -> ELam (decodeMatch (rl (head (rl mg.mg_alts))))
    HsCase _ (rl -> e) mg -> ECase (decodeExpr e) (map (decodeMatch . rl) (rl mg.mg_alts))
    ArithSeq _ _ seq' ->
        let (e1, e2, e3) = case seq' of
                From (rl -> f) -> (f, Nothing, Nothing)
                FromTo (rl -> f) (rl -> t) -> (f, Just t, Nothing)
                FromThen (rl -> f) (rl -> s) -> (f, Nothing, Just s)
                FromThenTo (rl -> f) (rl -> s) (rl -> t) -> (f, Just t, Just s)
         in ERange (decodeExpr e1) (decodeExpr <$> e2) (decodeExpr <$> e3)
    ExplicitList _x xs -> EList $ map (decodeExpr . rl) xs
    ExplicitTuple _x xs _ ->
        let getTupleExpr = \case
                Present _ (rl -> e) -> Just (decodeExpr e)
                _ -> Nothing
         in ETuple $ mapMaybe getTupleExpr xs
    HsIf _x (rl -> e1) (rl -> e2) (rl -> e3) -> EIf (decodeExpr e1) (decodeExpr e2) (decodeExpr e3)
    HsLet _x _let (HsValBinds _ (ValBinds _ (bagToList -> binds) _sigs)) _in (rl -> e) ->
        let bindings = map (decodeBinding . rl) binds
         in ELet bindings (decodeExpr e)
    HsDo _ flavor (rl -> xs) -> case flavor of
        ListComp -> EComp (map (decodeStatement . rl) xs)
        DoExpr Nothing -> EDo (map (decodeStatement . rl) xs)
        _ -> error ("Unknown do flavor: " <> showPpr xs)
    SectionL{} -> ELit "SectionL"
    SectionR _ (rl -> e1) (rl -> e2) -> EApp (decodeExpr e1) (decodeExpr e2)
    ExprWithTySig _ (rl -> e) _ -> decodeExpr e
    e -> error ("Unknown expr: " <> showPpr e)

decodeStatement :: StmtLR GhcPs GhcPs _ -> Statement
decodeStatement = \case
    BindStmt _ (rl -> pat) (rl -> e) -> SBind (decodePattern pat) (decodeExpr e)
    LastStmt _ (rl -> e) _ _ -> SBody (decodeExpr e)
    BodyStmt _ (rl -> e) _ _ -> SBody (decodeExpr e)
    s -> error ("Unknown statement: " <> showPpr s)

decodeGuardedExpr :: GRHS GhcPs (LHsExpr GhcPs) -> GuardedExpr
decodeGuardedExpr = \case
    GRHS _x guards (rl -> body) ->
        let gs = map (decodeGuard . rl) guards
            expr = decodeExpr body
         in GuardedExpr gs expr
  where
    decodeGuard :: StmtLR _ _ _ -> Expr
    decodeGuard = \case
        BodyStmt _x (rl -> body) _ _ -> decodeExpr body
        g -> error $ "Unknown guard: " <> showPpr g

decodePattern :: Pat GhcPs -> Pattern
decodePattern = \case
    VarPat _ (rl -> lidp) -> PVar (decodeIdp lidp)
    LitPat _ litp -> PLit (fromPpr litp)
    NPat _ litp _ _ -> PLit (fromPpr litp)
    TuplePat _ xs _ -> PTup (map (decodePattern . rl) xs)
    ListPat _ xs -> PList (map (decodePattern . rl) xs)
    ParPat _ _ (rl -> p) _ -> PPar (decodePattern p)
    WildPat _ -> PLit "_"
    AsPat _ litp (rl -> pat) -> PNam (fromPpr litp) (decodePattern pat)
    ConPat _ a b -> case b of
        InfixCon (rl -> p1) (rl -> p2) -> PIco (fromPpr a) (decodePattern p1) (decodePattern p2)
        PrefixCon _ xs -> PCon (fromPpr a) (map (decodePattern . rl) xs)
        _ -> error $ "Unknown conpat: " <> showPpr b
    p -> error $ "Unknown pattern: " <> showPpr p

decodeDecls :: HsDecl GhcPs -> Decl
decodeDecls = \case
    ValD _ b -> ValueDecl (decodeBinding b)
    SigD _ s -> SigDecl (fromPpr s)
    TyClD _ s -> TypeDecl (fromPpr s)
    x -> error ("Unknown decl: " <> showPpr x)

decodeBinding :: HsBindLR GhcPs GhcPs -> Binding
decodeBinding = \case
    FunBind _ (rl -> fid) mg _ -> Binding (decodeIdp fid) (map (decodeMatch . rl) (rl mg.mg_alts))
    PatBind _ (rl -> pat) rhs _ -> BindingPattern (decodePattern pat) (decodeGRHSs rhs)
    x -> error ("Unknown binding: " <> showPpr x)

decodeGRHSs :: GRHSs GhcPs (LHsExpr GhcPs) -> [GuardedExpr]
decodeGRHSs = \case
    GRHSs _ rhs _ -> map (decodeGuardedExpr . rl) rhs

decodeMatch :: Match GhcPs (LHsExpr GhcPs) -> BindingMatch
decodeMatch match =
    let pats = map (decodePattern . rl) match.m_pats
        GRHSs _ext grhss _where = match.m_grhss
        grhs = map (decodeGuardedExpr . rl) grhss
     in BindingMatch pats grhs

-- | Helpers
fromPpr :: Outputable a => a -> FastString
fromPpr = fsLit . showPpr

showPpr :: Outputable a => a -> String
showPpr = renderStyle style . flip runSDoc defaultSDocContext . ppr

decodeIdp :: IdP GhcPs -> FastString
decodeIdp = fsLit . showPpr . pprOccName . occName

-- | remove Location tag
rl :: GenLocated b a -> a
rl (L _ h) = h

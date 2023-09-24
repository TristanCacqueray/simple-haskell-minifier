-- | This module implements a haskell source minifier.
module Minifier where

import Control.Monad (when)
import Data.Bifunctor (second)
import Data.ByteString qualified as BS
import Data.Char
import Data.List (dropWhileEnd, group, intersperse, isSuffixOf, mapAccumL, sort)
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text.Lazy (Text)
import Data.Text.Lazy qualified as T
import Data.Text.Lazy.Encoding (decodeUtf8, encodeUtf8)
import Data.Text.Lazy.IO qualified as T
import GHC.Data.FastString
import GHCHaskell
import SimpleHaskell
import System.Directory
import System.Environment (getArgs)
import Test.Tasty
import Test.Tasty.Golden (goldenVsStringDiff)
import Text.Pretty.Simple (pShowNoColor)

minify :: Module -> Module
minify = renameModule . promoteRepeated . inlineSingleUse

names :: [Char]
names = ['a' .. 'z'] ++ ['α'..'ω']

availNames :: NameEnv -> [Char]
availNames env = filter (`notElem` (snd <$> env)) names

type NameEnv = [(Name, Char)]

declNames :: [Decl] -> [Name]
declNames = mapMaybe getName
  where
    getName = \case
        (ValueDecl (Binding name _)) -> Just name
        _ -> Nothing

-- | Module renamer change binders to single letter (except for "main")
renameModule :: Module -> Module
renameModule (Module imps decls) = Module imps (map (renameTopLevel topLevelEnv) decls)
  where
    -- handle top-level declaration
    renameTopLevel :: NameEnv -> Decl -> Decl
    renameTopLevel env (ValueDecl binding) = ValueDecl (renameBinding env binding)
    renameTopLevel _ d = d

    -- create new names for top-level declaration
    topLevelEnv :: NameEnv
    topLevelEnv = zip (filter ("main" /=) $ declNames decls) names

-- | Apply renameModule strategy to individual binding.
renameBinding :: NameEnv -> Binding -> Binding
renameBinding benv = \case
  Binding name matches -> Binding (replaceName benv name) (map (renameMatch benv) matches)
  BindingPattern pat exprs -> BindingPattern (renamePattern benv pat) (map (renameGExpr benv) exprs)
  where
    replaceName :: NameEnv -> Name -> Name
    replaceName env oldName = case lookup oldName env of
        Just c -> consFS c mempty
        Nothing -> oldName

    renameMatch :: NameEnv -> BindingMatch -> BindingMatch
    renameMatch env (BindingMatch pats gexps) =
        let patEnv = zip (foldMap collectPattern pats) (availNames env)
            mEnv = patEnv <> env
         in BindingMatch (map (renamePattern mEnv) pats) (map (renameGExpr mEnv) gexps)

    renamePattern :: NameEnv -> Pattern -> Pattern
    renamePattern env = \case
        PVar n -> PVar (replaceName env n)
        PLit n -> PLit n
        PList xs -> PList (renamePattern env <$> xs)
        PTup xs -> PTup (map (renamePattern env) xs)
        PNam n p -> PNam (replaceName env n) (renamePattern env p)
        PIco n p1 p2 -> PIco n (renamePattern env p1) (renamePattern env p2)
        PCon n xs -> PCon n (map (renamePattern env) xs)
        PPar p -> PPar (renamePattern env p)

    renameGExpr :: NameEnv -> GuardedExpr -> GuardedExpr
    renameGExpr env (GuardedExpr guards expr) = GuardedExpr (map (renameExpr env) guards) (renameExpr env expr)

    collectBinding :: Binding -> [Name]
    collectBinding = \case
      Binding bname _ -> [bname]
      BindingPattern pat _ -> collectPattern pat

    renameExpr :: NameEnv -> Expr -> Expr
    renameExpr env = \case
        ELit e -> ELit e
        EVar e -> EVar (replaceName env e)
        EOp e1 e2 e3 -> EOp (renameExpr env e1) (renameExpr env e2) (renameExpr env e3)
        EApp e1 e2 -> EApp (renameExpr env e1) (renameExpr env e2)
        ELam bm -> ELam (renameMatch env bm)
        ECase e bms -> ECase (renameExpr env e) (map (renameMatch env) bms)
        EDo xs -> EDo (snd $ mapAccumL renameStatement env xs)
        EComp xs -> EComp (snd $ mapAccumL renameStatement env xs)
        EList xs -> EList (map (renameExpr env) xs)
        ETuple xs -> ETuple (map (renameExpr env) xs)
        EPar expr -> EPar (renameExpr env expr)
        EIf e1 e2 e3 -> EIf (renameExpr env e1) (renameExpr env e2) (renameExpr env e3)
        ERange e1 mE2 mE3 -> ERange (renameExpr env e1) (renameExpr env <$> mE2) (renameExpr env <$> mE3)
        ELet bindings e ->
            let bindingEnv :: NameEnv
                -- TODO: remove the names from the parent that are not used in sub exprs.
                bindingEnv = zip (concatMap collectBinding bindings) (availNames env)
                newEnv = bindingEnv <> env
                newBindings = map (renameBinding newEnv) bindings
             in ELet newBindings (renameExpr newEnv e)

    renameStatement :: NameEnv -> Statement -> (NameEnv, Statement)
    renameStatement env = \case
        SBind pat expr ->
            let patEnv = zip (collectPattern pat) (availNames env)
                bindEnv = patEnv <> env
             in (bindEnv, SBind (renamePattern bindEnv pat) (renameExpr bindEnv expr))
        SBody expr -> (env, SBody (renameExpr env expr))

collectPattern :: Pattern -> [Name]
collectPattern = \case
    PVar n -> [n]
    PLit{} -> []
    PTup xs -> concatMap collectPattern xs
    PList xs -> concatMap collectPattern xs
    PNam n p -> n : collectPattern p
    PIco _ p1 p2 -> concatMap collectPattern [p1, p2]
    PCon _ xs -> concatMap collectPattern xs
    PPar p -> collectPattern p

type BindedVars = Set LexicalFastString

{- | Promote repeated name to top-level binding.

Note: this only handles single variable like `putStr` or `maximum`.
      input code should promote composition like `max 0` manually.
-}
promoteRepeated :: Module -> Module
promoteRepeated (Module imps decls) = Module imps (renamedDecls <> newDecls)
  where
    topLevelNames :: [Name]
    topLevelNames = monoPoly <> declNames decls

    -- these names are problematic when automatically promoted to top-level
    monoPoly = ["show", "max", "min"]

    -- look for repeated names
    repeatedNames :: [Name]
    repeatedNames = replacableNames $ foldMap collectDeclFreeNames decls
      where
        topLevelBinds :: BindedVars
        topLevelBinds = Set.fromList . map LexicalFastString $ topLevelNames

        collectDeclFreeNames = \case
            ValueDecl b -> collectFreeNames topLevelBinds b
            _ -> []

    mkRepeatedBinder :: Name -> Name
    mkRepeatedBinder origName = "repeat_" <> origName

    -- Replace repated name with top-level alias
    renamedDecls = map doDecl decls
      where
        doDecl :: Decl -> Decl
        doDecl = \case
            ValueDecl binding -> ValueDecl (inlineExprs renamedExprs binding)
            d -> d
        renamedExprs :: IExprs
        renamedExprs = otherwiseTrue <> map (\n -> (n, EVar (mkRepeatedBinder n))) repeatedNames
        -- if otherwise is promoted, then it can replace 'True' too
        otherwiseTrue :: IExprs
        otherwiseTrue
            | "otherwise" `elem` repeatedNames = [("True", EVar (mkRepeatedBinder "otherwise"))]
            | otherwise = []

    -- Create new Decl for promoted names
    newDecls :: [Decl]
    newDecls = map makeNewDecls repeatedNames
      where
        makeNewDecls :: Name -> Decl
        makeNewDecls origName =
            let
                varValue
                    | origName == "otherwise" = "True"
                    | otherwise = origName
                match = BindingMatch [] [GuardedExpr [] (EVar varValue)]
             in
                ValueDecl (Binding (mkRepeatedBinder origName) [match])

-- | Return the list of free variables.
collectFreeNames :: BindedVars -> Binding -> [Name]
collectFreeNames parentVars = \case
  Binding name matches ->
    let scopeVars = Set.insert (LexicalFastString name) parentVars
     in foldMap (goMatches scopeVars) matches
  BindingPattern pat gexprs ->
    let scopeVars = parentVars <> goPats pat
     in foldMap (goGexpr scopeVars scopeVars) gexprs
  where
    goMatches :: BindedVars -> BindingMatch -> [Name]
    goMatches scopeVars (BindingMatch pats gexprs) =
        let bindVars = Set.union scopeVars (foldMap goPats pats)
         in foldMap (goGexpr scopeVars bindVars) gexprs

    goPats :: Pattern -> BindedVars
    goPats = \case
        PVar n -> Set.singleton (LexicalFastString n)
        PLit{} -> mempty
        PTup xs -> foldMap goPats xs
        PList xs -> foldMap goPats xs
        PNam n p -> foldMap goPats [PVar n, p]
        PIco _ p1 p2 -> foldMap goPats [p1, p2]
        PCon _ xs -> foldMap goPats xs
        PPar p -> goPats p

    goGexpr :: BindedVars -> BindedVars -> GuardedExpr -> [Name]
    goGexpr scopeVars exprVars (GuardedExpr guards expr) =
        let isFree n = LexicalFastString n `Set.notMember` exprVars
         in filter isFree (foldMap goExpr (expr : guards))
      where
        goExpr :: Expr -> [Name]
        goExpr = \case
            ELit n -> [n]
            EVar n -> [n]
            EOp e1 e2 e3 -> foldMap goExpr [e1, e2, e3]
            EApp e1 e2 -> foldMap goExpr [e1, e2]
            ELam bm -> goMatches scopeVars bm
            ECase e bms -> goExpr e <> concatMap (goMatches scopeVars) bms
            EDo xs -> foldMap goStatement xs
            EComp xs -> foldMap goStatement xs
            EList xs -> foldMap goExpr xs
            ETuple xs -> foldMap goExpr xs
            EPar e -> goExpr e
            EIf e1 e2 e3 -> foldMap goExpr [e1, e2, e3]
            ERange e1 mE2 mE3 -> foldMap goExpr (e1 : maybeToList mE2 <> maybeToList mE3)
            ELet bindings e ->
                let bindVars :: BindedVars
                    bindVars = foldMap goBinding bindings
                    letVars = Set.union exprVars bindVars
                 in foldMap (collectFreeNames letVars) bindings <> goGexpr scopeVars letVars (GuardedExpr [] e)

        goBinding :: Binding -> BindedVars
        goBinding = \case
          Binding n _ -> Set.fromList [LexicalFastString n]
          BindingPattern pat _ -> goPats pat

        goStatement :: Statement -> [Name]
        goStatement = \case
            SBind _pat e -> goExpr e
            SBody e -> goExpr e

-- | Check if promoting a name to a top-level binder is useful.
replacableNames :: [Name] -> [Name]
replacableNames = mapMaybe mkName . group . sort . map LexicalFastString
  where
    mkName :: [LexicalFastString] -> Maybe Name
    mkName [] = Nothing
    mkName [_] = Nothing
    mkName xs@(LexicalFastString x : _)
        | not (any isAlphaNum name) = Nothing
        | l >= 5 && c >= 2 = Just x
        | l >= 3 && c >= 3 = Just x
        | l >= 2 && c >= 5 = Just x
        | otherwise = Nothing
      where
        name = unpackFS x
        l = lengthFS x
        c = length xs

-- | Inline binder expr that are used only once.
inlineSingleUse :: Module -> Module
inlineSingleUse (Module imps decls) = Module imps newDecls
  where
    -- do an extra pass to inline expr present in inlined expr
    inlined :: [(FastString, Expr)]
    inlined = map (second doSecondPass) inlined1
      where
        doSecondPass :: Expr -> Expr
        doSecondPass e = case inlineExprs inlined1 (Binding "" [BindingMatch [] [GuardedExpr [] e]]) of
            Binding _ [BindingMatch _ [GuardedExpr _ x]] -> x
            _ -> error "bad pass"

    inlined1 :: [(FastString, Expr)]
    inlined1 = mapMaybe declUsedOnce decls
      where
        declUsedOnce = \case
            ValueDecl (Binding n bms@[BindingMatch [] [GuardedExpr [] e]])
                | n == "main" -> Nothing
                | -- detect recursive binding
                  countOccurenceInBinding n (Binding "" bms) > 0 ->
                    Nothing
                | countOccurence n == 1 -> Just (n, e)
            _ -> Nothing

    countOccurence :: FastString -> Int
    countOccurence name = sum (map countDecl decls)
      where
        countDecl = \case
            ValueDecl binding -> countOccurenceInBinding name binding
            _ -> 0

    newDecls :: [Decl]
    newDecls = map doInline (filter notInlined decls)
      where
        notInlined = \case
            ValueDecl (Binding n _) -> n `notElem` (fst <$> inlined)
            _ -> True

    doInline :: Decl -> Decl
    doInline = \case
        ValueDecl binding -> ValueDecl (inlineExprs inlined binding)
        e -> e

-- | Apply inlineSingleUse to individual binding.
type IExprs = [(FastString, Expr)]

inlineExprs :: IExprs -> Binding -> Binding
inlineExprs inlined = \case
  Binding bname matches -> Binding bname (map (inlineMatch inlined) matches)
  BindingPattern pat gexprs -> BindingPattern pat (map (inlineGexpr inlined) gexprs)
  where
    inlineMatch :: IExprs -> BindingMatch -> BindingMatch
    inlineMatch ix (BindingMatch pats gexprs) = BindingMatch pats (map (inlineGexpr (removeShadowed ix)) gexprs)
      where
        removeShadowed = filter shadowedInPat
        patNames = concatMap collectPattern pats
        shadowedInPat (n, _) = n `notElem` patNames

    inlineGexpr :: IExprs -> GuardedExpr -> GuardedExpr
    inlineGexpr ix (GuardedExpr guards expr) = GuardedExpr (map (inlineExpr ix) guards) (inlineExpr ix expr)

    inlineExpr :: IExprs -> Expr -> Expr
    inlineExpr ix = \case
        ELit x -> ELit x
        e@(EVar name) -> case fromMaybe e (lookup name ix) of
            x@EOp{} -> EPar x
            x@EApp{} -> EPar x
            x -> x
        EOp e1 e2 e3 -> EOp (inlineExpr ix e1) (inlineExpr ix e2) (inlineExpr ix e3)
        ELam bm -> ELam (inlineMatch ix bm)
        ECase e bms -> ECase (inlineExpr ix e) (map (inlineMatch ix) bms)
        EApp e1 e2 -> EApp (inlineExpr ix e1) (inlineExpr ix e2)
        EDo xs -> EDo (map (inlineStatement ix) xs)
        EComp xs -> EComp (map (inlineStatement ix) xs)
        EList xs -> EList (map (inlineExpr ix) xs)
        ETuple xs -> ETuple (map (inlineExpr ix) xs)
        EPar xs -> EPar (inlineExpr ix xs)
        ELet bindings e ->
            -- TODO: remove shadow from ix
            ELet (map (inlineExprs ix) bindings) (inlineExpr ix e)
        EIf e1 e2 e3 -> EIf (inlineExpr ix e1) (inlineExpr ix e2) (inlineExpr ix e3)
        ERange e1 mE2 mE3 -> ERange (inlineExpr ix e1) (inlineExpr ix <$> mE2) (inlineExpr ix <$> mE3)

    inlineStatement :: IExprs -> Statement -> Statement
    inlineStatement ix = \case
        SBind pat e -> SBind pat (inlineExpr ix e)
        SBody e -> SBody (inlineExpr ix e)

countOccurenceInBinding :: Name -> Binding -> Int
countOccurenceInBinding name = \case
  Binding bname matches
    | name == bname -> 0
    | otherwise -> sum (map countInMatches matches)
  BindingPattern _ gexprs -> sum (map countInGExpr gexprs)
  where
    countInMatches :: BindingMatch -> Int
    countInMatches (BindingMatch pats gexprs)
        | name `elem` patternNames pats = 0
        | otherwise = sum $ map countInGExpr gexprs

    patternNames :: [Pattern] -> [Name]
    patternNames = concatMap getName
      where
        getName = \case
            PVar n -> [n]
            PLit{} -> []
            PTup xs -> concatMap getName xs
            PList xs -> concatMap getName xs
            PNam n p -> n : getName p
            PIco _ p1 p2 -> concatMap getName [p1, p2]
            PCon _ xs -> concatMap getName xs
            PPar p -> getName p

    countInGExpr :: GuardedExpr -> Int
    countInGExpr (GuardedExpr guards expr) = countInExprs (expr : guards)

    countInExprs :: [Expr] -> Int
    countInExprs = sum . map countInExpr

    countInExpr :: Expr -> Int
    countInExpr = \case
        ELit _ -> 0
        EVar n
            | n == name -> 1
            | otherwise -> 0
        EApp e1 e2 -> countInExprs [e1, e2]
        ELam bm -> countInMatches bm
        ECase e bms -> countInExpr e + sum (map countInMatches bms)
        EDo xs -> sum (map countInStatement xs)
        EComp xs -> sum (map countInStatement xs)
        EOp e1 e2 e3 -> countInExprs [e1, e2, e3]
        EList xs -> countInExprs xs
        ETuple xs -> countInExprs xs
        EPar e -> countInExpr e
        ELet bindings expr
            | -- The name is shadowed by a binding
              name `elem` concatMap bindingNames bindings ->
                0
            | otherwise ->
                countInExpr expr + sum (map (countOccurenceInBinding name) bindings)
        EIf e1 e2 e3 -> countInExprs [e1, e2, e3]
        ERange e mE1 mE2 -> countInExprs $ e : maybeToList mE1 <> maybeToList mE2

    countInStatement :: Statement -> Int
    countInStatement = \case
        SBind _pat e -> countInExpr e
        SBody e -> countInExpr e

    bindingNames :: Binding -> [Name]
    bindingNames = \case
      Binding n _ -> [n]
      BindingPattern pat _ -> patternNames [pat]


-- | Render a module with one declaration per line.
renderModule :: Module -> Text
renderModule (Module imps decls) = T.intercalate ";\n" content
  where
    content :: [Text]
    content = map renderImport imps <> concatMap renderDecl decls
    renderImport :: Import -> Text
    renderImport (Import name []) = "import " <> ft name
    renderImport (Import name inames) = "import " <> ft name <> "(" <> ft (mconcat (intersperse "," inames)) <> ")"
    renderDecl = \case
        ValueDecl binding -> renderBinding binding
        -- Ignore non value
        _ -> []

concatName :: Text -> Text -> Text
concatName a b
    | next `elem` ['"', '(', '[', ':', ','] = a <> b
    | prev `elem` ['"', ')', ']', ':', ','] = a <> b
    | isNumber prev && not (isNumber next) = a <> b
    | otherwise = a <> " " <> b
  where
    prev = maybe minBound snd (T.unsnoc a)
    next = maybe minBound fst (T.uncons b)

concatNames :: [Text] -> Text
concatNames = foldr concatName ""

renderBinding :: Binding -> [Text]
renderBinding = \case
  Binding name matches -> map (renderTopMatch name) matches
  BindingPattern pat exprs -> [renderPat pat <> mconcat (map (renderGuard "=") exprs)]
  where
    renderTopMatch :: FastString -> BindingMatch -> Text
    renderTopMatch name bm = ft name <> renderMatch "=" bm

    renderMatch :: Text -> BindingMatch -> Text
    renderMatch sep (BindingMatch pats exprs) =
        mconcat (map renderPat pats) <> mconcat (map (renderGuard sep) exprs)

    renderPat :: Pattern -> Text
    renderPat = \case
        PVar v -> " " <> ft v
        PLit v ->
            let txt = ft v
                res
                    | T.isPrefixOf "\"" txt = txt
                    | otherwise = " " <> txt
             in res
        PTup xs -> "(" <> T.intercalate "," (map (T.dropWhile (== ' ') . renderPat) xs) <> ")"
        PList xs -> "[" <> T.intercalate "," (map (T.dropWhile (== ' ') . renderPat) xs) <> "]"
        PNam n p -> " " <> ft n <> "@" <> renderPat p
        PIco n p1 p2 -> T.dropWhile (== ' ') (renderPat p1) <> ft n <> T.dropWhile (== ' ') (renderPat p2)
        PCon n xs -> " " <> ft n <> " " <> concatNames (map renderPat xs)
        PPar e -> "(" <> renderPat e <> ")"

    renderGuard sep (GuardedExpr guards expr) =
        let txt = case guards of
                [] -> ""
                _ -> "|" <> T.intercalate "," (map renderExpr guards)
         in txt <> sep <> renderExpr expr

    renderExpr :: Expr -> Text
    renderExpr = \case
        ELit x -> ft x
        EVar "+" -> "(+)"
        EVar x -> ft x
        EApp e1 e2 -> renderExpr e1 `concatName` renderExpr e2
        ELam bm -> "\\" <> T.dropWhile (== ' ') (renderMatch "->" bm)
        ECase e bms -> "case " <> renderExpr e <> " of{" <> T.intercalate ";" (map (renderMatch "->") bms) <> "}"
        EDo xs -> "do{" <> T.intercalate ";" (map renderStatement xs) <> "}"
        EComp xs ->
            let (l, i) = (last xs, init xs)
             in "[" <> renderStatement l <> "|" <> T.intercalate "," (map renderStatement i) <> "]"
        EOp e1 e2 e3 -> renderExpr e1 <> renderOperator (renderExpr e2) <> renderExpr e3
        EPar e -> "(" <> renderExpr e <> ")"
        EList xs -> "[" <> mconcat (map renderExpr xs) <> "]"
        ETuple xs -> "(" <> T.intercalate "," (map renderExpr xs) <> ")"
        EIf e1 e2 e3 -> concatNames ["if", renderExprNoPar e1, "then", renderExprNoPar e2, "else", renderExprNoPar e3]
        ERange e1 mE2 mE3 -> "[" <> renderExpr e1 <> maybe "" (mappend "," . renderExpr) mE3 <> ".." <> maybe "" renderExpr mE2 <> "]"
        ELet bindings e -> "let " <> T.intercalate ";" (concatMap renderBinding bindings) <> " in " <> renderExpr e

    renderExprNoPar :: Expr -> Text
    renderExprNoPar = \case
        EPar e -> renderExpr e
        e -> renderExpr e

    renderStatement :: Statement -> Text
    renderStatement = \case
        SBind pat e -> T.dropWhile (== ' ') (renderPat pat) <> "<-" <> renderExpr e
        SBody e -> renderExpr e

    removePar = T.dropWhile (== '(') . T.dropWhileEnd (== ')')
    renderOperator baseName =
        let opName = removePar baseName
            opText
                | maybe False (isLetter . fst) (T.uncons opName) = "`" <> opName <> "`"
                | otherwise = opName
         in opText

minifies :: (FilePath, String) -> Text
minifies =
    renderModule
        . minify
        . decodeModule
        . parseGHCModule

main :: IO ()
main =
    getArgs >>= \case
        ["test"] -> test
        [fp] -> readFile fp >>= doProcess fp
        _ -> getContents >>= doProcess "<stdin>"
  where
    doProcess fp src = do
      let result = minifies (fp, src)
      T.putStrLn result
      when (fp /= "<stdin>") do
        T.writeFile (fp <> ".min.hs") result

-- | Test helpers
minName :: FilePath -> FilePath
minName fp = dropWhileEnd (/= '.') fp <> "min.hs"

test :: IO ()
test = do
    samplesName <- filter (not . isSuffixOf ".min.hs") <$> listDirectory "examples"
    samples <- zip samplesName <$> traverse (readFile . mappend "examples/") samplesName
    defaultMain (testGroup "Tests" $ map goldenTestAST samples)
  where
    goldenDiff ref new = ["diff", "--color=always", "-u", ref, new]
    goldenTest fp = goldenVsStringDiff fp goldenDiff ("examples/" <> fp) . pure
    goldenTestAST sample@(name, _) =
        goldenTest (minName name) $
            encodeUtf8 $
                let simpleMod = decodeModule (parseGHCModule sample)
                    final = renderModule (minify simpleMod)
                 in final <> "\n{- AST:\n" <> pShowNoColor simpleMod <> "\n-}\n"

ft :: FastString -> Text
ft = decodeUtf8 . BS.fromStrict . bytesFS

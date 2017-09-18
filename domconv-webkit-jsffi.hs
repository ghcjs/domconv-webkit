{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
-- DOM interface converter: a tool to convert Haskell files produced by
-- H/Direct into properly structured DOM wrapper

module Main where

import Prelude
import System.Directory
import System.Environment
import System.FilePath
import System.Exit
import System.IO
       (hPutStr, stderr, openFile, hClose, IOMode(..), hPutStrLn)
import Control.Monad
import Data.Maybe
import Data.Either
import Data.List
import Data.Char
import Language.Haskell.Pretty
import Language.Preprocessor.Cpphs
import BrownPLT.JavaScript
import qualified Language.Haskell.Syntax as H
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L
import qualified Utils as U
import qualified OmgParser
import LexM
import Literal
import qualified IDLSyn as I
import qualified IDLUtils
import IDLUtils hiding (getDef)
import BasicTypes
import SplitBounds
import Paths_domconv_webkit
import Control.Applicative ((<$>))
import Debug.Trace (trace)
import Language.Javascript.JMacro (jmacro, jmacroE, jLam, JExpr(..), JVal(..),
  Ident(..), jVarTy, JStat(..), toJExpr, renderJs, jsv)
import Data.List.Split
import Common
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Monoid ((<>))

callNew "AudioContext" = [jmacroE| new (window["AudioContext"] || window["webkitAudioContext"]) |]
callNew x = [jmacroE| new window[`(jsname x)`] |]

main = do
  putStrLn "domconv-webkit : Makes Gtk2Hs Haskell bindings for webkitgtk"
  p <- getProgName
  args <- getArgs
  case args of
    [] -> putStrLn "Usage: domconv-webkit-jsffi webkit.idl -Iwebkit-1.8.0/Source/WebCore"
    idl:args -> makeWebkitBindings idl args

makeWebkitBindings idl args = do
    putStrLn "The package will be created in the current directory which has to be empty."
    putStrLn $ "Processing IDL: " ++ idl ++ " args " ++ show args
    prntmap <- processIDL idl args
    -- let reversedMap = M.fromListWith S.union $ map (\(a,b)->(a,S.singleton b)) prntmap
    jsffiFixHierarchy prntmap "ghcjs-dom-jsffi/src/GHCJS/DOM/Types.hs" jsffiTypes
    exitSuccess
  where
    jsffiFixHierarchy prntmap hierarchyFile printTypes = do
        hh <- openFile (hierarchyFile ++ ".new") WriteMode
        current <- readFile hierarchyFile
        let startGuard = "-- AUTO GENERATION STARTS HERE"
            endGuard   = "-- AUTO GENERATION ENDS HERE"
            (start, rest) = span (/= startGuard) $ lines current
            middle = takeWhile (/= startGuard) $ dropWhile (/= endGuard) rest
            allParents = nub $ concatMap (rights . snd) prntmap
            childmap = M.fromListWith (<>) $ concatMap (\(a, b) -> map (, [a]) $ rights b) prntmap
        mapM_ (hPutStrLn hh) start
        hPutStrLn hh startGuard
        hPutStrLn hh $ exportSumTypes childmap
        forM_ prntmap $ \(n, parents) -> hPutStrLn hh $ ffiExports (typeFor n) allParents
        mapM_ (hPutStrLn hh) middle
        hPutStrLn hh startGuard
        hPutStrLn hh "-- The remainder of this file is generated from IDL files using domconv-webkit-jsffi"
        hPutStrLn hh $ defineSumTypes True childmap
        printTypes hh prntmap allParents

        hClose hh
        old <- doesFileExist hierarchyFile >>= \case
                    True  -> T.readFile hierarchyFile
                    False -> return ""
        new <- T.readFile (hierarchyFile ++ ".new")
        when (old /= new) $ do
            renameFile hierarchyFile (hierarchyFile ++ ".old")
            renameFile (hierarchyFile ++ ".new") hierarchyFile

    webkitFixHierarchy prntmap hierarchyFile printTypes = do
        hh <- openFile (hierarchyFile ++ ".new") WriteMode
        current <- readFile hierarchyFile
        let startGuard = "-- AUTO GENERATION STARTS HERE"
            endGuard   = "-- AUTO GENERATION ENDS HERE"
            (start, rest) = span (/= startGuard) $ lines current
            middle = takeWhile (/= startGuard) $ dropWhile (/= endGuard) rest
            allParents = nub $ concatMap (rights . snd) prntmap
        mapM_ (hPutStrLn hh) start
        hPutStrLn hh startGuard
        hPutStrLn hh $
               "    propagateGError, GType(..), DOMString(..), ToDOMString(..), FromDOMString(..)\n"
            ++ "  , FocusEvent\n"
            ++ "  , TouchEvent\n"
            ++ "  , module Graphics.UI.Gtk.WebKit.Types\n"
            ++ "  , IsGObject\n"
        forM_ prntmap $ \(n, parents) -> when (inWebKitGtk $ typeFor n) . hPutStr hh .
             oldWebKitGuard (typeFor n) $ "  , Is" ++ typeFor n ++ "\n"
        mapM_ (hPutStrLn hh) middle
        hPutStrLn hh startGuard
        hPutStrLn hh "-- The remainder of this file is generated from IDL files using domconv-webkit-jsffi"
        printTypes hh prntmap allParents

        hClose hh
        renameFile hierarchyFile (hierarchyFile ++ ".old")
        renameFile (hierarchyFile ++ ".new") hierarchyFile

    ffiExports name allParents =
            "  , " ++ name ++ "(" ++ name ++ "), un" ++ name
            ++ (if name `elem` allParents then ", Is" ++ name ++ ", to" ++ name else "")
            ++ ", no" ++ name ++ ", gType" ++ name

    jsffiTypes hh prntmap allParents =
        forM_ prntmap $ \(n, parents) -> hPutStrLn hh $
            let name = typeFor n in
            "-- | Functions for this inteface are in \"GHCJS.DOM." ++ name ++ "\".\n"
            ++ (
                if null parents
                    then ""
                    else "-- Base interface functions are in:\n"
                            ++ "--\n"
                            ++ concatMap (\parent -> "--     * \"GHCJS.DOM." ++ parent ++ "\"\n") (rights parents)
            )
            ++ "--\n"
            ++ "-- <https://developer.mozilla.org/en-US/docs/Web/API/"
                       ++ jsname name ++ " Mozilla " ++ jsname name ++ " documentation>\n"
            ++ "newtype " ++ name ++ " = " ++ name ++ " { un" ++ name ++ " :: JSVal }\n\n"
            ++ "instance Eq (" ++ name ++ ") where\n"
            ++ "  (" ++ name ++ " a) == (" ++ name ++ " b) = js_eq a b\n\n"

            ++ "instance PToJSVal " ++ name ++ " where\n"
            ++ "  pToJSVal = un" ++ name ++ "\n"
            ++ "  {-# INLINE pToJSVal #-}\n\n"

            ++ "instance PFromJSVal " ++ name ++ " where\n"
            ++ "  pFromJSVal = " ++ name ++ "\n"
            ++ "  {-# INLINE pFromJSVal #-}\n\n"

            ++ "instance ToJSVal " ++ name ++ " where\n"
            ++ "  toJSVal = return . un" ++ name ++ "\n"
            ++ "  {-# INLINE toJSVal #-}\n\n"

            ++ "instance FromJSVal " ++ name ++ " where\n"
            ++ "  fromJSVal = return . fmap " ++ name ++ " . maybeJSNullOrUndefined\n"
            ++ "  {-# INLINE fromJSVal #-}\n\n"

            ++ (if name `elem` allParents
                    then "class (" ++ intercalate ", " (map (\x -> "Is" ++ x ++ " o") (rights parents ++ ["GObject"])) ++ ") => Is" ++ name ++ " o\n"
                            ++ "to" ++ name ++ " :: Is" ++ name ++ " o => o -> " ++ name ++ "\n"
                            ++ "to" ++ name ++ " = " ++ name ++ " . coerce\n\n"
                            ++ "instance Is" ++ name ++ " " ++ name ++ "\n"
                    else "")

            ++ concatMap (\parent -> "instance Is" ++ parent ++ " " ++ name ++ "\n") (rights parents)
            ++ "instance IsGObject " ++ name ++ " where\n"
            ++ "  typeGType _ = gType" ++ name ++ "\n"
            ++ "  {-# INLINE typeGType #-}\n"

            ++ "no" ++ name ++ " :: Maybe " ++ name ++ "\n"
            ++ "no" ++ name ++ " = Nothing\n"
            ++ "{-# INLINE no" ++ name ++ " #-}\n\n"

            ++ "foreign import javascript unsafe \"window[\\\"" ++ jsname name ++ "\\\"]\" gType" ++ name ++ " :: GType\n"
    webkitTypes hh prntmap allParents =
        forM_ prntmap $ \(n, parents) -> hPutStrLn hh $
            let name = typeFor n in
               (if inWebKitGtk name then oldWebKitGuard name ("type Is" ++ name ++ " o = " ++ name ++ "Class o\n") else "")
            ++ "\n"
    oldWebKitGuard name s | webkitTypeGuard name /= "webkit-dom" = "#ifndef USE_OLD_WEBKIT\n" ++ s ++ "#endif\n"
                          | otherwise = s

moduleInWebKitGtk "Comment" = False
moduleInWebKitGtk "DocumentFragment" = False
moduleInWebKitGtk "StorageQuota" = False
moduleInWebKitGtk "MessagePort" = False
moduleInWebKitGtk x = inWebKitGtk x

processIDL idl args = do
  let epopts = parseOptions args -- ("-DLANGUAGE_GOBJECT=1":args)
  case epopts of
    Left s -> do
      hPutStrLn stderr $ "domconv: command line parse error " ++ s
      exitWith (ExitFailure 1)
    Right opts -> procopts idl opts

procopts idl opts = do
  let (hsrc, inclfile) = (readFile idl, idl)
      baseopt = [("boolean", "Bool")]
      optsb = opts {defines = defines opts ++ baseopt
                   ,boolopts = (boolopts opts) {pragma = True} }
  hsrc' <- hsrc
  hsrcpr <- runCpphs optsb inclfile hsrc'
  x <- runLexM [] inclfile hsrcpr OmgParser.parseIDL
  let prntmap = mkParentMap x
  let valmsg = valParentMap prntmap
      allParents = nub $ concatMap (rights . snd) $ M.toList prntmap
  unless (null valmsg) $ do
    mapM_ (hPutStrLn stderr) valmsg
    exitWith (ExitFailure 2)
  let modst = DOMState {
         pm = prntmap
        ,imp = []
        ,ns = "GHCJS.DOM." -- this will be the default namespace unless a pragma namespace is used.
        ,procmod = []
        ,convlog = []
      }
      modst' = domLoop modst x
--  mapM_ (putStrLn .show) $ (procmod modst')
  Prelude.writeFile "ghcjs-dom-jsffi/reexported-modules.txt" "          -- Generated by domconv-webkit-jsffi\n"
  Prelude.writeFile "ghcjs-dom-jsaddle/reexported-modules.txt" "          -- Generated by domconv-webkit-jsffi\n"
  mapM_ (mapM_ putSplit . splitModule allParents) (procmod modst')

  let getParent (a, Right b : _) = (b, a)
      getParent (a, _) = ("", a)

  return $ M.toList prntmap

-- Write a module surrounded by split begin/end comments
{-
maybeNull = ()
makeNewGObject = ()
mkEventTarget = castRef
-}

putSplit :: (H.HsModule, String -> Maybe String) -> IO ()

putSplit (H.HsModule loc modid exp imp decl, comment) = do
  let components = U.split '.' $ modName modid
      name = components !! 2

  createDirectoryIfMissing True "ghcjs-dom-jsffi/src/GHCJS/DOM/JSFFI/Generated"

  customFileExists <- doesFileExist $ "ghcjs-dom-jsffi/src/GHCJS/DOM/JSFFI" </> name ++ ".hs"
  let jsffiModule = "GHCJS.DOM.JSFFI." ++ (if customFileExists then "" else "Generated.") ++ name
      new = "{-# LANGUAGE PatternSynonyms #-}\n"
         ++ "{-# LANGUAGE ForeignFunctionInterface #-}\n"
         ++ "{-# LANGUAGE JavaScriptFFI #-}\n"
         ++ "-- For HasCallStack compatibility\n"
         ++ "{-# LANGUAGE ImplicitParams, ConstraintKinds, KindSignatures #-}\n"
         ++ prettyJS (H.HsModule loc (H.Module $ "GHCJS.DOM.JSFFI.Generated." ++ name) exp imp decl) comment
      filename = "ghcjs-dom-jsffi/src/GHCJS/DOM/JSFFI/Generated" </> name ++ ".hs"
  old <- doesFileExist filename >>= \case
                    True  -> T.readFile filename
                    False -> return ""
  when (T.unpack old /= new) $ writeFile filename new
  Prelude.appendFile "ghcjs-dom-jsffi/reexported-modules.txt" $ "          , " ++ jsffiModule ++ " as " ++ modName modid ++ "\n"

  createDirectoryIfMissing True "ghcjs-dom-jsaddle/src/GHCJS/DOM"

  Prelude.appendFile "ghcjs-dom-jsaddle/reexported-modules.txt" $ "          , JSDOM." ++ name ++ " as " ++ modName modid ++ "\n"

prettyJS (H.HsModule pos m mbExports imp decls) comment = intercalate "\n" $
       prettyPrint (H.HsModule pos m mbExports imp [])
     : map prettyDecl decls >>= lines >>= gaurdFromJSValUnchecked
  where
    prettyDecl d@(H.HsForeignImport nullLoc "javascript" _ _ (H.HsIdent defop) tpsig) = prettyPrint d
    prettyDecl d@(H.HsForeignImport nullLoc "javascript interruptible" _ _ (H.HsIdent defop) tpsig) = unlines . map (\l ->
        case ( stripPrefix "foreign import javascript interruptible unsafe" l
             , stripPrefix "foreign import javascript interruptible safe" l )of
          (Nothing, Nothing) -> l
          (_, Just x) -> "foreign import javascript interruptible" <> x
          (Just x, _) -> "foreign import javascript interruptible" <> x) . lines $ prettyPrint d
    prettyDecl d@(H.HsTypeSig _ [H.HsIdent n] _) = concat $ catMaybes [comment n, Just $ prettyPrint d]
    prettyDecl d = prettyPrint d
    interfaceName = stripPrefix "GHCJS.DOM." $ modName m
    prefix = U.toLowerInitCamel <$> interfaceName
    stripCamelPrefix p s = case stripPrefix p s of
                                Just r@(x:xs) | isUpper x -> r
                                _ -> s
    stripGetSet = stripCamelPrefix "Get" . stripCamelPrefix "Set"
    fix' pre s = case stripPrefix pre s of
                        Just rest | all isDigit rest -> ""
                        Just ('\'':_) -> ""
                        _ -> s
    fix = fix' "new" . fix' "newSync" . fix' "newAsync"
--    gaurdFromJSValUnchecked "        fromJSValUnchecked = return . pFromJSVal" =
--        [ "#if MIN_VERSION_ghcjs_base(0,2,0)"
--        , "        fromJSValUnchecked = return . pFromJSVal"
--        , "#endif" ]
    gaurdFromJSValUnchecked line = [line]

--    comment n = do
--        iname <- interfaceName
--        p <- prefix
--        f <- U.toLowerInitCamel . stripGetSet <$> stripPrefix p n
--        let func' = fix f
--            func = if null func' then "" else "." ++ func'
--        return $ "\n-- | <https://developer.mozilla.org/en-US/docs/Web/API/"
--                      ++ jsname iname ++ func ++ " Mozilla " ++ jsname iname ++ func ++ " documentation>"

-- Split a proto-module created by domLoop. All class, data, and instance definitions
-- remain in the "head" class. All methods are grouped by their `this' argument
-- context and placed into modules with the name of that context (first character removed).
-- All modules get the same imports that the "head" module has plus the "head" module itself.

splitModule :: [String] -> H.HsModule -> [(H.HsModule, String -> Maybe String)]

splitModule allParents (H.HsModule _ modid mbexp imps decls) = submods where
  headns = modNS $ modName modid
--  headmod = H.HsModule nullLoc modid headexp imps headdecls
  headdecls = filter (null . nsOf) decls
--  headexp = Just $ map (mkEIdent . declname) (classes)
--  datas = filter datadecl decls
--  datadecl (H.HsDataDecl _ _ _ _ _ _) = True
--  datadecl (H.HsNewTypeDecl _ _ _ _ _ _) = True
--  datadecl _ = False
  classes = filter classdecl headdecls
  classdecl H.HsClassDecl{} = True
  classdecl _ = False
--  instances = filter instdecl decls
--  instdecl (H.HsInstDecl _ _ _ _ _) = True
--  instdecl _ = False
  expname (H.HsEVar (H.UnQual (H.HsIdent s))) = s
  expname _ = ""
  declname (H.HsForeignImport _ _ _ _ (H.HsIdent s) _) = s
  declname (H.HsDataDecl _ _ (H.HsIdent s) _ _ _) = s
  declname (H.HsNewTypeDecl _ _ (H.HsIdent s) _ _ _) = s
  declname (H.HsClassDecl _ _ (H.HsIdent s) _ _) = s
  declname (H.HsTypeSig _ [H.HsIdent s] _) = s
  declname (H.HsFunBind [H.HsMatch _ (H.HsIdent s) _ _ _]) = s
  declname (H.HsInstDecl _ _ (H.UnQual (H.HsIdent s)) _ _) = s
  declname _ = ""
  mkEIdent (H.HsDataDecl _ _ (H.HsIdent s) _ _ _) = H.HsEThingAll . H.UnQual $ H.HsIdent s
  mkEIdent decl = H.HsEVar . H.UnQual . H.HsIdent $ declname decl
  mtsigs = filter (not . null . nsOf) (reverse decls)
  corrn = drop 1 . dropWhile (/= '|') . drop 1 . dropWhile (/= '|')
  renameMap s = [(corrn s, takeWhile (/= '|') . drop 1 $ dropWhile (/= '|') s)]
  methcorrn (H.HsForeignImport a b c d (H.HsIdent s) f) = (H.HsForeignImport a b c d (H.HsIdent (corrn s)) f, renameMap s)
  methcorrn (H.HsTypeSig x [H.HsIdent s] y) = (H.HsTypeSig x [H.HsIdent (corrn s)] y, renameMap s)
  methcorrn (H.HsFunBind [H.HsMatch x (H.HsIdent s) y z t]) =
    (H.HsFunBind [H.HsMatch x (H.HsIdent (corrn s)) y z t], renameMap s)
  methcorrn (H.HsDataDecl a b (H.HsIdent s) c d e) = (H.HsDataDecl a b (H.HsIdent (corrn s)) c d e, renameMap s)
  methcorrn (H.HsInstDecl a b (H.UnQual (H.HsIdent s)) c d) = (H.HsInstDecl a b (H.UnQual (H.HsIdent (corrn s))) c d, renameMap s)
  methcorrn z = (z, [])

  nsOf x = case span (/= '|') (declname x) of
                (_, "") -> ""
                (ns, _) -> ns
  methassoc meth =
    let i = ns ++ nsOf meth
        ns = case headns of
          "" -> ""
          mns -> mns ++ "."
    in (i, methcorrn meth)
  methmap = mkmethmap M.empty (map methassoc mtsigs)
--  mkmethmap :: M.Map String (H.HsDecl, [(String, String)]) -> [(String, (H.HsDecl, [(String, String)]))] -> M.Map String (H.HsDecl, [(String, String)])
  mkmethmap m [] = m
  mkmethmap m ((i, meth) : ims) = mkmethmap addmeth ims where
    addmeth = case M.lookup i m of
      Nothing -> M.insert i [meth] m
      (Just meths) -> M.insert i (meth : meths) m
  submods :: [(H.HsModule, String -> Maybe String)]
  submods = M.elems $ M.mapWithKey mksubmod methmap
  mksubmod :: String -> [(H.HsDecl, [(String, String)])] -> (H.HsModule, String -> Maybe String)
  mksubmod iid smdecls =
    (H.HsModule nullLoc (H.Module iid) (Just subexp)
               -- (mkModImport modid : (imps ++ docimp))
               (map (mkModImport . H.Module) ([
                      "Prelude ((.), (==), (>>=), return, IO, Int, Float, Double, Bool(..), Maybe, maybe, fromIntegral, round, fmap, Show, Read, Eq, Ord)"
                    , "qualified Prelude (error)"
                    , "Data.Typeable (Typeable)"
                    , "GHCJS.Types (JSVal(..), JSString)"
                    , "GHCJS.Foreign (jsNull, jsUndefined)"
                    , "GHCJS.Foreign.Callback (syncCallback, asyncCallback, syncCallback1, asyncCallback1, syncCallback2, asyncCallback2, OnBlocked(..))"
                    , "GHCJS.Marshal (ToJSVal(..), FromJSVal(..))"
                    , "GHCJS.Marshal.Pure (PToJSVal(..), PFromJSVal(..))"
                    , "Control.Monad (void)"
                    , "Control.Monad.IO.Class (MonadIO(..))"
                    , "Data.Int (Int64)"
                    , "Data.Word (Word, Word64)"
                    , "Data.Maybe (fromJust)"
                    , "Data.Traversable (mapM)"
                    , "GHCJS.DOM.Types"
                    , "Control.Applicative ((<$>))"
                    ] ++ if name == "Enums"
                            then []
                            else eventImp iid ++ ["GHCJS.DOM.JSFFI.Generated.Enums"]))
               (H.HsFunBind [] : map fst smdecls), comment) where
      renameMap :: M.Map String String
      renameMap = M.fromList $ concatMap snd smdecls
      realName :: String -> String
      realName s = case M.lookup s renameMap of
                        Just "" -> ""
                        Just n  -> "." ++ n
                        Nothing -> ""
      comment :: String -> Maybe String
      comment n = do
        iname <- stripPrefix "GHCJS.DOM." iid
        return $ "\n-- | <https://developer.mozilla.org/en-US/docs/Web/API/"
                      ++ jsname iname ++ realName n ++ " Mozilla " ++ jsname iname ++ realName n ++ " documentation>"
      name = typeFor . reverse . takeWhile (/= '.') $ reverse iid
--      subexp = map mkEIdent . nub $ (filter (not . isSuffixOf "'") $ map declname smdecls) ++
      subexp = nub $ map (mkEIdent . fst) smdecls ++
                case name of
                    "Enums" -> []
                    _ | "Callback" `isSuffixOf` name || name == "MediaQueryListListener" || name == "NodeFilter" -> map (H.HsEVar . H.UnQual . H.HsIdent) (name : parentExp)
                    _ -> map (H.HsEVar . H.UnQual . H.HsIdent) ([name++"(..)", "gType" ++ name] ++ parentExp)
      parentExp | name `elem` allParents = ["Is" ++ name, "to" ++ name]
                | otherwise = []
      eventImp "GHCJS.DOM.Event" = []
      eventImp "GHCJS.DOM.UIEvent" = []
      eventImp "GHCJS.DOM.MouseEvent" = []
      eventImp "GHCJS.DOM.EventTarget" = []
      eventImp _ = ["GHCJS.DOM.EventTargetClosures (EventName, unsafeEventName)"]
      docimp = []
--      docimp = case "createElement" `elem` (map declname smdecls) of
--        True -> []
--        _ -> [(mkModImport (H.Module docmod))
--               {H.importSpecs = Just (False, [H.HsIVar $ H.HsIdent "createElement"])}] where
--          docmod = concat $ intersperse "." $ reverse
--                   ("Document" : tail (reverse $ parts (== '.') iid))


-- Loop through the list of toplevel parse results (modules, pragmas).
-- Pragmas modify state, modules don't.

domLoop :: DOMState -> [I.Defn] -> DOMState

domLoop st [] = st
domLoop st (def : defs) = case def of
  I.Pragma prgm -> domLoop (prgm2State st (dropWhile isSpace prgm)) defs
  I.Module id moddef ->
    let prmod = mod2mod st (I.Module id' moddef)
        modn = ns st ++
                 renameMod
                   (intercalate "." (reverse $ parts (== '.') (getDef def)))
        id' = I.Id modn
        imp' = modn : imp st
        modl = prmod : procmod st in
    domLoop st {procmod = modl, imp = imp'} defs
  z ->
    let logmsg = "Expected a Module or a Pragma; found " ++ show z in
    domLoop st {convlog = convlog st ++ [logmsg]} defs

-- Modify DOMState based on a pragma encountered

prgm2State :: DOMState -> String -> DOMState

prgm2State st ('n':'a':'m':'e':'s':'p':'a':'c':'e':nns) =
  let nnsst = read (dropWhile isSpace nns)
      dot = if null nnsst then "" else "." in
  st {ns = nnsst ++ dot}

prgm2State st upgm =
  let logmsg = "Unknown pragma " ++ upgm in
  st {convlog = convlog st ++ [logmsg]}

-- Module converter mutable state (kind of)

data DOMState = DOMState {
   pm :: M.Map String [Either String String] -- inheritance map
  ,imp :: [String]                           -- import list
  ,ns :: String                              -- output module namespace (#pragma namespace)
  ,procmod :: [H.HsModule]                   -- modules already processed
  ,convlog :: [String]                       -- conversion messages
} deriving (Show)

-- Convert an IDL module definition into Haskell module syntax

mod2mod :: DOMState -> I.Defn -> H.HsModule

mod2mod st md@(I.Module _ moddefs') =
  H.HsModule nullLoc (H.Module modid') (Just []) imps decls where
    moddefs = map fixDefn moddefs'
    enums = concatMap getEnums moddefs
    all = concatMap getAllInterfaces moddefs
    parents = nub $ concatMap getParents moddefs
    leaves = map typeFor $ filter (`notElem` parents) all
    isLeaf = flip elem leaves . typeFor
    modlst = ["Control.Monad"]
    modid' = renameMod $ getDef md
    imps = [] -- map mkModImport (map H.Module (modlst ++ imp st))
    intfs = filter intfAndCallbacks moddefs
    eqop op1 op2 = getDef op1 == getDef op2
    decls = nub $ types ++ classes ++ instances ++ methods ++ attrs ++ makers
    makers  = concatMap intf2maker intfs
    classes = concatMap intf2class intfs
    methods = concatMap (intf2meth enums isLeaf) intfs
    types = concatMap intf2type moddefs
    attrs = concatMap (intf2attr enums isLeaf) intfs
    instances = concatMap (intf2inst $ pm st) intfs

mod2mod _ z = error $ "Input of mod2mod should be a Module but is " ++ show z

-- Create a module import declaration

mkModImport :: H.Module -> H.HsImportDecl

mkModImport s = H.HsImportDecl {H.importLoc = nullLoc
                               ,H.importQualified = False
                               ,H.importModule = s
                               ,H.importAs = Nothing
                               ,H.importSpecs = Nothing}

-- For each interface, locate it in the inheritance map,
-- and produce instance declarations for the corresponding datatype.

intf2inst :: M.Map String [Either String String] -> I.Defn -> [H.HsDecl]

intf2inst pm intf@(I.Interface _ _ _ at _) = self : parents ++ eventTargetInst where
  sid = getDef intf
  self = mkInstDecl sid sid
  parents = case M.lookup sid pm of
    Nothing -> []
    Just ess -> map (flip mkInstDecl sid . either id id) ess
  eventTargetInst | I.ExtAttr (I.Id "EventTarget") [] `elem` at = [mkInstDecl "EventTarget" sid]
                  | otherwise = []

intf2inst _ _ = []

-- For each interface found, define a newtype with the same name

intf2type :: I.Defn -> [H.HsDecl]


--intf2type intf@(I.Interface _ _ _) =
--  let typename = H.HsIdent (typeFor $ getDef intf) in
--  [H.HsDataDecl nullLoc [] typename []
--    [H.HsConDecl nullLoc typename []] []]

intf2type (I.TypeDecl (I.TyEnum (Just (I.Id typename)) vals)) =
    [H.HsDataDecl nullLoc [] (H.HsIdent $ "Enums||" ++ typename) []
      (map constructor vals) (map (H.UnQual . H.HsIdent) ["Show", "Read", "Eq", "Ord", "Typeable"]),
     H.HsInstDecl nullLoc [] (H.UnQual $ H.HsIdent "Enums||PToJSVal")
        [H.HsTyCon . H.UnQual $ H.HsIdent typename]
        [H.HsFunBind (map ptoJsVal vals)],
     H.HsInstDecl nullLoc [] (H.UnQual $ H.HsIdent "Enums||ToJSVal")
        [H.HsTyCon . H.UnQual $ H.HsIdent typename]
        [H.HsFunBind
            [H.HsMatch nullLoc (H.HsIdent "toJSVal") []
                (H.HsUnGuardedRhs $ H.HsInfixApp (mkVar "return") (H.HsQVarOp $ mkSymbol ".") (mkVar "pToJSVal")) []
            ]
        ],
     H.HsInstDecl nullLoc [] (H.UnQual $ H.HsIdent "Enums||PFromJSVal")
        [H.HsTyCon . H.UnQual $ H.HsIdent typename]
        [H.HsFunBind (map pfromJsVal vals)],
     H.HsInstDecl nullLoc [] (H.UnQual $ H.HsIdent "Enums||FromJSVal")
        [H.HsTyCon . H.UnQual $ H.HsIdent typename]
        [H.HsFunBind
            [H.HsMatch nullLoc (H.HsIdent "fromJSValUnchecked") []
                (H.HsUnGuardedRhs $ H.HsInfixApp (mkVar "return") (H.HsQVarOp $ mkSymbol ".") (mkVar "pFromJSVal")) [],
             H.HsMatch nullLoc (H.HsIdent "fromJSVal") []
                (H.HsUnGuardedRhs $ H.HsInfixApp (mkVar "return") (H.HsQVarOp $ mkSymbol ".") (mkVar "pFromJSVal")) []
            ]
        ]
    ]
    ++ map jsffi vals
  where
    constructor (Right n, _, Nothing)  = H.HsConDecl nullLoc (H.HsIdent $ typename ++ conName n) []
    constructor val = error $ "Unhandled enum value " ++ show val
    jsffi (Right n, _, Nothing) = H.HsForeignImport nullLoc "javascript" H.HsUnsafe
        ("\"" ++ n ++ "\"")
        (H.HsIdent $ "Enums||js_" ++ typename ++ conName n)
        (mkTIdent "JSVal")
    jsffi val = error $ "Unhandled enum value " ++ show val
    ptoJsVal (Right n, _, Nothing) =
        H.HsMatch nullLoc (H.HsIdent "pToJSVal") [H.HsPVar . H.HsIdent $ typename ++ conName n]
            (H.HsUnGuardedRhs . mkVar $ "js_" ++ typename ++ conName n) []
    ptoJsVal val = error $ "Unhandled enum value " ++ show val
    pfromJsVal (Right n, _, Nothing) =
        H.HsMatch nullLoc (H.HsIdent "pFromJSVal") [H.HsPVar $ H.HsIdent "x"]
            (H.HsGuardedRhss [H.HsGuardedRhs nullLoc
                (H.HsInfixApp (mkVar "x") (H.HsQVarOp $ mkSymbol "`js_eq`") (mkVar $ "js_" ++ typename ++ conName n))
                (H.HsVar . H.UnQual . H.HsIdent $ typename ++ conName n)]) []
    pfromJsVal val = error $ "Unhandled enum value " ++ show val
    conName = concatMap conPart . splitOn "-"
    conPart (initial : rest) = toUpper initial : rest
    conPart [] = ""

intf2type _ = []

-- Convert an Interface specification into a class specification

intf2class :: I.Defn -> [H.HsDecl]

intf2class intf@(I.Interface _ supers _ _ _) =
  [H.HsClassDecl nullLoc sups (H.HsIdent (classFor $ getDef intf)) (take 1 azHIList) []] where
    sups = map (name2ctxt . jsname') supers

intf2class _ = []

-- For certain interfaces (ancestors of HTMLElement), special maker functions
-- are introduced to simplify creation of the formers.

intf2maker :: I.Defn -> [H.HsDecl]

--intf2maker intf@(I.Interface (I.Id iid) _ _) =
--  case (tagFor iid) of
--    "" -> []
--    tag -> [mktsig, mkimpl] where
--      mkimpl =
--        let defmaker = iid ++ "|mk" ++ renameMod tag
--            flipv = mkVar "flip"
--            crelv = mkVar "createElement"
--            exprv = mkVar "StringLit"
--            tags  = H.HsLit (H.HsString tag)
--            tagv  = H.HsApp (H.HsApp exprv tags) tags
--            rhs = H.HsUnGuardedRhs (H.HsApp crelv $ H.HsParen tagv)
--            match = H.HsMatch nullLoc (H.HsIdent defmaker) [] rhs [] in
--        H.HsFunBind [match]
--      mktsig =
--        let monadtv = mkTIdent "IO"
--            -- exprtv = mkTIdent "Expression"
--            defmaker = iid ++ "|mk" ++ renameMod tag
--            parms = [H.HsIdent "a"]
--            actx = (mkUIdent (classFor "HTMLDocument"),[mkTIdent "a"])
--            -- monadctx = (mkUIdent "Monad",[monadtv])
--            tpsig = mkTsig (map (H.HsTyVar) parms)
--                           (H.HsTyApp monadtv (mkTIdent (typeFor iid)))
--            retts = H.HsQualType (actx : []) tpsig in
--        H.HsTypeSig nullLoc [H.HsIdent defmaker] retts

intf2maker _ = []

-- Attributes are represented by methods with proper type signatures.
-- These methods are wrappers around type-neutral unsafe get/set property
-- functions.

intf2attr :: [String] -> (String -> Bool) -> I.Defn -> [H.HsDecl]

intf2attr enums isLeaf intf@(I.Interface (I.Id iid') _ cldefs _ _) =
  concatMap mkattr (collectAttrs intf) where
    iid = jsname' iid'
    mkattr (I.Attribute [] _ _ _ _) = []
    mkattr (I.Attribute [I.Id "URL"] _ _ _ _) | getDef intf `elem` ["EventSource", "WebSocket"] = [] -- The standard is the lowercase url
    mkattr (I.Attribute [I.Id iat] _ (I.TyName "EventListener" _) _ _) = mkevent iid iat
    mkattr (I.Attribute [I.Id iat] _ (I.TyName "EventHandler" _) _ _) = mkevent iid iat
    mkattr (I.Attribute [I.Id _] _ (I.TyName t _) _ _) | getDef intf `elem` ["Window", "WorkerGlobalScope"]
                                                         && "Constructor" `isSuffixOf` t = []
    mkattr (I.Attribute [I.Id iat] False tat raises ext) =
      (if I.ExtAttr (I.Id "Replaceable") [] `elem` ext
        then []
        else mksetter iid iat tat ext raises)
      ++ mkgetter iid iat tat ext raises
    mkattr (I.Attribute [I.Id iat] True  tat raises ext) = mkgetter iid iat tat ext raises
    mkattr (I.Attribute (iatt:iats) b tat raises ext) =
      mkattr (I.Attribute [iatt] b tat raises ext) ++ mkattr (I.Attribute iats b tat raises ext)
    mksetter iid iat tat' ext r = let tat = overrideAttributeType iid iat tat' in [sjsffi iid iat tat ext, stsig iid iat tat ext, simpl iid iat tat ext r]
    setf intf iat = U.toLowerInitCamel $ "set" ++ U.toUpperHead iat
    getf intf iat = U.toLowerInitCamel $ "get" ++ U.toUpperHead iat
    eventName iat = fromMaybe iat (stripPrefix "on" iat)
    eventf intf iat = U.toLowerInitCamel $ fixEventName (getDef intf) iat
    sjsffi iid iat tat ext =
      let monadtv = mkTIdent "IO"
          defop = iid ++ "|" ++ iat ++ "|" ++ "js_" ++ setf intf iat
          parm = [I.Param I.Required (I.Id "val") tat [I.Mode In] ext]
          parms = ffiTySelf intf : map (fst . tyParmFFI enums isLeaf) parm
          tpsig = mkTsig parms (H.HsTyApp monadtv $ H.HsTyCon (H.Special H.HsUnitCon))
          retts = H.HsQualType [] tpsig
          jsimpl = show . renderJs $ [jmacro| $1[`(iat)`] = $2 |]
          safe = if any (\case I.ExtAttr (I.Id str) _ -> "MayThrowException" `isSuffixOf` str;
                               _ -> False) ext
                   then H.HsSafe else H.HsUnsafe
      in H.HsForeignImport nullLoc "javascript" safe jsimpl (H.HsIdent defop) tpsig
    simpl iid iat tat ext raises =
      let defset = iid ++ "|" ++ iat ++ "|" ++ setf intf iat
          ffi = H.HsVar . H.UnQual . H.HsIdent $ "js_" ++ setf intf iat
          parms = [H.HsPVar $ H.HsIdent "self", H.HsPVar $ H.HsIdent "val"]
          call = applySelf isLeaf iid ffi
          val = I.Param I.Required (I.Id "val") tat [I.Mode In] ext
          rhs = H.HsUnGuardedRhs $ H.HsApp (mkVar "liftIO") . H.HsParen $ applyParam enums isLeaf val call
          match = H.HsMatch nullLoc (H.HsIdent defset) parms rhs [] in
      H.HsFunBind [match]
    stsig iid iat tat ext =
      let monadtv = mkTIdent "m"
          defset = iid ++ "|" ++ iat ++ "|" ++ setf intf iat
          parm = I.Param I.Required (I.Id "val") tat [I.Mode In] ext
          parms = [tySelf isLeaf iid, fst $ tyParm enums isLeaf parm]
          contxt = (mkUIdent "MonadIO", [mkTIdent "m"]) : ctxSelf isLeaf iid ++ snd (tyParm enums isLeaf parm)
          tpsig = mkTsig parms (H.HsTyApp monadtv $ H.HsTyCon (H.Special H.HsUnitCon))
          retts = H.HsQualType contxt tpsig in
      H.HsTypeSig nullLoc [H.HsIdent defset] retts
    mkgetter _ _ (I.TyName "NodeFilter" _) _ _ = []
    mkgetter _ _ (I.TyOptional (I.TyName "NodeFilter" _)) _ _ = []
    mkgetter iid iat tat' ext r =
        gjsffi : concatMap (\wrapType -> gtsig wrapType (rawReturn wrapType) ++ gimpl wrapType (rawReturn wrapType)) [Normal, Unsafe, Unchecked]
      where
        tat = overrideAttributeType iid iat tat'
        gjsffi =
          let monadtv = mkTIdent "IO"
              defop = iid ++ "|" ++ iat ++ "|" ++ "js_" ++ getf intf iat
              parms = [ffiTySelf intf]
              tpsig = mkTsig parms (H.HsTyApp monadtv $ fromJust $ tyRet enums True tat ext Normal)
              retts = H.HsQualType [] tpsig
              promise = case tat of
                            (I.TyPromise _) -> True
                            _ -> False
              jsimpl = jsReturn tat $ [jmacroE| $1[`(iat)`] |]
              safe = if any (\case I.ExtAttr (I.Id str) _ -> "MayThrowException" `isSuffixOf` str;
                                   _ -> False) ext
                       then H.HsSafe else H.HsUnsafe
          in H.HsForeignImport nullLoc (if promise then "javascript interruptible" else "javascript") safe jsimpl (H.HsIdent defop) tpsig
        rawReturn = tyRet enums False tat ext
        gimpl _ Nothing = []
        gimpl wrapType (Just retType) =
          let defget = iid ++ "|" ++ iat ++ "|" ++ wrapName wrapType (getf intf iat)
              ffi = H.HsVar . H.UnQual . H.HsIdent $ "js_" ++ getf intf iat
              parm = H.HsPVar $ H.HsIdent "self"
              call = applySelf isLeaf iid ffi
              rhs = H.HsUnGuardedRhs $ H.HsApp (mkVar "liftIO") . H.HsParen $ returnType enums tat ext wrapType call
              match = H.HsMatch nullLoc (H.HsIdent defget) [parm] rhs [] in
          [H.HsFunBind [match]]
        gtsig _ Nothing = []
        gtsig wrapType (Just retType) =
          let monadtv = mkTIdent "m"
              defget = iid ++ "|" ++ iat ++ "|" ++ wrapName wrapType (getf intf iat)
              parms = [tySelf isLeaf iid]
              contxt = (mkUIdent "MonadIO", [mkTIdent "m"]) : ctxSelf isLeaf iid ++ ctxRet wrapType tat
              tpsig = mkTsig parms (H.HsTyApp monadtv retType)
              retts = H.HsQualType contxt tpsig in
          [H.HsTypeSig nullLoc [H.HsIdent defget] retts]
    mkevent iid iat = [eventtsig iid iat, eventimpl iid iat]
    eventimpl iid iat =
      let defget = iid ++ "|" ++ iat ++ "|" ++ eventf intf iat
          rhs = H.HsUnGuardedRhs $
                H.HsApp
                    (mkVar "unsafeEventName")
                    (H.HsParen
                        (H.HsApp
                            (mkVar "toJSString")
                            (H.HsLit (H.HsString (eventName iat)))
                        )
                    )
          match = H.HsMatch nullLoc (H.HsIdent defget) [] rhs [] in
      H.HsFunBind [match]
    eventtsig iid iat =
      let defget = iid ++ "|" ++ iat ++ "|" ++ eventf intf iat
          contxt' = ctxSelf isLeaf iid
          contxt | null contxt' = []
                 | otherwise = contxt' ++ [(mkUIdent "IsEventTarget",[mkTIdent "self"])]
          tpsig = mkTsig [] $ eventTyRet isLeaf (getDef intf) iat
          retts = H.HsQualType contxt tpsig in
      H.HsTypeSig nullLoc [H.HsIdent defget] retts
--    gtcnc iid iat tat =
--      let defcnc = iid ++ "|getm'" ++ iat
--          parms = [H.HsIdent "this"]
--          thisctx = (mkUIdent (classFor iid),[mkTIdent "this"])
--          tpsig = mkTsig (map (H.HsTyApp exprtv . H.HsTyVar) parms)
--                         (H.HsTyApp monadtv $ H.HsTyApp exprtv (cnRet tat))
--          retts = H.HsQualType [thisctx] tpsig in
--      H.HsTypeSig nullLoc [H.HsIdent defcnc] retts
--    eqcnc iid iat =
--      let defcnc = iid ++ "|getm'" ++ iat
--          defget = "get'" ++ iat
--          rhs = H.HsUnGuardedRhs (mkVar defget)
--          match = H.HsMatch nullLoc (H.HsIdent defcnc) [] rhs [] in
--      H.HsFunBind [match]



intf2attr _ _ _ = []

-- Create a Javascript body for a getter. Template for a getter is:
-- get'prop this = do
--   let et = undefined :: zz
--       r = DotRef et (this /\ et) (Id et "propname")
--   return r
-- where zz is a type variable or type name of the method return type.

mkGetter :: String -> H.HsPat -> H.HsType -> H.HsExp

mkGetter prop arg rett = H.HsDo [let1, let2, ret] where
  let1 = H.HsLetStmt [
           H.HsFunBind [
             H.HsMatch nullLoc
                       (H.HsIdent "et")
                       []
                       (H.HsUnGuardedRhs $ H.HsExpTypeSig nullLoc
                                                          (mkVar "undefined")
                                                          (H.HsQualType [] rett))
                       []
            ]
          ]
  let2 = H.HsLetStmt [
           H.HsFunBind [
             H.HsMatch nullLoc
                       (H.HsIdent "r")
                       []
                       (H.HsUnGuardedRhs $ H.HsApp (
                                             H.HsApp (
                                               H.HsApp (mkVar "DotRef")
                                                       (mkVar "et"))
                                               (H.HsParen $
                                                  H.HsInfixApp (mkVar "thisp")
                                                               (H.HsQVarOp $ mkSymbol "/\\")
                                                               (mkVar "et")))
                                             (H.HsParen $
                                                H.HsApp (
                                                  H.HsApp (mkVar "Id")
                                                          (mkVar "et"))
                                                  (H.HsLit $ H.HsString prop)))
                       []
             ]
           ]
  ret = H.HsQualifier $
          H.HsApp (mkVar "return") (mkVar "r")

-- Methods are lifted to top level. Declared argument types are converted
-- into type constraints unless they are of primitive types. First argument
-- always gets a type of the interface where the method is declared.
-- Only `In' parameters are supported at this time. The "this" argument
-- goes last to make monadic composition of actions easier.

callbackMethods :: [String] -> (String -> Bool) -> I.Defn -> I.Defn -> [H.HsDecl]
callbackMethods enums isLeaf intf (I.Operation (I.FunId _ _ parm) resultType _ _ _) =
    [f callbackType | callbackType <- [SyncContinueAsync, SyncThrowWouldBlock, Async], f <- [tsig, timpl]]
  where
    defop callbackType = getDef intf ++ "||new" ++ getDef intf ++ callbackPostfix callbackType
    tsig callbackType =
      let monadtv = mkTIdent "m"
          cparm _ (I.TyApply (I.TySigned False) (I.TyInteger LongLong)) = mkTIdent "Double"
          cparm pname pType = tyRet' pname enums False pType []
          cparms = [cparm pname pType | I.Param _ (I.Id pname) pType _ _ <- parm]
          cbfunc = [mkTsig cparms (H.HsTyApp (mkTIdent "IO") (mkTIdent "()"))] -- Callbacks can't return values for now (fst $ tyParm enums isLeaf resultParam))]
--          parms | sync && withArgs = mkTIdent "OnBlocked":cbfunc
--                | otherwise = cbfunc
          parms = cbfunc
          contxt = (mkUIdent "MonadIO", [mkTIdent "m"]) : concat [ctxRet' pname pType | I.Param _ (I.Id pname) pType _ _ <- {-resultParam :-} parm]
          resultParam = I.Param I.Required (I.Id "result") resultType [I.Mode In] []
          retType = case getDef intf of
                        "NotificationPermissionCallback" -> H.HsTyApp (mkTIdent (typeFor $ getDef intf)) (mkTIdent "permission")
                        "StringCallback" -> H.HsTyApp (mkTIdent (typeFor $ getDef intf)) (mkTIdent "data'")
                        _ -> mkTIdent (typeFor $ getDef intf)
          tpsig = mkTsig parms (H.HsTyApp monadtv retType)
          retts = H.HsQualType contxt tpsig in
      H.HsTypeSig nullLoc [H.HsIdent (defop callbackType)] retts
    timpl callbackType =
      let cparms = map (H.HsPVar . H.HsIdent . paramName) parm
          cbfunc = [H.HsPVar $ H.HsIdent "callback"]
--          parms | sync && withArgs = H.HsPVar (H.HsIdent "onBlocked"):cbfunc
--                | otherwise = cbfunc
          parms = cbfunc
          castReturn = H.HsInfixApp (
                          H.HsInfixApp
                            (mkVar $ getDef intf)
                            (H.HsQVarOp (mkSymbol "."))
                            (mkVar "pToJSVal")
                          )
                          (H.HsQVarOp (mkSymbol "<$>"))
          callbackN | null parm = ""
                    | otherwise = show (length parm)
          lambda | null parm = mkVar "callback"
                 | otherwise = H.HsParen (H.HsLambda nullLoc cparms (L.foldl applyCParam (mkVar "callback") parm))
          call SyncContinueAsync   = H.HsApp (H.HsApp (mkVar $ "syncCallback" ++ callbackN) (mkVar "ThrowWouldBlock")) lambda
          call SyncThrowWouldBlock = H.HsApp (H.HsApp (mkVar $ "syncCallback" ++ callbackN) (mkVar "ContinueAsync")) lambda
          call Async               = H.HsApp (mkVar $ "asyncCallback" ++ callbackN) lambda
          rhs = H.HsUnGuardedRhs $ H.HsApp (mkVar "liftIO") $ H.HsParen (
                H.HsInfixApp
                    (mkVar $ typeFor $ getDef intf)
                    (H.HsQVarOp (mkSymbol "<$>"))
                    (call callbackType))
          match  = H.HsMatch nullLoc (H.HsIdent (defop callbackType)) parms rhs []
          applyCParam e param =
            H.HsInfixApp
              (H.HsApp
                (mkVar "fromJSValUnchecked")
                (mkVar (paramName param))
              )
              (H.HsQVarOp (mkSymbol ">>="))
              (H.HsLambda nullLoc [H.HsPVar (H.HsIdent $ paramName param ++ "'")]
                (H.HsApp e (mkVar $ paramName param ++ "'"))
              )
      in  H.HsFunBind [match]
callbackMethods _ _ _ _ = []

intf2meth :: [String] -> (String -> Bool) -> I.Defn -> [H.HsDecl]
intf2meth enums isLeaf intf@(I.Interface _ _ cldefs at mbCB) =
  concatMap mkconstructor constructors ++
  (case mbCB of
    (Just (I.Id "callback")) -> concatMap (callbackMethods enums isLeaf intf) cldefs
    _ -> concatMap mkmeth (collectOps intf)) ++
  concatMap mkconst (collectConst intf) where
    getDefHs = getDef
    getDefJs op@(I.Operation _ _ _ mbctx _) = case mbctx of
      Nothing -> getDef op
      Just [] -> getDef op
      Just (s:_) -> s
    constructors = zip (map (`replicate` '\'') [0..]) $ reverse [parm | x@(I.ExtAttr (I.Id name) parm) <- at, name `elem` ["Constructor", "CustomConstructor"]]
    mkconstructor c = constructorjsffi c : constructortsig c : constructortimpl c
    constructorRaises | I.ExtAttr (I.Id "ConstructorRaisesException") [] `elem` at = [I.Raises ["RaisesException"]]
                      | otherwise = []
    constructorjsffi (postfix, parm) =
      let monadtv = mkTIdent "IO"
          defop = getDef intf ++ "||" ++ jsffiConstructorName ++ postfix
          parms = map (fst . tyParmFFI enums isLeaf) parm
          tpsig = mkTsig parms (H.HsTyApp monadtv (ffiTySelf intf))
          retts = H.HsQualType [] tpsig
          jsimpl = case parm of
                    [I.Param _ _ (I.TyName "DOMString..." _) _ _] ->
                        error "Unexpected constuctor parameter DOMString..." -- show . renderJs $ [jmacroE| window[`(jsname (getDef intf))`].apply(window, $1) |]
                    _ ->
                        show . renderJs $ ApplExpr (callNew (getDef intf))
                            (map (\(n, _) -> jsv $ '$':show n) $ zip [1..] parm)
          safe = if any (\case I.ExtAttr (I.Id str) _ -> "MayThrowException" `isSuffixOf` str;
                               _ -> False) at
                   then H.HsSafe else H.HsUnsafe
       in H.HsForeignImport nullLoc "javascript" safe jsimpl (H.HsIdent defop) tpsig
    constructortsig (postfix, parm) =
      let monadtv = mkTIdent "m"
          defop = getDef intf ++ "||" ++ constructorName ++ postfix
          parms = map (fst . tyParm enums isLeaf) parm
          contxt = (mkUIdent "MonadIO", [mkTIdent "m"]) : concatMap (snd . tyParm enums isLeaf) parm
          tpsig = mkTsig parms (H.HsTyApp monadtv (mkTIdent (typeFor $ getDef intf)))
          retts = H.HsQualType contxt tpsig in
      H.HsTypeSig nullLoc [H.HsIdent defop] retts
    constructortimpl (postfix, parm) =
      let defop = getDef intf ++ "||" ++ constructorName ++ postfix
          ffi = H.HsVar . H.UnQual . H.HsIdent $ jsffiConstructorName ++ postfix
          parms = map (H.HsPVar . H.HsIdent . paramName) parm
          call = ffi
          rhs = H.HsUnGuardedRhs $ H.HsApp (mkVar "liftIO") . H.HsParen $
                  L.foldl (flip $ applyParam enums isLeaf) call parm
          match  = H.HsMatch nullLoc (H.HsIdent defop) parms rhs []
      in  [H.HsFunBind [match]]
    mkconst cn@(I.Constant (I.Id cid) _ _ (I.Lit (IntegerLit (ILit base val)))) =
      let defcn = getDef intf ++ "||pattern " ++ cid
          match = H.HsMatch nullLoc (H.HsIdent defcn) [] crhs []
          crhs = H.HsUnGuardedRhs (H.HsLit (H.HsInt val))
      in  [H.HsFunBind [match]]
--    mkmeth (I.Operation _ _ _ _ ext) | I.ExtAttr (I.Id "V8EnabledAtRuntime") [] `elem` ext = []
    mkmeth (I.Operation _ _ _ _ exAttr) | not (visible exAttr) = []
    mkmeth (I.Operation (I.FunId (I.Id "handleEvent") _ _) _ _ _ exAttr) | getDef intf `notElem` ["EventListener", "EventHandler"] = error $ "Unexpected handleEvent function in " ++ show intf
    mkmeth op | skip op = []
    mkmeth op@(I.Operation (I.FunId _ _ parm) optype' raises _ ext) =
        jsffi : concatMap (\wrapType -> tsig wrapType (rawReturn wrapType) ++ timpl wrapType (rawReturn wrapType)) [Normal, Underscore, Unsafe, Unchecked]
      where
        optype = overrideReturnType (getDef intf) (name op parm) optype'
        isGetter = I.ExtAttr (I.Id "Getter") [] `elem` ext
        jsffi =
          let monadtv = mkTIdent "IO"
              defop = getDef intf ++ "|" ++ rawName op ++ "|" ++ jsffiName op parm
              parms = ffiTySelf intf : map (fst . tyParmFFI enums isLeaf) parm
              tpsig = mkTsig parms (H.HsTyApp monadtv $ fromJust $ tyRet enums True optype ext Normal)
              retts = H.HsQualType [] tpsig
              promise = case optype of
                            (I.TyPromise _) -> True
                            _ -> False
              jsimpl = case parm of
                        [I.Param _ _ (I.TyName "DOMString..." _) _ _] ->
                            jsReturn optype $ [jmacroE| $1[`(getDef op)`].apply($1, $2) |]
                        [p] | isGetter ->
                            jsReturn optype [jmacroE| $1[$2] |]
                        _ ->
                            jsReturn optype $ ApplExpr [jmacroE| $1[`(getDef op)`] |]
                                (map (\(n, _) -> jsv $ '$':show n) $ zip [2..] parm)
              safe = if any (\case I.ExtAttr (I.Id str) _ -> "MayThrowException" `isSuffixOf` str;
                                   _ -> False) ext
                       then H.HsSafe else H.HsUnsafe
          in H.HsForeignImport nullLoc (if promise then "javascript interruptible" else "javascript") safe jsimpl (H.HsIdent defop) tpsig
        rawReturn = tyRet enums False optype ext
        tsig _ Nothing = []
        tsig wrapType (Just retType) =
          let monadtv = mkTIdent "m"
              -- exprtv = mkTIdent "Expression"
              defop = getDef intf ++ "|" ++ rawName op ++ "|" ++ wrapName wrapType (name op parm)
              parms = tySelf isLeaf (getDef intf) : map (fst . tyParm enums isLeaf) parm
              contxt = (mkUIdent "MonadIO", [mkTIdent "m"]) : ctxSelf isLeaf (getDef intf) ++ concatMap (snd . tyParm enums isLeaf) parm ++ ctxRet wrapType optype
              -- monadctx = (mkUIdent "Monad",[monadtv])
              tpsig = mkTsig parms (H.HsTyApp monadtv retType)
              retts = H.HsQualType contxt tpsig in
          [H.HsTypeSig nullLoc [H.HsIdent defop] retts]
        timpl _ Nothing = []
        timpl wrapType (Just retType) =
          let defop = getDef intf ++ "|" ++ rawName op ++ "|" ++ wrapName wrapType (name op parm)
              ffi = H.HsVar . H.UnQual . H.HsIdent $ jsffiName op parm
              parms = map (H.HsPVar . H.HsIdent) ("self" : map paramName parm)
              call = applySelf isLeaf (getDef intf) ffi
              -- params' = map (\I.Param (I.Id "val") tat [I.Mode In] parm
              rhs = H.HsUnGuardedRhs $ H.HsApp (mkVar "liftIO") . H.HsParen $ returnType enums optype ext wrapType $ L.foldl (flip $ applyParam enums isLeaf) call parm
    --          rhs = H.HsUnGuardedRhs $ mkMethod (getDefJs op) parms (tyRet optype)
              match  = H.HsMatch nullLoc (H.HsIdent defop) parms rhs []
          in  [H.HsFunBind [match]]
    constructorName = "new" ++ getDef intf
    rawName = getDefHs
    name op parm = rawName op ++ disambiguate (getDef intf) (rawName op) parm
--    jsffiName op parm = name op parm ++ "'"
    jsffiConstructorName = "js_new" ++ getDef intf
    jsffiName op parm = "js_" ++ name op parm
    -- Only EventTarget needs these (the rest use an IsEventTarget to get them)
    skip (I.Operation (I.FunId (I.Id "addEventListener") _ _) _ _ _ _) = getDef intf /= "EventTarget"
    skip (I.Operation (I.FunId (I.Id "removeEventListener") _ _) _ _ _ _) = getDef intf /= "EventTarget"
    skip (I.Operation (I.FunId (I.Id "dispatchEvent") _ _) _ _ _ _) = getDef intf /= "EventTarget"
    skip op@(I.Operation (I.FunId _ _ parm) _ _ _ _) = any excludedParam parm || exclude (getDef intf) (rawName op) parm
    excludedParam _ = False
    canvasPathFunctionNames = [
        "fill",
        "stroke",
        "clip",
        "isPointInPath",
        "isPointInStroke",
        "drawFocusIfNeeded"]

intf2meth _ _ _ = []

-- Create a Javascript body for a method. Template for a method is:
-- method a1 ... an this = do
--   let et = undefined :: zz
--       r = DotRef et (this /\ et) (Id et "methodname")
--   return (CallExpr et r [a1 /\ et, ... an /\ et]
-- where zz is a type variable or type name of the method return type.

mkMethod :: String -> [H.HsPat] -> H.HsType -> H.HsExp

mkMethod meth args rett = H.HsDo [let1, let2, ret] where
  args' = init args
  cast ts (H.HsPVar (H.HsIdent hn)) =
    H.HsInfixApp (mkVar hn) (H.HsQVarOp $ mkSymbol "/\\") (mkVar ts)
  let1 = H.HsLetStmt [
           H.HsFunBind [
             H.HsMatch nullLoc
                       (H.HsIdent "et")
                       []
                       (H.HsUnGuardedRhs $ H.HsExpTypeSig nullLoc
                                                          (mkVar "undefined")
                                                          (H.HsQualType [] rett))
                       []
            ]
          ]
  let2 = H.HsLetStmt [
           H.HsFunBind [
             H.HsMatch nullLoc
                       (H.HsIdent "r")
                       []
                       (H.HsUnGuardedRhs $ H.HsApp (
                                             H.HsApp (
                                               H.HsApp (mkVar "DotRef")
                                                       (mkVar "et"))
                                               (H.HsParen $
                                                  H.HsInfixApp (mkVar "thisp")
                                                               (H.HsQVarOp $ mkSymbol "/\\")
                                                               (mkVar "et")))
                                             (H.HsParen $
                                                H.HsApp (
                                                  H.HsApp (mkVar "Id")
                                                          (mkVar "et"))
                                                  (H.HsLit $ H.HsString meth)))
                       []
             ]
           ]
  ret = H.HsQualifier $
          H.HsApp (mkVar "return")
                  (H.HsParen $
                     H.HsApp (
                       H.HsApp (
                         H.HsApp (mkVar "CallExpr")
                                 (mkVar "et"))
                         (mkVar "r"))
                       (H.HsList $ map (cast "et") args'))


-- Apply a parameter to a FFI call

applySelf isLeaf iid call | isLeaf iid = H.HsApp call . H.HsVar . H.UnQual $ H.HsIdent "self"
                          | otherwise = H.HsApp call . H.HsParen $
                                                    H.HsApp
                                                        (H.HsVar . H.UnQual . H.HsIdent $ "to" ++ typeFor iid)
                                                        (H.HsVar . H.UnQual $ H.HsIdent "self")

applyParam :: [String] -> (String -> Bool) -> I.Param -> H.HsExp -> H.HsExp

applyParam enums isLeaf param@(I.Param optional (I.Id p) ptype [I.Mode In] ext) call = lookup ptype where
  pname = mkVar $ paramName param
  canBeNull = optional == I.Optional && canBeMaybe ptype
  isMaybeType = case tyParm enums isLeaf param of
                    (H.HsTyApp (H.HsTyVar (H.HsIdent "Maybe")) _, _) -> True
                    _ -> False
  toOptional p =
          H.HsApp
            call
            (H.HsParen
              (H.HsApp
                (mkVar "maybeToOptional")
                p
              )
            )
  defaultCall p
    | isMaybeType = toOptional p
    | otherwise = H.HsApp call p

  lookup ptype =
   case ptype of
    I.TyOptional t@(I.TyName c Nothing) | isNothing (asIs [] (const False) False c) -> lookup t
    I.TyOptional (I.TySequence t _) ->
        H.HsInfixApp
          (H.HsApp
            (mkVar "toJSVal")
            (mkVar (paramName param))
          )
          (H.HsQVarOp (mkSymbol ">>="))
          (H.HsLambda nullLoc [H.HsPVar (H.HsIdent $ paramName param ++ "'")]
            (H.HsApp call (mkVar $ paramName param ++ "'"))
          )
    t | isOptionalStringType t || (isStringType t && canBeNull) -> H.HsApp call (H.HsParen $ H.HsApp (mkVar "toOptionalJSString") pname)
      | isStringType t -> H.HsApp call (H.HsParen $ H.HsApp (mkVar "toJSString") pname)
    I.TyOptional t -> lookup t
    I.TySequence t _ ->
        H.HsInfixApp
          (H.HsApp
            (mkVar "toJSVal")
            (mkVar (paramName param))
          )
          (H.HsQVarOp (mkSymbol ">>="))
          (H.HsLambda nullLoc [H.HsPVar (H.HsIdent $ paramName param ++ "'")]
            (defaultCall (mkVar $ paramName param ++ "'"))
          )
    I.TySafeArray t ->
        H.HsInfixApp
          (H.HsApp
            (mkVar "toJSVal")
            (mkVar (paramName param))
          )
          (H.HsQVarOp (mkSymbol ">>="))
          (H.HsLambda nullLoc [H.HsPVar (H.HsIdent $ paramName param ++ "'")]
            (defaultCall (mkVar $ paramName param ++ "'"))
          )
    I.TyFrozenArray t ->
        H.HsInfixApp
          (H.HsApp
            (mkVar "toJSVal")
            (mkVar (paramName param))
          )
          (H.HsQVarOp (mkSymbol ">>="))
          (H.HsLambda nullLoc [H.HsPVar (H.HsIdent $ paramName param ++ "'")]
            (defaultCall (mkVar $ paramName param ++ "'"))
          )
--    I.TyName "DOMString" Nothing | optional == I.Optional ->
--      H.HsApp
--        call
--        (H.HsParen
--          (H.HsApp
--            (H.HsApp
--              (H.HsApp
--                (mkVar "maybe")
--                (mkVar "jsNull")
--              )
--              (mkVar "toJSString")
--            )
--            pname
--          )
--        )
    I.TyName "DOMString..." Nothing ->
        H.HsInfixApp
          (H.HsApp
            (mkVar "toJSVal")
            (mkVar (paramName param))
          )
          (H.HsQVarOp (mkSymbol ">>="))
          (H.HsLambda nullLoc [H.HsPVar (H.HsIdent $ paramName param ++ "'")]
            (defaultCall (mkVar $ paramName param ++ "'"))
          )
    I.TyName "DOMTimeStamp" Nothing -> defaultCall pname
    I.TyName "CompareHow" Nothing -> defaultCall pname
    I.TyName "GLintptr" Nothing -> defaultCall (H.HsParen $ H.HsApp (mkVar "fromIntegral") pname)
    I.TyName "GLsizeiptr" Nothing -> defaultCall (H.HsParen $ H.HsApp (mkVar "fromIntegral") pname)
    I.TyName "GLint64" Nothing -> defaultCall (H.HsParen $ H.HsApp (mkVar "fromIntegral") pname)
    I.TyName "GLuint64" Nothing -> defaultCall (H.HsParen $ H.HsApp (mkVar "fromIntegral") pname)
    I.TyName x Nothing | x `elem` glTypes -> defaultCall pname
    I.TyName "Bool" Nothing -> defaultCall pname
    I.TyName "boolean" Nothing -> defaultCall pname
    I.TyName "boolen" Nothing -> defaultCall pname
    I.TyName x Nothing | x `elem` ("IceTransportState" : "IceGatheringState" : "RtpTransceiverDirection" : enums)  ->
      if isMaybeType
        then toOptional pname
        else
          H.HsApp
            call
            (H.HsParen
              (H.HsApp
                (mkVar "pToJSVal")
                pname
              )
            )
    I.TyName x Nothing | isLeaf x -> defaultCall pname
                       | not (hasTo x) ->
                            H.HsInfixApp
                              (H.HsApp
                                (mkVar "toJSVal")
                                (mkVar (paramName param))
                              )
                              (H.HsQVarOp (mkSymbol ">>="))
                              (H.HsLambda nullLoc [H.HsPVar (H.HsIdent $ paramName param ++ "'")]
                                (defaultCall
                                  (H.HsParen
                                    (H.HsApp
                                      (mkVar $ typeFor x)
                                      (mkVar $ paramName param ++ "'")
                                    )
                                  )
                                )
                              )
                       | isMaybeType -> defaultCall
                                            (H.HsParen
                                              (H.HsApp
                                                (H.HsApp
                                                  (mkVar $ "fmap")
                                                  (mkVar $ "to" ++ typeFor x)
                                                )
                                                pname
                                              )
                                            )
                       | otherwise -> defaultCall
                                            (H.HsParen
                                              (H.HsApp
                                                (mkVar $ "to" ++ typeFor x)
                                                pname
                                              )
                                            )
    I.TyInteger LongLong
        | isMaybeType -> defaultCall (H.HsParen $ H.HsApp (H.HsApp (mkVar "fmap") (mkVar "fromIntegral")) pname)
        | otherwise -> defaultCall (H.HsParen $ H.HsApp (mkVar "fromIntegral") pname)
    I.TyInteger _ -> defaultCall pname
    I.TyFloat _   -> defaultCall pname
    I.TyApply _ (I.TyInteger LongLong)
        | isMaybeType -> defaultCall (H.HsParen $ H.HsApp (H.HsApp (mkVar "fmap") (mkVar "fromIntegral")) pname)
        | otherwise -> defaultCall (H.HsParen $ H.HsApp (mkVar "fromIntegral") pname)
    I.TyApply _ (I.TyInteger _) -> defaultCall pname
    I.TyAny
        | isMaybeType ->
            H.HsInfixApp
              (H.HsApp
                (H.HsApp
                  (mkVar "mapM")
                  (mkVar "toJSVal")
                )
                (mkVar (paramName param))
              )
              (H.HsQVarOp (mkSymbol ">>="))
              (H.HsLambda nullLoc [H.HsPVar (H.HsIdent $ paramName param ++ "'")]
                (defaultCall (mkVar $ paramName param ++ "'"))
              )
        | otherwise ->
            H.HsInfixApp
              (H.HsApp
                (mkVar "toJSVal")
                (mkVar (paramName param))
              )
              (H.HsQVarOp (mkSymbol ">>="))
              (H.HsLambda nullLoc [H.HsPVar (H.HsIdent $ paramName param ++ "'")]
                (defaultCall (mkVar $ paramName param ++ "'"))
              )
    I.TySum t
        | isMaybeType ->
            H.HsInfixApp
              (H.HsApp
                (H.HsApp
                  (mkVar "mapM")
                  (mkVar "toJSVal")
                )
                (mkVar (paramName param))
              )
              (H.HsQVarOp (mkSymbol ">>="))
              (H.HsLambda nullLoc [H.HsPVar (H.HsIdent $ paramName param ++ "'")]
                (defaultCall
                  (H.HsParen
                    (H.HsApp
                      (H.HsApp
                        (mkVar "fmap")
                        (mkVar (sumType t))
                      )
                      (mkVar $ paramName param ++ "'")
                    )
                  )
                )
              )
        | otherwise ->
            H.HsInfixApp
              (H.HsApp
                (mkVar "toJSVal")
                (mkVar (paramName param))
              )
              (H.HsQVarOp (mkSymbol ">>="))
              (H.HsLambda nullLoc [H.HsPVar (H.HsIdent $ paramName param ++ "'")]
                (defaultCall (H.HsParen (H.HsApp (mkVar (sumType t)) (mkVar $ paramName param ++ "'"))))
              )
    t -> error $ "Apply Param type " ++ show t

applyParam _ _ param@I.Param{} _ = error $ "Unsupported parameter attributes " ++ show param

returnType :: [String] ->  I.Type -> [I.ExtAttribute] -> WrapType -> H.HsExp -> H.HsExp
returnType _ _ _ Underscore e =
        H.HsApp
          (mkVar "void")
          (H.HsParen e)
returnType enums t ext Unsafe e =
        H.HsInfixApp
          (H.HsParen (returnType enums t ext Normal e))
          (H.HsQVarOp (mkSymbol ">>="))
          (H.HsApp
            (H.HsApp
              (mkVar "maybe")
              (H.HsParen (H.HsApp (mkVar "Prelude.error") (H.HsLit $ H.HsString "Nothing to return"))))
              (mkVar "return"))
returnType _ t ext wrapType e
    | isOptionalStringType t = H.HsApp (H.HsApp (
            (if wrapType == Unchecked
                then H.HsInfixApp (mkVar "fromJust") (H.HsQVarOp (mkSymbol "."))
                else id)
                    (mkVar "fromMaybeJSString")) (mkVar "<$>")) (H.HsParen e)
    | isStringType t = H.HsApp (H.HsApp (mkVar "fromJSString") (mkVar "<$>")) (H.HsParen e)
--returnType (I.TySafeArray (I.TyName "DOMString" Nothing)) _ e = H.HsApp (H.HsApp (mkVar "fromJSVal") (mkVar "<$>")) (H.HsParen e)
--returnType (I.TySequence (I.TyName "DOMString" Nothing) _) _ e = H.HsApp (H.HsApp (mkVar "fromJSVal") (mkVar "<$>")) (H.HsParen e)
returnType _ (I.TyName "DOMTimeStamp" Nothing) _ _ e = e
returnType _ (I.TyName "CompareHow" Nothing) _ _ e = e
returnType _ (I.TyName "GLintptr" Nothing) _ _ e = H.HsApp (H.HsApp (mkVar "round") (mkVar "<$>")) (H.HsParen e)
returnType _ (I.TyName "GLsizeiptr" Nothing) _ _ e = H.HsApp (H.HsApp (mkVar "round") (mkVar "<$>")) (H.HsParen e)
returnType _ (I.TyName "GLint64" Nothing) _ _ e = H.HsApp (H.HsApp (mkVar "round") (mkVar "<$>")) (H.HsParen e)
returnType _ (I.TyName "GLuint64" Nothing) _ _ e = H.HsApp (H.HsApp (mkVar "round") (mkVar "<$>")) (H.HsParen e)
returnType _ (I.TyName x Nothing) _ _ e | x `elem` glTypes = e
returnType _(I.TyName "Bool" Nothing) _ _ e = e
returnType enums (I.TyName x Nothing) _ _ e | x `elem` ("IceTransportState" : "IceGatheringState" : "RtpTransceiverDirection" : enums) || x == "PerformanceEntryList" =
        H.HsInfixApp
          (H.HsParen e)
          (H.HsQVarOp (mkSymbol ">>="))
          (mkVar "fromJSValUnchecked")
returnType enums t@(I.TyName x Nothing) _ wrapType e = e
--        H.HsInfixApp
--          (case tyRet enums False t [] wrapType of
--            Just (H.HsTyApp (H.HsTyVar (H.HsIdent "Maybe")) _) -> mkVar "nullableToMaybe"
--            Just _ -> H.HsInfixApp (mkVar "fromJust") (H.HsQVarOp (mkSymbol ".")) (mkVar "nullableToMaybe")
--            _ -> error "Issue with wrapType?")
--          (H.HsQVarOp (mkSymbol "<$>"))
--          (H.HsParen e)
returnType _ (I.TyInteger LongLong) _ _ e = H.HsApp (H.HsApp (mkVar "round") (mkVar "<$>")) (H.HsParen e)
--returnType (I.TyFloat _) _ _ e = H.HsApp (H.HsApp (mkVar "realToFrac") (mkVar "<$>")) (H.HsParen e)
returnType _ (I.TyApply _ (I.TyInteger LongLong)) _ _ e = H.HsApp (H.HsApp (mkVar "round") (mkVar "<$>")) (H.HsParen e)
returnType _ (I.TyOptional (I.TyInteger LongLong)) _ Unchecked e =
    H.HsApp (H.HsApp (H.HsApp (H.HsApp (H.HsInfixApp (mkVar "round") (H.HsQVarOp (mkSymbol ".")) (mkVar "fromJust")) (mkVar ".")) (mkVar "nullableToMaybe")) (mkVar "<$>")) (H.HsParen e)
returnType _ (I.TyOptional (I.TyInteger LongLong)) _ _ e =
    H.HsApp (H.HsApp (H.HsApp (H.HsApp (H.HsApp (mkVar "fmap") (mkVar "round")) (mkVar ".")) (mkVar "nullableToMaybe")) (mkVar "<$>")) (H.HsParen e)
returnType _ (I.TyOptional (I.TyApply _ (I.TyInteger LongLong))) _ Unchecked e =
    H.HsApp (H.HsApp (H.HsApp (H.HsApp (H.HsInfixApp (mkVar "round") (H.HsQVarOp (mkSymbol ".")) (mkVar "fromJust")) (mkVar ".")) (mkVar "nullableToMaybe")) (mkVar "<$>")) (H.HsParen e)
returnType _ (I.TyOptional (I.TyApply _ (I.TyInteger LongLong))) _ _ e =
    H.HsApp (H.HsApp (H.HsApp (H.HsApp (H.HsApp (mkVar "fmap") (mkVar "round")) (mkVar ".")) (mkVar "nullableToMaybe")) (mkVar "<$>")) (H.HsParen e)
--returnType (I.Ty (I.TyName "DOMString" Nothing)) _ _ e = H.HsApp (H.HsApp (mkVar "fromJSVal") (mkVar "<$>")) (H.HsParen e)
returnType _ (I.TySafeArray _) _ _ e =
        H.HsInfixApp
          (H.HsParen e)
          (H.HsQVarOp (mkSymbol ">>="))
          (mkVar "fromJSValUnchecked")
returnType _ (I.TyFrozenArray _) _ _ e =
        H.HsInfixApp
          (H.HsParen e)
          (H.HsQVarOp (mkSymbol ">>="))
          (mkVar "fromJSValUnchecked")
returnType _ (I.TySequence _ _) _ _ e =
        H.HsInfixApp
          (H.HsParen e)
          (H.HsQVarOp (mkSymbol ">>="))
          (mkVar "fromJSValUnchecked")
returnType _ (I.TyOptional (I.TySequence _ _)) _ _ e =
        H.HsInfixApp
          (H.HsParen e)
          (H.HsQVarOp (mkSymbol ">>="))
          (mkVar "fromJSValUnchecked")
returnType _ (I.TyOptional _) _ Normal e =
        H.HsInfixApp
          (mkVar "nullableToMaybe")
          (H.HsQVarOp (mkSymbol "<$>"))
          (H.HsParen e)
returnType _ (I.TyOptional _) _ _ e =
        H.HsInfixApp
          (H.HsInfixApp (mkVar "fromJust") (H.HsQVarOp (mkSymbol ".")) (mkVar "nullableToMaybe"))
          (H.HsQVarOp (mkSymbol "<$>"))
          (H.HsParen e)
--        H.HsInfixApp
--          (H.HsParen e)
--          (H.HsQVarOp (mkSymbol ">>="))
--          (mkVar "fromJSValUnchecked")
--returnType enums t@(I.TyObject) _ wrapType e =
--        H.HsInfixApp
--          (case tyRet enums False t [] wrapType of
--            Just (H.HsTyApp (H.HsTyVar (H.HsIdent "Maybe")) _) -> mkVar "nullableToMaybe"
--            Just _ -> H.HsInfixApp (mkVar "fromJust") (H.HsQVarOp (mkSymbol ".")) (mkVar "nullableToMaybe")
--            _ -> error "Issue with wrapType?")
--          (H.HsQVarOp (mkSymbol "<$>"))
--          (H.HsParen e)
returnType enums (I.TyPromise I.TyVoid) ext wrapType e =
        H.HsInfixApp
          (H.HsParen e)
          (H.HsQVarOp (mkSymbol ">>="))
          (mkVar "maybeThrowPromiseRejected")
returnType enums (I.TyPromise t) ext wrapType e =
        returnType enums t ext wrapType $
            H.HsInfixApp
              (H.HsParen e)
              (H.HsQVarOp (mkSymbol ">>="))
              (mkVar "checkPromiseResult")
--returnType _ (I.TySum _) _ _ e =
--        H.HsInfixApp
--          (H.HsParen e)
--          (H.HsQVarOp (mkSymbol ">>="))
--          (mkVar "fromJSValUnchecked")
--returnType _ (I.TyRecord _ _) _ _ e =
--        H.HsInfixApp
--          (H.HsParen e)
--          (H.HsQVarOp (mkSymbol ">>="))
--          (mkVar "fromJSValUnchecked")
returnType _ _ _ _ e = e

jsReturn :: I.Type -> JExpr -> String
jsReturn (I.TyName "Bool" Nothing) e = show $ renderJs [jmacroE| `(e)`?1:0 |]
jsReturn (I.TyPromise _) e = show (renderJs [jmacroE| `(e)` |]) <> ".then(function(s) { $c(null, s);}, function(e) { $c(e, null);});"
jsReturn _ e = show $ renderJs e

gtkName s =
    let lower = map toLower (U.toUnderscoreCamel s) in
    case stripPrefix "htmli_" lower of
        Just rest -> "html_i"++rest
        Nothing   -> case stripPrefix "x_path" lower of
                        Just rest -> "xpath"++rest
                        Nothing   -> case stripPrefix "web_kit" lower of
                                        Just rest -> "webkit"++rest
                                        Nothing   -> lower






















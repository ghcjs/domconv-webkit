-- DOM interface converter: a tool to convert Haskell files produced by
-- H/Direct into properly structured DOM wrapper
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding (putStrLn, readFile)
import System.Environment.UTF8
import System.Directory
import System.FilePath
import System.Exit
import System.IO (stderr, openFile, hClose, IOMode (..))
import System.IO.UTF8
import Control.Monad
import Data.Maybe
import Data.Either
import Data.List
import Data.Char
import Language.Haskell.Pretty
import Language.Preprocessor.Cpphs
import BrownPLT.JavaScript
import qualified Language.Haskell.Syntax as H
import qualified Language.Haskell.Pretty as H
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L
import qualified Utils as U
import qualified OmgParser
import LexM
import Literal
import qualified IDLSyn as I
import IDLUtils
import BasicTypes
import SplitBounds
import Paths_domconv_webkit
import Control.Applicative ((<$>))
import qualified Data.Text as T
import Data.Monoid ((<>))
import Debug.Trace
import Data.List.Split
import Common

main = do
  putStrLn "domconv-webkit : Makes Gtk2Hs Haskell bindings for webkitgtk"
  p <- getProgName
  args <- getArgs
  case args of
    [] -> putStrLn "Usage: domconv-webkit webkit.idl -Iwebkit-1.8.0/Source/WebCore"
    idl:args -> makeWebkitBindings idl args

makeWebkitBindings idl args = do
    putStrLn $ "The package will be created in the current directory which has to be empty."
    putStrLn $ "Processing IDL: " ++ idl ++ " args " ++ show args
    prntmap <- processIDL idl args
    let reversedMap = M.fromListWith S.union $ map (\(a,b)->(a,S.singleton b)) prntmap
    fixHierarchy reversedMap "hierarchy.list"
    fixHierarchy reversedMap "hierarchy3.list"
    exitWith (ExitSuccess)
  where
    fixHierarchy reversedMap hierarchyFile = do
        hh <- openFile (hierarchyFile ++ ".new") WriteMode
        current <- readFile hierarchyFile
        mapM_ (hPutStrLn hh) . filter (\line -> not ((" if webkit-dom" `isSuffixOf` line) || (" if webkitgtk-" `isInfixOf` line))) $ lines current
        let underscore "HTMLIFrameElement" = "html_iframe_element"
            underscore "XPathExpression" = "xpath_expression"
            underscore "XPathNSResolver" = "xpath_ns_resolver"
            underscore "XPathResult" = "xpath_result"
            underscore "WebKitNamedFlow" = "webkit_named_flow"
            underscore "WebKitPoint" = "webkit_point"
            underscore "WebKitAnimation" = "webkit_animation"
            underscore "WebKitAnimationList" = "webkit_animation_list"
            underscore c = U.toUnderscoreCamel c
            hierarchy n parent =
                case M.lookup parent reversedMap of
                    Just s -> do
                        forM_ (S.toList s) $ \child -> do
                            hPutStrLn hh $ replicate n ' ' ++ "WebKitDOM" ++ child ++ " as " ++ typeFor child
                                    ++ ", webkit_dom_" ++ map toLower (underscore child) ++ "_get_type if " ++ guard (typeFor child)
                            hierarchy (n+4) child
                    _ -> return ()
        hierarchy 8 ""
        hClose hh
        renameFile hierarchyFile (hierarchyFile ++ ".old")
        renameFile (hierarchyFile ++ ".new") hierarchyFile
    guard "BarProp" = "webkitgtk-2.2"
    guard "DOMNamedFlowCollection" = "webkitgtk-2.2"
    guard "DOMSecurityPolicy" =  "webkitgtk-1.10"
    guard "DOMWindowCSS" = "webkitgtk-2.2"
    guard "KeyboardEvent" = "webkitgtk-2.2"
    guard "StorageInfo" = "webkitgtk-1.10"
    guard _ = "webkit-dom"

processIDL idl args = do
  let epopts = parseOptions args -- ("-DLANGUAGE_GOBJECT=1":args)
  case epopts of
    Left s -> do
      hPutStrLn stderr $ "domconv: command line parse error " ++ s
      exitWith (ExitFailure 1)
    Right opts -> procopts idl opts

interfaceName "AbstractView" = "DOMWindow"
interfaceName n = n

procopts idl opts = do
  let (hsrc, inclfile) = (readFile idl, idl)
      baseopt = [("boolean", "Bool")]
      optsb = opts {defines = ((defines opts) ++ baseopt)
                   ,boolopts = ((boolopts opts) {pragma = True}) }
  hsrc' <- hsrc
  hsrcpr <- runCpphs optsb inclfile hsrc'
  x <- runLexM [] inclfile hsrcpr OmgParser.parseIDL
  let prntmap = mkParentMap x
  let valmsg = valParentMap prntmap
      allParents = nub $ concatMap (rights . snd) $ M.toList prntmap
  when (length valmsg > 0) $ do
    mapM_ (hPutStrLn stderr) valmsg
    exitWith (ExitFailure 2)
  let modst = DOMState {
         pm = prntmap
        ,imp = []
        ,ns = "Graphics.UI.Gtk.WebKit.DOM." -- this will be the default namespace unless a pragma namespace is used.
        ,procmod = []
        ,convlog = []
      }
      modst' = domLoop modst x
--  mapM_ (putStrLn .show) $ (procmod modst')
  mapM_ (mapM_ putSplit . splitModule) (procmod modst')

  let getParent (a, (Right b):_) = (b, a)
      getParent (a, _) = ("", a)

      filterSupported = filter (inWebKitGtk . fst)
  return . map getParent . filterSupported $ M.toList prntmap


getEnums (I.TypeDecl (I.TyEnum (Just (I.Id typename)) _)) = [typename]
getEnums (I.Module _ defs) = concatMap getEnums defs
getEnums _ = []

getAllInterfaces (I.Interface (I.Id name) _ _ _ _) = [name]
getAllInterfaces (I.Module _ defs) = concatMap getAllInterfaces defs
getAllInterfaces _ = []

getParents (I.Interface _ names _ _ _) = names
getParents (I.Module _ defs) = concatMap getParents defs
getParents _ = []

-- Retrieve a module name as a string from Module

modName :: H.Module -> String

modName m = read $ drop 1 $ dropWhile (not . isSpace) (show m)

-- Get a module namespace (all elements of name separated with dots except
-- the last one)

modNS :: String -> String

modNS mn = concat $ intersperse "." mnpts where
  mnpts = case (reverse $ parts (== '.') mn) of
    [] -> []
    [_] -> []
    (p:ps) -> reverse ps

-- Write a module surrounded by split begin/end comments
--
--newtype DomMod = DomMod H.HsModule
--
--ppHsModuleHeader :: H.Module -> Maybe [H.HsExportSpec] ->  H.Doc
--ppHsModuleHeader m mbExportList = H.mySep [
--        H.text "module (",
--        pretty m,
--        H.maybePP (H.parenList . map pretty) mbExportList,
--        text ") where"]
--  where
--    sameGuard a b = declGuard' a == declGuard' b
--    declGroups = groupBy sameGuard decls
--
--instance H.Pretty DomMod where
--    pretty (DomMod (H.HsModule pos m mbExports imp decls)) =
--            H.topLevel (ppHsModuleHeader m mbExports)
--                     (map pretty imp ++ map pretty decls)

putSplit :: (H.HsModule, String -> Maybe String) -> IO ()

putSplit (mod@(H.HsModule _ modid _ _ _), comment) = do
  let components = U.split '.' $ modName modid
      name = head (drop 2 components)
      fixMods "import Graphics.UI.Gtk.WebKit.Types" = "{#import Graphics.UI.Gtk.WebKit.Types#}"
      fixMods l = l

  createDirectoryIfMissing True (concat $ intersperse "/" $ init components)
  when (inWebKitGtk (last components)) $
      Prelude.writeFile ((concat $ intersperse "/" components) ++ ".chs") . unlines . map fixMods . lines $ prettyJS mod comment


prettyJS (H.HsModule pos (H.Module m) (Just exports) imp decls) comment = fixRenamedFunctions $
--       prettyPrint ((H.HsEVar . H.UnQual $ H.HsIdent "\n#if WEBKIT_CHECK_VERSION(2,2,2)\n        "):exports) ++ (concat . intersperse "\n" $ map prettyDecl decls)
    concat . intersperse "\n" $ ("module " ++ m ++ "(") : withModuleGuard (map (prettyExportGroupWithGuard) exportGroups) ++ ") where" : withModuleGuard (map prettyPrint imp ++ map prettyDeclGroupWithGuard declGroups)
  where
    prettyExportGroupWithGuard :: [H.HsExportSpec] -> String
    prettyExportGroupWithGuard decls@(a:_) = prettyExportGroupWithGuard' (exportGuard a) decls

    prettyExportGroupWithGuard' :: String -> [H.HsExportSpec] -> String
    prettyExportGroupWithGuard' "" decls = prettyExportGroup decls
    prettyExportGroupWithGuard' guard decls = "#if " ++ guard ++ "\n" ++ prettyExportGroup decls ++ "\n#endif"

    prettyExportGroup :: [H.HsExportSpec] -> String
    prettyExportGroup = concat . intersperse "\n" . map ((++ ",") . prettyPrint)

    prettyDecl d@(H.HsForeignImport nullLoc "javascript" H.HsUnsafe _ (H.HsIdent defop) tpsig) = concat [
        prettyPrint d]
    prettyDecl d@(H.HsTypeSig _ [H.HsIdent n] _) = prettyPrint d
    prettyDecl d = prettyPrint d

    prettyDeclGroupWithGuard :: [H.HsDecl] -> String
    prettyDeclGroupWithGuard decls@(a:_) = prettyDeclGroupWithGuard' (declGuard a) decls

    prettyDeclGroupWithGuard' :: String -> [H.HsDecl] -> String
    prettyDeclGroupWithGuard' "" decls = prettyDeclGroup decls
    prettyDeclGroupWithGuard' guard decls = "\n#if " ++ guard ++ prettyDeclGroup decls ++ "\n#endif"

    prettyDeclGroup :: [H.HsDecl] -> String
    prettyDeclGroup = concat . intersperse "\n" . map prettyDecl

    declName :: H.HsDecl -> String
    declName (H.HsTypeSig _ [H.HsIdent n] _) = n
    declName (H.HsFunBind (H.HsMatch _ (H.HsIdent n) _ _ _ : _)) = n
    declName _ = ""

    declGuard :: H.HsDecl -> String
    declGuard a = declGuard' (stripPrefix "Graphics.UI.Gtk.WebKit.DOM." m) (declName a)

    sameGuard a b = declGuard a == declGuard b
    declGroups = groupBy sameGuard decls

    exportGuard :: H.HsExportSpec -> String
    exportGuard (H.HsEVar (H.UnQual (H.HsIdent s))) = declGuard' (stripPrefix "Graphics.UI.Gtk.WebKit.DOM." m) s
    exportGuard _ = ""

    sameGuardExp a b = exportGuard a == exportGuard b
    exportGroups = groupBy sameGuardExp exports

    withModuleGuard = withModuleGuard' .  moduleGuard$ stripPrefix "Graphics.UI.Gtk.WebKit.DOM." m
    withModuleGuard' :: String -> [String] -> [String]
    withModuleGuard' "" l = l
    withModuleGuard' guard l = ("#if " ++ guard) : l ++ ["#endif"]

moduleGuard (Just "BarProp") = "WEBKIT_CHECK_VERSION(2,2,2)"
moduleGuard _ = ""

declGuard' (Just "Document") "webkitExitPointerLock" = "WEBKIT_CHECK_VERSION(2,2,2) && !WEBKIT_CHECK_VERSION(2,5,1)"
declGuard' (Just "Document") "webkitGetNamedFlows" = "WEBKIT_CHECK_VERSION(2,2,2)"
declGuard' (Just "Document") "hasFocus" = "WEBKIT_CHECK_VERSION(2,5,1)"
declGuard' (Just "Document") "getSecurityPolicy" = "WEBKIT_CHECK_VERSION(1,10,0)"
declGuard' (Just "Document") "getCurrentScript" = "WEBKIT_CHECK_VERSION(2,2,2)"
declGuard' (Just "Element") "hasAttributes" = "WEBKIT_CHECK_VERSION(2,2,2)"
declGuard' (Just "Element") "getAttributes" = "WEBKIT_CHECK_VERSION(2,2,2)"
declGuard' (Just "Element") "setId" = "WEBKIT_CHECK_VERSION(2,2,2)"
declGuard' (Just "Element") "getId" = "WEBKIT_CHECK_VERSION(2,2,2)"
declGuard' (Just "Element") "matches" = "WEBKIT_CHECK_VERSION(99,0,0)" -- TODO find out when/if this is being added
declGuard' (Just "Element") "closest" = "WEBKIT_CHECK_VERSION(99,0,0)" -- TODO find out when/if this is being added
declGuard' (Just "Element") "requestPointerLock" = "WEBKIT_CHECK_VERSION(2,2,2)"
declGuard' (Just "Element") "webkitGetRegionFlowRanges" = "WEBKIT_CHECK_VERSION(99,0,0)" -- TODO find out when/if this is being added
declGuard' (Just "File") "getLastModifiedDate" = "WEBKIT_CHECK_VERSION(99,0,0)" -- TODO find out when/if this is being added
declGuard' (Just "HTMLAnchorElement") "setText" = "WEBKIT_CHECK_VERSION(99,0,0)" -- TODO find out when/if this is being added
declGuard' (Just "HTMLAnchorElement") "getRelList" = "WEBKIT_CHECK_VERSION(99,0,0)" -- TODO find out when/if this is being added
declGuard' (Just "HTMLAreaElement") "getRel" = "WEBKIT_CHECK_VERSION(99,0,0)" -- TODO find out when/if this is being added
declGuard' (Just "HTMLAreaElement") "setRel" = "WEBKIT_CHECK_VERSION(99,0,0)" -- TODO find out when/if this is being added
declGuard' (Just "HTMLAreaElement") "getRelList" = "WEBKIT_CHECK_VERSION(99,0,0)" -- TODO find out when/if this is being added
declGuard' (Just "HTMLFormElement") "requestAutocomplete" = "WEBKIT_CHECK_VERSION(99,0,0)" -- TODO find out when/if this is being added
declGuard' (Just "HTMLImageElement") "getSizes" = "WEBKIT_CHECK_VERSION(99,0,0)" -- TODO find out when/if this is being added
declGuard' (Just "HTMLImageElement") "setSizes" = "WEBKIT_CHECK_VERSION(99,0,0)" -- TODO find out when/if this is being added
declGuard' (Just "HTMLImageElement") "getCurrentSrc" = "WEBKIT_CHECK_VERSION(99,0,0)" -- TODO find out when/if this is being added
declGuard' (Just "HTMLLinkElement") "getRelList" = "WEBKIT_CHECK_VERSION(99,0,0)" -- TODO find out when/if this is being added
declGuard' (Just "HTMLOptionsCollection") "add" = "WEBKIT_CHECK_VERSION(99,0,0)" -- TODO find out when/if this is being added
declGuard' (Just "HTMLOptionsCollection") "addBefore" = "WEBKIT_CHECK_VERSION(99,0,0)" -- TODO find out when/if this is being added
declGuard' (Just "KeyboardEvent") "getKeyCode" = "WEBKIT_CHECK_VERSION(99,0,0)" -- TODO find out when/if this is being added
declGuard' (Just "KeyboardEvent") "getCharCode" = "WEBKIT_CHECK_VERSION(99,0,0)" -- TODO find out when/if this is being added
declGuard' (Just "HTMLMediaElement") "canPlayType" = "WEBKIT_CHECK_VERSION(2,7,0)"
declGuard' (Just "HTMLMediaElement") "webkitGenerateKeyRequest" = "WEBKIT_CHECK_VERSION(99,0,0)" -- TODO find out when/if this is being added
declGuard' (Just "HTMLMediaElement") "webkitAddKey" = "WEBKIT_CHECK_VERSION(99,0,0)" -- TODO find out when/if this is being added
declGuard' (Just "HTMLMediaElement") "webkitCancelKeyRequest" = "WEBKIT_CHECK_VERSION(99,0,0)" -- TODO find out when/if this is being added
declGuard' (Just "HTMLMediaElement") "webkitSetMediaKeys" = "WEBKIT_CHECK_VERSION(99,0,0)" -- TODO find out when/if this is being added
declGuard' (Just "HTMLMediaElement") "getVideoPlaybackQuality" = "WEBKIT_CHECK_VERSION(99,0,0)" -- TODO find out when/if this is being added
declGuard' (Just "HTMLMediaElement") "getWebkitKeys" = "WEBKIT_CHECK_VERSION(99,0,0)" -- TODO find out when/if this is being added
declGuard' (Just "HTMLMediaElement") "getSrcObject" = "WEBKIT_CHECK_VERSION(99,0,0)" -- TODO find out when/if this is being added
declGuard' (Just "HTMLMediaElement") "setSrcObject" = "WEBKIT_CHECK_VERSION(99,0,0)" -- TODO find out when/if this is being added
declGuard' (Just "HTMLVideoElement") "webkitSupportsPresentationMode" = "WEBKIT_CHECK_VERSION(99,0,0)" -- TODO find out when/if this is being added
declGuard' (Just "HTMLVideoElement") "webkitSetPresentationMode" = "WEBKIT_CHECK_VERSION(99,0,0)" -- TODO find out when/if this is being added
declGuard' (Just "HTMLVideoElement") "getWebkitPresentationMode" = "WEBKIT_CHECK_VERSION(99,0,0)" -- TODO find out when/if this is being added
declGuard' (Just "HTMLInputElement") "getCapture" = "WEBKIT_CHECK_VERSION(2,7,0)"
declGuard' (Just "HTMLInputElement") "setCapture" = "WEBKIT_CHECK_VERSION(2,7,0)"
declGuard' (Just "TextTrack") "addCue" = "WEBKIT_CHECK_VERSION(2,7,0)"
declGuard' (Just "TextTrack") "getInBandMetadataTrackDispatchType" = "WEBKIT_CHECK_VERSION(99,0,0)" -- TODO find out when/if this is being added
declGuard' (Just "VideoTrackList") "getSelectedIndex" = "WEBKIT_CHECK_VERSION(99,0,0)" -- TODO find out when/if this is being added
declGuard' (Just "FocusEvent") "getRelatedTarget" = "WEBKIT_CHECK_VERSION(99,0,0)" -- TODO find out when/if this is being added
declGuard' _ _ = ""

--    interfaceName = stripPrefix "GHCJS.DOM." $ modName m
--    prefix = U.toLowerInitCamel <$> interfaceName
--    stripCamelPrefix p s = case stripPrefix p s of
--                                Just r@(x:xs) | isUpper x -> r
--                                _ -> s
--    stripGetSet = stripCamelPrefix "Get" . stripCamelPrefix "Set"
--    fix' pre s = case stripPrefix pre s of
--                        Just rest | all isDigit rest -> ""
--                        Just ('\'':_) -> ""
--                        _ -> s
--    fix = fix' "new" . fix' "newSync" . fix' "newAsync"
--
-- Split a proto-module created by domLoop. All class, data, and instance definitions
-- remain in the "head" class. All methods are grouped by their `this' argument
-- context and placed into modules with the name of that context (first character removed).
-- All modules get the same imports that the "head" module has plus the "head" module itself.

fixRenamedFunctions l = T.unpack $ T.unlines ( (T.lines $ T.pack l) >>= fixLine)
    where
        fixLine :: T.Text -> [T.Text]
        fixLine line | "{# call " `T.isInfixOf` line = fix line
        fixLine line = [line]
        fix :: T.Text -> [T.Text]
        fix line = foldl applyFix [line] fixups
        applyFix :: [T.Text] -> (T.Text, T.Text, T.Text) -> [T.Text]
        applyFix [line] (findString, replaceString, guard) | findString `T.isInfixOf` line =
            [ "#if " <> guard
            , line
            , "#else"
            , T.replace findString replaceString line
            , "#endif"]
        applyFix x _ = x
        fixups :: [(T.Text, T.Text, T.Text)]
        fixups =
            [ ("webkit_dom_document_get_visibility_state", "webkit_dom_document_get_webkit_visibility_state", "WEBKIT_CHECK_VERSION(2,2,2)")
            , ("webkit_dom_document_get_hidden", "webkit_dom_document_get_webkit_hidden", "WEBKIT_CHECK_VERSION(2,2,2)")
            , ("webkit_dom_element_request_pointer_lock", "webkit_dom_element_webkit_request_pointer_lock", "WEBKIT_CHECK_VERSION(2,6,0)")
            , ("webkit_dom_mouse_event_get_movement_x", "webkit_dom_mouse_event_get_webkit_movement_x", "WEBKIT_CHECK_VERSION(2,6,0)")
            , ("webkit_dom_mouse_event_get_movement_y", "webkit_dom_mouse_event_get_webkit_movement_y", "WEBKIT_CHECK_VERSION(2,6,0)")
            , ("{# call webkit_dom_element_set_inner_html #}", "({# call webkit_dom_html_element_set_inner_html #} . castToHTMLElement)", "WEBKIT_CHECK_VERSION(2,8,0)")
            , ("{# call webkit_dom_element_get_inner_html #}", "({# call webkit_dom_html_element_get_inner_html #} . castToHTMLElement)", "WEBKIT_CHECK_VERSION(2,8,0)")
            , ("{# call webkit_dom_element_set_outer_html #}", "({# call webkit_dom_html_element_set_outer_html #} . castToHTMLElement)", "WEBKIT_CHECK_VERSION(2,8,0)")
            , ("{# call webkit_dom_element_get_outer_html #}", "({# call webkit_dom_html_element_get_outer_html #} . castToHTMLElement)", "WEBKIT_CHECK_VERSION(2,8,0)")
            ]

splitModule :: H.HsModule -> [(H.HsModule, String -> Maybe String)]

splitModule (H.HsModule _ modid mbexp imps decls) = submods where
  headns = modNS $ modName modid
--  headmod = H.HsModule nullLoc modid headexp imps headdecls
  headdecls = filter (null . nsOf) decls
--  headexp = Just $ map (mkEIdent . declname) (classes)
--  datas = filter datadecl decls
--  datadecl (H.HsDataDecl _ _ _ _ _ _) = True
--  datadecl (H.HsNewTypeDecl _ _ _ _ _ _) = True
--  datadecl _ = False
  classes = filter classdecl headdecls
  classdecl (H.HsClassDecl _ _ _ _ _) = True
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
  methcorrn (H.HsClassDecl a b (H.HsIdent s) c d) = ((H.HsClassDecl a b (H.HsIdent (corrn s)) c d), renameMap s)
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
  submods :: [(H.HsModule, (String -> Maybe String))]
  submods = M.elems $ M.mapWithKey mksubmod methmap
  mksubmod :: String -> [(H.HsDecl, [(String, String)])] -> (H.HsModule, (String -> Maybe String))
  mksubmod iid smdecls =
    (H.HsModule nullLoc (H.Module iid) (Just subexp)
               -- (mkModImport modid : (imps ++ docimp))
               (map (mkModImport . H.Module) ([
                      "Prelude hiding (drop, error, print)"
                    , "System.Glib.FFI (maybeNull, withForeignPtr, nullForeignPtr, Ptr, nullPtr, castPtr, Word, Int64, Word64, CChar(..), CInt(..), CUInt(..), CLong(..), CULong(..), CShort(..), CUShort(..), CFloat(..), CDouble(..), toBool, fromBool)"
                    , "System.Glib.UTFString (GlibString(..), readUTFString)"
                    , "Control.Applicative ((<$>))"
                    , "Control.Monad (void)"
                    , "Control.Monad.IO.Class (MonadIO(..))"
                    , "Graphics.UI.Gtk.WebKit.Types"
                    , "System.Glib.GError"
                    , "Graphics.UI.Gtk.WebKit.DOM.EventTargetClosures"
                    ] ++ if name == "Enums"
                            then []
                            else eventImp iid ++ ["Graphics.UI.Gtk.WebKit.DOM.Enums"]))
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
        iname <- stripPrefix "GHCJS.DOM." $ iid
        return $ "\n-- | <https://developer.mozilla.org/en-US/docs/Web/API/"
                      ++ jsname iname ++ realName n ++ " Mozilla " ++ jsname iname ++ realName n ++ " documentation>"
      name = typeFor . reverse . takeWhile (/= '.') $ reverse iid
--      subexp = map mkEIdent . nub $ (filter (not . isSuffixOf "'") $ map declname smdecls) ++
      subexp = nub $ map (mkEIdent . fst) smdecls ++
                if name == "Enums"
                    then []
                    else map (H.HsEVar . H.UnQual . H.HsIdent) ([name, "castTo" ++ name, "gType" ++ name] ++ parentExp)
      parentExp = [name ++ "Class", "to" ++ name]
      eventImp "Graphics.UI.Gtk.WebKit.DOM.Event" = []
      eventImp "Graphics.UI.Gtk.WebKit.DOM.UIEvent" = []
      eventImp "Graphics.UI.Gtk.WebKit.DOM.MouseEvent" = []
      eventImp "Graphics.UI.Gtk.WebKit.DOM.EventTarget" = []
      eventImp _ = ["Graphics.UI.Gtk.WebKit.DOM.EventM"]
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
        modn = ns st ++ (renameMod $ concat $ intersperse "." $
                           reverse $ parts ( == '.') (getDef def))
        id' = I.Id modn
        imp' = modn : imp st
        modl = prmod : (procmod st) in
    domLoop st {procmod = modl, imp = imp'} defs
  z ->
    let logmsg = "Expected a Module or a Pragma; found " ++ (show z) in
    domLoop st {convlog = convlog st ++ [logmsg]} defs

-- Modify DOMState based on a pragma encountered

prgm2State :: DOMState -> String -> DOMState

prgm2State st ('n':'a':'m':'e':'s':'p':'a':'c':'e':nns) =
  let nnsst = read (dropWhile isSpace nns)
      dot = if length nnsst == 0 then "" else "." in
  st {ns = nnsst ++ dot}

prgm2State st upgm =
  let logmsg = "Unknown pragma " ++ upgm in
  st {convlog = convlog st ++ [logmsg]}

-- Validate a map of interface inheritance. Any "Left" parent identifier
-- causes a log message to be produced. It is also checked that an interface
-- does not have itself as a parent. Circular inheritance is not checked.

valParentMap :: M.Map String [Either String String] -> [String]

valParentMap pm = concat (M.elems m2) where
  m2 = M.mapWithKey lefts pm
  lefts intf parents = concat $ map (leftmsg intf) parents
  leftmsg intf (Right _) = []
  leftmsg "InternalSettings" _ = []
  leftmsg intf (Left p) = ["Interface " ++ intf ++ " has " ++ p ++ " as a parent, but " ++
                           p ++ " is not defined anywhere"]

-- Prepare a complete map of interfaces inheritance. All ancestors
-- must be defined in the IDL module being processed plus in other
-- modules it includes.

mkParentMap :: [I.Defn] -> M.Map String [Either String String]

mkParentMap defns = m2 where
  allintfs = nub $ concat $ map getintfs defns
  getintfs (I.Module _ moddefs) = filter intfOnly moddefs
  getintfs _ = []
  m1 = M.fromList $ zip (map getDef allintfs) allintfs
  m2 = M.fromList (map getparents allintfs)
  getparents i@(I.Interface (I.Id intf)  supers _ extAttrs _) = (getDef i, concat $ map parent allSupers)
    where
        allSupers | intf /= "EventTarget" && I.ExtAttr (I.Id "EventTarget") [] `elem` extAttrs = "EventTarget" : supers
                  | otherwise = supers
  parent pidf = case (pidf `M.member` m1) of
    True  -> (Right pidf) : snd (getparents (fromJust $ M.lookup pidf m1))
    False -> [Left pidf]


-- Fake source location

nullLoc = H.SrcLoc {H.srcFilename = "", H.srcLine = 0, H.srcColumn = 0}

-- A list of single-letter formal argument names (max. 26)

azList = map (: []) ['a' .. 'z']

azHIList = map H.HsIdent azList

-- Rename a module. First character of module name is uppercased. Each
-- underscore followed by a character causes that character uppercased.

renameMod :: String -> String

renameMod "" = ""
renameMod (m:odule) = toUpper m : renameMod' odule where
  renameMod' "" = ""
  renameMod' ('_':o:dule) = '.' : toUpper o : renameMod' dule
  renameMod' ('.':o:dule) = '.' : toUpper o : renameMod' dule
  renameMod' (o:dule) = o : renameMod' dule

-- Module converter mutable state (kind of)

data DOMState = DOMState {
   pm :: M.Map String [Either String String] -- inheritance map
  ,imp :: [String]                           -- import list
  ,ns :: String                              -- output module namespace (#pragma namespace)
  ,procmod :: [H.HsModule]                   -- modules already processed
  ,convlog :: [String]                       -- conversion messages
} deriving (Show)


-- Helpers to produce class and datatype identifiers out of DOM identifiers

classFor s = typeFor s ++ "Class"
typeFor  "Range" = "DOMRange"
typeFor  "Screen" = "DOMScreen"
typeFor  "Attr" = "DOMAttr"
typeFor  "Key" = "CryptoKey"
typeFor  "AlgorithmIdentifier" = "DOMString"
typeFor  "KeyFormat" = "DOMString"
-- typeFor  "XMLHttpRequestResponseType" = "DOMString"
typeFor  "custom" = "CanvasStyle"
typeFor  s = s
fixType (I.TyName s x) = I.TyName (typeFor s) x
fixType (I.TyOptional a) = I.TyOptional (fixType a)
fixType x = x
fixDefn (I.Attribute a b c d e) = I.Attribute a b (fixType c) d e
fixDefn (I.Operation a b c d e) = I.Operation (fixId a) (fixType b) c d e
fixDefn (I.Interface a b c d e) = I.Interface a b (map fixDefn c) d e
fixDefn x = x
fixId (I.FunId a b c) = I.FunId (fixId a) b (map fixParam c)
fixId x = x
fixParam (I.Param a b c d) = I.Param a b (fixType c) d

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
    intfs = filter intfOnly moddefs
    eqop op1 op2 = getDef op1 == getDef op2
    decls = types ++ classes ++ instances ++ methods ++ attrs ++ makers
    makers  = concat $ map intf2maker intfs
    classes = concat $ map intf2class intfs
    methods = concat $ map (intf2meth enums) intfs
    types = domEnumClass : (concat $ map intf2type moddefs)
    attrs = concat $ map (intf2attr enums) intfs
    instances = concat $ map (intf2inst $ pm st) intfs

mod2mod _ z = error $ "Input of mod2mod should be a Module but is " ++ show z

domEnumClass = H.HsClassDecl nullLoc [] (H.HsIdent "Enums||DomEnum") [H.HsIdent "e"] [
    H.HsTypeSig nullLoc [H.HsIdent "enumToString"] (H.HsQualType [] (H.HsTyFun (mkTIdent "e") (mkTIdent "String"))),
    H.HsTypeSig nullLoc [H.HsIdent "stringToEnum"] (H.HsQualType [] (H.HsTyFun (mkTIdent "String") (mkTIdent "e")))]

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

intf2inst pm intf@(I.Interface _ _ _ _ _) = self : parents where
  sid = getDef intf
  self = mkInstDecl sid sid
  parents = case M.lookup sid pm of
    Nothing -> []
    Just ess -> map (flip mkInstDecl sid) (map (either id id) ess)

intf2inst _ _ = []

-- For each interface found, define a newtype with the same name

intf2type :: I.Defn -> [H.HsDecl]


--intf2type intf@(I.Interface _ _ _ _ _) =
--  let typename = H.HsIdent (typeFor $ getDef intf) in
--  [H.HsDataDecl nullLoc [] typename []
--    [H.HsConDecl nullLoc typename []] []]

intf2type (I.TypeDecl (I.TyEnum (Just (I.Id typename)) vals)) =
    [H.HsDataDecl nullLoc [] (H.HsIdent $ "Enums||" ++ typename) []
      (map constructor vals) [],
     H.HsInstDecl nullLoc [] (H.UnQual $ H.HsIdent "Enums||DomEnum")
        [H.HsTyCon . H.UnQual $ H.HsIdent typename]
        [H.HsFunBind (map toString vals), H.HsFunBind (map fromString vals)]
    ]
  where
    constructor (Right n, _, Nothing)  = H.HsConDecl nullLoc (H.HsIdent $ typename ++ conName n) []
    constructor val = error $ "Unhandled enum value " ++ show val
    toString (Right n, _, Nothing) =
        H.HsMatch nullLoc (H.HsIdent "enumToString") [H.HsPVar . H.HsIdent $ typename ++ conName n]
            (H.HsUnGuardedRhs . H.HsLit $ H.HsString n) []
    toString val = error $ "Unhandled enum value " ++ show val
    fromString (Right n, _, Nothing) =
        H.HsMatch nullLoc (H.HsIdent "stringToEnum") [H.HsPLit $ H.HsString n]
            (H.HsUnGuardedRhs . H.HsVar . H.UnQual . H.HsIdent $ typename ++ conName n) []
    fromString val = error $ "Unhandled enum value " ++ show val
    conName = concatMap conPart . splitOn "-"
    conPart (initial : rest) = toUpper initial : rest
    conPart [] = ""

intf2type _ = []

-- Convert an Interface specification into a class specification

intf2class :: I.Defn -> [H.HsDecl]

intf2class intf@(I.Interface _ supers _ _ _) =
  [H.HsClassDecl nullLoc sups (H.HsIdent (classFor $ getDef intf)) (take 1 azHIList) []] where
    sups = map name2ctxt supers

intf2class _ = []

-- Convert a name to a type context assertion (assume single parameter class)

name2ctxt name = (mkUIdent $ classFor name, [H.HsTyVar $ head azHIList])

-- A helper function to produce an unqualified identifier

mkUIdent = H.UnQual . H.HsIdent

mkSymbol = H.UnQual . H.HsSymbol

-- A filter to select only operations (methods)

opsOnly :: I.Defn -> Bool
opsOnly (I.Operation _ _ _ _ _) = True
opsOnly _ = False

-- A filter to select only attributes

attrOnly :: I.Defn -> Bool
attrOnly (I.Attribute _ _ _ _ _) = True
attrOnly _ = False

-- A filter to select only interfaces (classes)

intfOnly :: I.Defn -> Bool
intfOnly (I.Interface _ _ cldefs _ _) = True
intfOnly _ = False

-- A filter to select only constant definitions

constOnly :: I.Defn -> Bool
constOnly (I.Constant _ _ _ _) = True
constOnly _ = False

-- Collect all operations defined in an interface

collectOps :: I.Defn -> [I.Defn]

collectOps (I.Interface _ _ cldefs _ _) =
  filter opsOnly cldefs

collectOps _ = []

-- Collect all constants defined in an interface

collectConst :: I.Defn -> [I.Defn]

collectConst (I.Interface _ _ cldefs _ _) =
  filter constOnly cldefs

collectConst _ = []

-- Collect all attributes defined in an interface

collectAttrs :: I.Defn -> [I.Defn]

collectAttrs (I.Interface _ _ cldefs _ _) =
  filter attrOnly cldefs

collectAttrs _ = []

-- Declare an instance (very simple case, no context, no methods only one class parameter)

mkInstDecl :: String -> String -> H.HsDecl

mkInstDecl clname typename =
  H.HsInstDecl nullLoc [] (mkUIdent $ classFor clname) [mkTIdent $ typeFor typename] []

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

-- Tag values corresponding to certain HTML element interfaces

tagFor "HTMLButtonElement" = "button"
tagFor "HTMLDivElement" = "div"
tagFor "HTMLImageElement" = "img"
tagFor "HTMLAppletElement" = "applet"
tagFor "HTMLFontElement" = "font"
tagFor "HTMLFormElement" = "form"
tagFor "HTMLFrameElement" = "frame"
tagFor "HTMLInputElement" = "input"
tagFor "HTMLObjectElement" = "object"
tagFor "HTMLParagraphElement" = "p"
tagFor "HTMLParamElement" = "param"
tagFor "HTMLPreElement" = "pre"
tagFor "HTMLScriptElement" = "script"
tagFor "HTMLTableCellElement" = "td"
tagFor "HTMLTableColElement" = "col"
tagFor "HTMLTableElement" = "table"
tagFor "HTMLTableRowElement" = "tr"
tagFor "HTMLTextAreaElement" = "textarea"
tagFor "HTMLBRElement" = "br"
tagFor "HTMLHRElement" = "hr"
tagFor "HTMLLIElement" = "li"
tagFor "HTMLDListElement" = "dl"
tagFor "HTMLOListElement" = "ol"
tagFor "HTMLUListElement" = "ul"

tagFor _ = ""

-- Attributes are represented by methods with proper type signatures.
-- These methods are wrappers around type-neutral unsafe get/set property
-- functions.

intf2attr :: [String] -> I.Defn -> [H.HsDecl]

intf2attr enums intf@(I.Interface (I.Id iid) _ cldefs _ _) =
  concat $ map mkattr $ collectAttrs intf where
    mkattr (I.Attribute [] _ _ _ _) = []
    mkattr (I.Attribute _ _ (I.TyName "MediaQueryListListener" _) _ _) = []
    mkattr (I.Attribute _ _ (I.TyName "Crypto" _) _ _) = []
    mkattr (I.Attribute [I.Id "type"] _ _ _ _) = []
    mkattr (I.Attribute [I.Id "URL"] _ _ _ _) = []
    mkattr (I.Attribute [I.Id "location"] _ _ _ _) = []
    mkattr (I.Attribute [I.Id "valueAsDate"] _ _ _ _) = []
    mkattr (I.Attribute [I.Id "webkitPeerConnection"] _ _ _ _) = []
    mkattr (I.Attribute [I.Id "contentType"] _ _ _ _) | iid == "Document" = []
    mkattr (I.Attribute [I.Id "pointerLockElement"] _ _ _ _) | iid == "Document" = []
    mkattr (I.Attribute [I.Id "activeElement"] _ _ _ _) | iid == "Document" = []
    mkattr (I.Attribute [I.Id "origin"] _ _ _ _) | iid == "Document" = []
    mkattr (I.Attribute _ _ _ _ ext) | I.ExtAttr (I.Id "Custom") [] `elem` ext = []
    mkattr (I.Attribute _ _ _ _ ext) | I.ExtAttr (I.Id "CustomSetter") [] `elem` ext = []
    mkattr (I.Attribute _ _ _ _ ext) | I.ExtAttr (I.Id "CustomGetter") [] `elem` ext = []
    mkattr (I.Attribute [I.Id iat] _ (I.TyName "EventListener" _) _ _) = trace (getDef intf ++ " " ++ iat) $ mkevent iid iat
    mkattr (I.Attribute [I.Id iat] False tat raises ext) =
      (if I.ExtAttr (I.Id "Replaceable") [] `elem` ext
        then []
        else mksetter iid iat tat raises ext)
      ++ mkgetter iid iat tat raises ext
    mkattr (I.Attribute [I.Id iat] True  tat raises ext) = mkgetter iid iat tat raises ext
    mkattr (I.Attribute (iatt:iats) b tat raises ext) =
      mkattr (I.Attribute [iatt] b tat raises ext) ++ mkattr (I.Attribute iats b tat raises ext)
    mksetter iid iat tat r ext = [stsig iid iat tat, simpl iid iat tat r ext]
    monadtv = mkTIdent "m"
    setf intf iat = "set" ++ U.toUpperHead iat
    getf intf iat = "get" ++ U.toUpperHead iat
    eventName iat = maybe iat id (stripPrefix "on" iat)
    eventf intf iat = U.toLowerInitCamel $ fixEventName (getDef intf) iat
    simpl iid iat tat raises ext =
      let defset = iid ++ "|" ++ iat ++ "|" ++ setf intf iat
          ffi = foldl H.HsApp (mkVar "{#") [H.HsVar . H.UnQual . H.HsIdent $ "call",
                H.HsVar . H.UnQual . H.HsIdent $ "webkit_dom_" ++ gtkName iid (setf intf iat),
                mkVar "#}"]
          parms = [H.HsPVar $ H.HsIdent "self", H.HsPVar $ H.HsIdent "val"]
          call = (H.HsApp ffi . H.HsParen $ H.HsApp
            (H.HsVar . H.UnQual . H.HsIdent $ "to" ++ typeFor (getDef intf))
            (H.HsVar . H.UnQual $ H.HsIdent "self"))
          val = I.Param I.Required (I.Id "val") tat [I.Mode In]
          canRaise = (not $ null (I.setterRaises raises)) || (I.ExtAttr (I.Id "SetterRaisesException") []) `elem` ext
          rhs = H.HsUnGuardedRhs $ H.HsApp (mkVar "liftIO") . H.HsParen $ propExcept canRaise $ applyParam enums val call
          match = H.HsMatch nullLoc (H.HsIdent defset) parms rhs [] in
      H.HsFunBind [match]
    stsig iid iat tat =
      let ityp = I.TyName iid Nothing
          defset = iid ++ "|" ++ iat ++ "|" ++ setf intf iat
          parm = [I.Param I.Required (I.Id "val") tat [I.Mode In]]
          parms = mkTIdent "self" : (map (fst . tyParm enums) parm)
          contxt = (mkUIdent "MonadIO", [mkTIdent "m"]) : (concat $ map (snd . tyParm enums) parm) ++ ctxRet enums ityp ++ ctxString (ityp:map paramType parm)
          tpsig = mkTsig parms (H.HsTyApp monadtv $ H.HsTyCon (H.Special H.HsUnitCon))
          retts = H.HsQualType contxt tpsig in
      H.HsTypeSig nullLoc [H.HsIdent defset] retts
    mkgetter iid iat tat r ext = [gtsig iid iat tat, gimpl iid iat tat r ext]
    gimpl iid iat tat raises ext =
      let defget = iid ++ "|" ++ iat ++ "|" ++ getf intf iat
          ffi = foldl H.HsApp (mkVar "{#") [H.HsVar . H.UnQual . H.HsIdent $ "call",
                H.HsVar . H.UnQual . H.HsIdent $ "webkit_dom_" ++ gtkName iid (getf intf iat),
                mkVar "#}"]
          parm = H.HsPVar $ H.HsIdent "self"
          call = (H.HsApp ffi . H.HsParen $ H.HsApp
            (H.HsVar . H.UnQual . H.HsIdent $ "to" ++ typeFor (getDef intf))
            (H.HsVar . H.UnQual $ H.HsIdent "self"))
          canRaise = (not $ null (I.getterRaises raises)) || (I.ExtAttr (I.Id "GetterRaisesException") []) `elem` ext
          rhs = H.HsUnGuardedRhs $ H.HsApp (mkVar "liftIO") . H.HsParen $ returnType enums tat $ propExcept canRaise call
          match = H.HsMatch nullLoc (H.HsIdent defget) [parm] rhs [] in
      H.HsFunBind [match]
    gtsig iid iat tat =
      let ityp = I.TyName iid Nothing
          defget = iid ++ "|" ++ iat ++ "|" ++ getf intf iat
          parms = [H.HsIdent "self"]
          contxt = (mkUIdent "MonadIO", [mkTIdent "m"]) : ctxRet enums ityp ++ ctxString [ityp,tat]
          tpsig = mkTsig (map H.HsTyVar parms)
                         (H.HsTyApp monadtv $ tyRet enums tat)
          retts = H.HsQualType contxt tpsig in
      H.HsTypeSig nullLoc [H.HsIdent defget] retts
    mkevent iid iat = [eventtsig iid iat, eventimpl iid iat]
    eventimpl iid iat =
      let defget = iid ++ "|" ++ iat ++ "|" ++ eventf intf iat
          ffi = foldl H.HsApp (mkVar "{#") [H.HsVar . H.UnQual . H.HsIdent $ "call",
                H.HsVar . H.UnQual . H.HsIdent $ "webkit_dom_" ++ gtkName iid (getf intf iat),
                mkVar "#}"]
          rhs = H.HsUnGuardedRhs $
              (H.HsApp
                (mkVar "EventName")
                (H.HsLit (H.HsString (eventName iat)))
              )
          match = H.HsMatch nullLoc (H.HsIdent defget) [] rhs [] in
      H.HsFunBind [match]
    eventtsig iid iat =
      let ityp = I.TyName iid Nothing
          defget = iid ++ "|" ++ iat ++ "|" ++ eventf intf iat
          contxt = ctxRet enums ityp ++ ctxString [ityp]
          tpsig = mkTsig [] $ eventTyRet (getDef intf) iat
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



intf2attr _ _ = []

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

intf2meth :: [String] -> I.Defn -> [H.HsDecl]

intf2meth enums intf@(I.Interface _ _ cldefs _ _) =
  (concat $ map mkmeth $ collectOps intf) ++
  (concat $ map mkconst $ collectConst intf) where
    getDefHs op = getDef op
    getDefJs op@(I.Operation _ _ _ mbctx _) = case mbctx of
      Nothing -> getDef op
      Just [] -> getDef op
      Just (s:_) -> s
    mkconst cn@(I.Constant (I.Id cid) _ _ (I.Lit (IntegerLit (ILit base val)))) =
      let defcn = getDef intf ++ "||pattern " ++ cid
          match = H.HsMatch nullLoc (H.HsIdent defcn) [] crhs []
          crhs = H.HsUnGuardedRhs (H.HsLit (H.HsInt val))
      in  [H.HsFunBind [match]]
    -- The EventTarget attribute on the interface makes the interface an EventTarget and so these functions
    -- are only needed on EventTarget itself
    mkmeth op | getDef intf /= "EventTarget" && getDef op `elem` ["addEventListener", "removeEventListener", "dispatchEvent"] = []
    mkmeth op | getDef op `elem` ["getCSSCanvasContext", "getSVGDocument"] = []
    mkmeth (I.Operation _ _ _ _ ext) | not (getDef intf `elem` ["Node"]) && I.ExtAttr (I.Id "Custom") [] `elem` ext = []
    mkmeth (I.Operation _ _ _ _ ext) | I.ExtAttr (I.Id "V8EnabledAtRuntime") [] `elem` ext = []
    mkmeth (I.Operation _ _ _ _ ext) | I.ExtAttr (I.Id "CallWith") [] `elem` ext = []
    mkmeth (I.Operation (I.FunId (I.Getter) _ _) _ _ _ _) = []
    mkmeth op | skip op = []
    mkmeth op = tsig op : timpl op
    tsig op@(I.Operation (I.FunId _ _ parm) optype _ _ _) =
      let monadtv = mkTIdent "m"
          -- exprtv = mkTIdent "Expression"
          defop = getDef intf ++ "|" ++ rawName op ++ "|" ++ name op parm
          parms =  mkTIdent "self" : (map (fst . tyParm enums) parm)
          contxt = (concat $ map (snd . tyParm enums) parm) ++ ctxString (optype:map paramType parm)
          -- monadctx = (mkUIdent "Monad",[monadtv])
          thisctx = (mkUIdent (classFor $ getDef intf),[mkTIdent "self"])
          tpsig = mkTsig parms (H.HsTyApp monadtv (tyRet enums optype))
          retts = H.HsQualType ((mkUIdent "MonadIO", [mkTIdent "m"]) : thisctx : contxt) tpsig in
      H.HsTypeSig nullLoc [H.HsIdent defop] retts
    timpl op@(I.Operation (I.FunId _ _ parm) optype raises _ attrib) =
      let defop = getDef intf ++ "|" ++ rawName op ++ "|" ++ name op parm
          ffi = foldl H.HsApp (mkVar "{#") [H.HsVar . H.UnQual . H.HsIdent $ "call",
                H.HsVar . H.UnQual . H.HsIdent $ "webkit_dom_" ++ gtkName (getDef intf) (getDefHs op),
                mkVar "#}"]
          parms = map (H.HsPVar . H.HsIdent) ("self" : map paramName parm)
          call = (H.HsApp ffi . H.HsParen $ H.HsApp
            (H.HsVar . H.UnQual . H.HsIdent $ "to" ++ typeFor (getDef intf))
            (H.HsVar . H.UnQual $ H.HsIdent "self"))
          -- params' = map (\I.Param (I.Id "val") tat [I.Mode In] parm
          canRaise = (not $ null raises) || (I.ExtAttr (I.Id "RaisesException") []) `elem` attrib
          rhs' = propExcept canRaise $ L.foldl (flip $ applyParam enums) call parm
          rhs "EventTarget" "addEventListener" = H.HsApp (H.HsApp (mkVar "void") (mkVar "$")) rhs'
          rhs "EventTarget" "removeEventListener" = H.HsApp (H.HsApp (mkVar "void") (mkVar "$")) rhs'
          rhs _ _ = returnType enums optype $ rhs'
--          rhs = H.HsUnGuardedRhs $ mkMethod (getDefJs op) parms (tyRet optype)
          match  = H.HsMatch nullLoc (H.HsIdent defop) parms (H.HsUnGuardedRhs . H.HsApp (mkVar "liftIO") . H.HsParen $ rhs (getDef intf) (getDefHs op)) []
      in  [H.HsFunBind [match]]
    rawName op = getDefHs op
    name op parm = rawName op ++ disambiguate (getDef intf) (rawName op) parm
    -- Only EventTarget needs these (the rest use an IsEventTarget to get them)
    skip (I.Operation (I.FunId (I.Id "addEventListener") _ _) _ _ _ _) = getDef intf /= "EventTarget"
    skip (I.Operation (I.FunId (I.Id "removeEventListener") _ _) _ _ _ _) = getDef intf /= "EventTarget"
    skip (I.Operation (I.FunId (I.Id "dispatchEvent") _ _) _ _ _ _) = getDef intf /= "EventTarget"
    skip op@(I.Operation (I.FunId _ _ parm) _ _ _ _) = any excludedParam parm || excludedMeth (getDef intf) (getDef op) parm
    excludedMeth "Document" "exitPointerLock" _ = True
    excludedMeth "DOMWindowCSS" "supports" [_] = True
    excludedMeth "HTMLInputElement" "setRangeText" [_] = True
    excludedMeth "HTMLTextAreaElement" "setRangeText" [_] = True
    excludedMeth "KeyboardEvent" "initKeyboardEvent" [_,_,_,_,_,_,_,_,_,_] = True
    excludedMeth _ _ _ = False
--    excludedParam (I.Param _ _ (I.TyName "EventListener" _) _) = True
    excludedParam (I.Param _ _ (I.TyName "MediaQueryListListener" _) _) = True
    excludedParam _ = False

intf2meth _ _ = []

-- Create a Javascript body for a method. Template for a method is:
-- method a1 ... an this = do
--   let et = undefined :: zz
--       r = DotRef et (this /\ et) (Id et "methodname")
--   return (CallExpr et r [a1 /\ et, ... an /\ et]
-- where zz is a type variable or type name of the method return type.

mkMethod :: String -> [H.HsPat] -> H.HsType -> H.HsExp

mkMethod meth args rett = H.HsDo [let1, let2, ret] where
  args' = (reverse . tail . reverse) args
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

-- Build a variable name

mkVar s = H.HsVar $ mkUIdent s

-- Build a method's type signature

mkTsig :: [H.HsType] -> H.HsType -> H.HsType

mkTsig [] a = a
mkTsig (p:ps) a = H.HsTyFun p (mkTsig ps a)

-- A helper function to produce a type identifier

mkTIdent = H.HsTyVar . H.HsIdent

mkTyList = H.HsTyApp (H.HsTyCon $ H.Special H.HsListCon)

-- A helper function to produce an export identifier.
-- Datas (Txxx) export all their members.

mkEIdent name@(n:_) | n `elem` ['T'] = (H.HsEThingAll . H.UnQual . H.HsIdent) name
                    | otherwise = (H.HsEVar . H.UnQual . H.HsIdent) name


-- Obtain a return type signature from a return type

tyRet :: [String] -> I.Type -> H.HsType

tyRet enums (I.TyOptional t@(I.TyName c Nothing)) | isNothing (asIs enums c) = tyRet enums t
tyRet enums (I.TyOptional t) = H.HsTyApp (mkTIdent "Maybe") (tyRet enums t)
tyRet enums (I.TySafeArray t) = mkTyList (tyRet enums t)
tyRet enums (I.TySequence t _) = mkTyList (tyRet enums t)
tyRet enums (I.TyName c Nothing) = case (asIs enums c) of
  Nothing -> H.HsTyApp (mkTIdent "Maybe") (mkTIdent $ typeFor c) -- H.HsTyCon $ H.Special H.HsUnitCon
  Just c' -> mkTIdent c'
tyRet _ I.TyVoid  = H.HsTyTuple []
tyRet _ (I.TyInteger LongLong) = mkTIdent "Int64"
tyRet _ (I.TyInteger _) = mkTIdent "Int"
tyRet _ (I.TyFloat Short) = mkTIdent "Float"
tyRet _ (I.TyFloat _) = mkTIdent "Double"
tyRet _ (I.TyApply (I.TySigned False) (I.TyInteger LongLong)) = mkTIdent "Word64"
tyRet _ (I.TyApply (I.TySigned False) (I.TyInteger _)) = mkTIdent "Word"
tyRet _ (I.TyApply _ (I.TyInteger LongLong)) = mkTIdent "Int64"
tyRet _ (I.TyApply _ (I.TyInteger _)) = mkTIdent "Int"
tyRet _ (I.TyObject) = mkTIdent "GObject"
tyRet _ (I.TyAny) = mkTIdent "(Ptr a)"
tyRet _ t = error $ "Return type " ++ (show t)

-- Events webkitgtk seems to know about
gtkEventType "Event" = "Event"
gtkEventType "UIEvent" = "UIEvent"
gtkEventType "MouseEvent" = "MouseEvent"
gtkEventType "WheelEvent" = "WheelEvent"
gtkEventType "KeyboardEvent" = "KeyboardEvent"
-- UIEvents webkitgtk does not know
gtkEventType "FocusEvent" = "UIEvent"
gtkEventType "TouchEvent" = "UIEvent"
gtkEventType "CompositionEvent" = "UIEvent"
gtkEventType "TextEvent" = "UIEvent"
gtkEventType "SVGZoomEvent" = "UIEvent"
gtkEventType _ = "Event"

eventTyRet :: String -> String -> H.HsType
eventTyRet interface eventName =
  H.HsTyApp
    (H.HsTyApp
      (mkTIdent "EventName")
      (mkTIdent "self")
    )
    (mkTIdent . gtkEventType $ eventType interface eventName)

-- The same, for a concrete type

cnRet :: [String] -> I.Type -> H.HsType

cnRet enums (I.TyName c Nothing) = case asIs enums c of
  Nothing -> mkTIdent ('T' : c)
  Just c' -> mkTIdent c'
cnRet enums z = tyRet enums z

-- Obtain a return type context (if any) from a return type

ctxRet :: [String] -> I.Type -> [H.HsAsst]

ctxRet enums (I.TyName c Nothing) = case (asIs enums c) of
  Nothing -> [(mkUIdent $ classFor c, [mkTIdent "self"])]
  Just c' -> []

ctxRet _ _ = []

ctxString :: [I.Type] -> [H.HsAsst]
ctxString types | I.TyName "DOMString"    Nothing `elem` types = [(mkUIdent $ "GlibString", [mkTIdent "string"])]
                | I.TyName "DOMString..." Nothing `elem` types = [(mkUIdent $ "GlibString", [mkTIdent "string"])]
ctxString _ = []

paramType (I.Param _ _ ptype _) = ptype

-- Obtain a type signature from a parameter definition

tyParm :: [String] -> I.Param -> (H.HsType, [H.HsAsst])

tyParm enums (I.Param opt (I.Id p) ptype [I.Mode In]) = lookup ptype where
  lookup ptype =
   case ptype of
    I.TyOptional t ->
        case lookup t of
            r@(H.HsTyApp (H.HsTyVar (H.HsIdent "Maybe")) _, _) -> r
            (hsType, hsAsst) -> (H.HsTyApp (mkTIdent "Maybe") hsType, hsAsst)
    I.TySequence t _ -> let (hsType, hsAsst) = lookup t in (mkTyList hsType, hsAsst)
    I.TyName c Nothing -> case asIs enums c of
      Just cc ->  (mkTIdent cc, [])
      Nothing -> (H.HsTyApp (mkTIdent "Maybe") (mkTIdent p), [(mkUIdent $ classFor c, [mkTIdent p])])
    I.TyInteger LongLong -> (mkTIdent "Int64",[])
    I.TyInteger _ -> (mkTIdent "Int",[])
    I.TyFloat Short -> (mkTIdent "Float",[])
    I.TyFloat _ -> (mkTIdent "Double",[])
    I.TyApply (I.TySigned False) (I.TyInteger LongLong) -> (mkTIdent "Word64",[])
    I.TyApply (I.TySigned False) (I.TyInteger _) -> (mkTIdent "Word",[])
    I.TyApply _ (I.TyInteger LongLong) -> (mkTIdent "Int64",[])
    I.TyApply _ (I.TyInteger _) -> (mkTIdent "Int",[])
    I.TyAny -> (mkTIdent "Ptr a",[])
    t -> error $ "Param type " ++ (show t)

tyParm _ param@(I.Param _ _ _ _) = error $ "Unsupported parameter attributes " ++ show param

-- Some types pass through as is, other are class names

asIs :: [String] -> String -> Maybe String

asIs enums a | a `elem` enums = Just a
asIs _ "DOMString"            = Just "string"
asIs _ "DOMString..."         = Just "string"
asIs _ "DOMTimeStamp"         = Just "Word"
asIs _ "CompareHow"           = Just "Word"
asIs _ "Bool"                 = Just "Bool"
asIs _ "Int"                  = Just "Int"
asIs _ _                      = Nothing

-- Apply a parameter to a FFI call

applyParam :: [String] -> I.Param -> H.HsExp -> H.HsExp

applyParam enums param@(I.Param _ (I.Id p) ptype [I.Mode In]) call = lookup ptype where
  pname = mkVar $ paramName param
  lookup ptype =
   case ptype of
    I.TyOptional t@(I.TyName c Nothing) | isNothing (asIs enums c) -> lookup t
    I.TySequence t _ ->
        (H.HsInfixApp
          (H.HsApp
            (mkVar "TODO_seqparm")
            (mkVar (paramName param))
          )
          (H.HsQVarOp (H.UnQual (H.HsSymbol ">>=")))
          (H.HsLambda nullLoc [H.HsPVar (H.HsIdent $ paramName param ++ "'")]
            (H.HsApp call (mkVar $ paramName param ++ "'"))
          )
        )
    I.TyName "DOMString" Nothing -> H.HsApp (H.HsApp (H.HsApp (mkVar "withUTFString") pname) (mkVar "$"))
                                   (H.HsLambda (H.SrcLoc "" 1 1) [H.HsPVar . H.HsIdent $ p ++ "Ptr"]
                                           $ H.HsApp call (mkVar $ p ++ "Ptr"))
    I.TyName "DOMString..." Nothing -> H.HsApp (H.HsApp (H.HsApp (mkVar "withUTFString") pname) (mkVar "$"))
                                   (H.HsLambda (H.SrcLoc "" 1 1) [H.HsPVar . H.HsIdent $ p ++ "Ptr"]
                                           $ H.HsApp call (mkVar $ p ++ "Ptr"))
    I.TyName t Nothing | t `elem` enums -> H.HsApp (H.HsApp (H.HsApp (mkVar "withUTFString") (H.HsParen (H.HsApp (mkVar "enumToString") pname))) (mkVar "$"))
                                   (H.HsLambda (H.SrcLoc "" 1 1) [H.HsPVar . H.HsIdent $ p ++ "Ptr"]
                                           $ H.HsApp call (mkVar $ p ++ "Ptr"))
    I.TyName "DOMTimeStamp" Nothing -> H.HsApp call (H.HsParen $ H.HsApp (mkVar $ "fromIntegral") pname)
    I.TyName "CompareHow" Nothing -> H.HsApp call (H.HsParen $ H.HsApp (mkVar $ "fromIntegral") pname)
    I.TyName "Bool" Nothing -> H.HsApp call (H.HsParen $ H.HsApp (mkVar $ "fromBool") pname)
    I.TyName "EventListener" Nothing ->
      H.HsApp
        call
        (H.HsParen
          (H.HsApp
            (H.HsApp
              (H.HsApp
                (mkVar "maybe")
                (mkVar "nullPtr")
              )
              (H.HsParen
                (H.HsInfixApp
                  (mkVar $ "castPtr")
                  (H.HsQVarOp $ mkSymbol ".")
                  (H.HsInfixApp
                    (mkVar $ "unEventListener")
                    (H.HsQVarOp $ mkSymbol ".")
                    (mkVar $ "toEventListener")
                  )
                )
              )
            )
            pname
          )
        )
    I.TyName x Nothing ->
      H.HsApp
        call
        (H.HsParen
          (H.HsApp
            (H.HsApp
              (H.HsApp
                (mkVar "maybe")
                (H.HsParen
                  (H.HsApp
                    (mkVar (typeFor x))
                    (mkVar "nullForeignPtr")
                  )
                )
              )
              (mkVar $ "to" ++ (typeFor x))
            )
            pname
          )
        )
    I.TyInteger _ -> H.HsApp call (H.HsParen $ H.HsApp (mkVar $ "fromIntegral") pname)
    I.TyFloat _   -> H.HsApp call (H.HsParen $ H.HsApp (mkVar $ "realToFrac") pname)
    I.TyApply _ (I.TyInteger _) -> H.HsApp call (H.HsParen $ H.HsApp (mkVar $ "fromIntegral") pname)
    I.TyAny -> H.HsApp call pname
    t -> error $ "Apply param type " ++ (show t)

applyParam enums param@(I.Param _ _ _ _) _ = error $ "Unsupported parameter attributes " ++ show param

returnType :: [String] -> I.Type -> H.HsExp -> H.HsExp
returnType _ (I.TyName "DOMString" Nothing) e = H.HsApp (H.HsApp (H.HsParen e) (mkVar ">>=")) (mkVar "readUTFString")
returnType enums (I.TyName t Nothing) e | t `elem` enums = H.HsApp (H.HsApp (mkVar "stringToEnum") (mkVar "<$>")) (H.HsParen (H.HsApp (H.HsApp (H.HsParen e) (mkVar ">>=")) (mkVar "readUTFString")))
returnType _ (I.TyName "DOMTimeStamp" Nothing) e = H.HsApp (H.HsApp (mkVar "fromIntegral") (mkVar "<$>")) (H.HsParen e)
returnType _ (I.TyName "CompareHow" Nothing) e = H.HsApp (H.HsApp (mkVar "fromIntegral") (mkVar "<$>")) (H.HsParen e)
returnType _ (I.TyName "Bool" Nothing) e = H.HsApp (H.HsApp (mkVar "toBool") (mkVar "<$>")) (H.HsParen e)
returnType _ (I.TyName x Nothing) e =
  H.HsApp
    (H.HsApp (mkVar "maybeNull")
      (H.HsParen
        (H.HsApp
          (mkVar "makeNewGObject")
          (mkVar $ "mk" ++ (typeFor x))
        )
      )
    )
    (H.HsParen e)
returnType _ (I.TyInteger _) e = H.HsApp (H.HsApp (mkVar "fromIntegral") (mkVar "<$>")) (H.HsParen e)
returnType _ (I.TyFloat _) e = H.HsApp (H.HsApp (mkVar "realToFrac") (mkVar "<$>")) (H.HsParen e)
returnType _ (I.TyApply _ (I.TyInteger _)) e = H.HsApp (H.HsApp (mkVar "fromIntegral") (mkVar "<$>")) (H.HsParen e)
returnType _ t e = e

propExcept False e = e
propExcept True e = H.HsApp (H.HsApp (mkVar "propagateGError") (mkVar "$"))
                                   (H.HsLambda (H.SrcLoc "" 1 1) [H.HsPVar . H.HsIdent $ "errorPtr_"]
                                           $ H.HsApp e (mkVar $ "errorPtr_"))

gtkName "TextTrack" "addCue" = "webkit_dom_text_track_add_cue_with_error"
gtkName "EventTarget" "addEventListener" = "event_target_add_event_listener_with_closure"
gtkName "EventTarget" "removeEventListener" = "event_target_remove_event_listener_with_closure"
gtkName i s =
    let lower = map toLower (U.toUnderscoreCamel i) ++ "_" ++ map toLower (U.toUnderscoreCamel s) in
    case stripPrefix "htmli_" lower of
        Just rest -> "html_i"++rest
        Nothing   -> case stripPrefix "x_path" lower of
                        Just rest -> "xpath"++rest
                        Nothing   -> case stripPrefix "web_kit" lower of
                                        Just rest -> "webkit"++rest
                                        Nothing   -> lower






















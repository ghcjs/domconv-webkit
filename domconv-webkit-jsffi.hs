{-# LANGUAGE QuasiQuotes #-}
-- DOM interface converter: a tool to convert Haskell files produced by
-- H/Direct into properly structured DOM wrapper

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
import Debug.Trace (trace)
import Language.Javascript.JMacro (jmacro, jmacroE, jLam, JExpr(..), JVal(..),
  Ident(..), jVarTy, JStat(..), toJExpr, renderJs, jsv)
import Data.List.Split

jsname "CryptoKey" = "Key"
jsname "CryptoKeyPair" = "KeyPair"
jsname "SubtleCrypto" = "WebKitSubtleCrypto"
jsname "DOMWindowCSS" = "CSS"
jsname "DOMCoreException" = "DOMException"
jsname "DOMNamedFlowCollection" = "WebKitNamedFlowCollection"
jsname "DOMFormData" = "FormData"
jsname "DOMURL" = "URL"
jsname "MediaKeyError" = "WebKitMediaKeyError"
jsname "DOMSecurityPolicy" = "SecurityPolicy"
jsname "DOMSelection" = "Selection"
jsname "DOMWindow" = "Window"
jsname "DOMMimeType" = "MimeType"
jsname "DOMMimeTypeArray" = "MimeTypeArray"
jsname "DOMPlugin" = "Plugin"
jsname "DOMPluginArray" = "PluginArray"
jsname "MediaKeyMessageEvent" = "WebKitMediaKeyMessageEvent"
jsname "MediaKeySession" = "WebKitMediaKeySession"
jsname "MediaKeys" = "WebKitMediaKeys"
jsname "MediaStream" = "webkitMediaStream"
jsname "RTCPeerConnection" = "webkitRTCPeerConnection"
jsname "AudioContext" = "webkitAudioContext"
jsname "OfflineAudioContext" = "webkitOfflineAudioContext"
jsname "PannerNode" = "webkitAudioPannerNode"
jsname "DOMPath" = "Path2D"
jsname "DataCue" = "WebKitDataCue"
jsname x = x

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
    -- let reversedMap = M.fromListWith S.union $ map (\(a,b)->(a,S.singleton b)) prntmap
    fixHierarchy prntmap "src/GHCJS/DOM/Types.hs" ffiExports ffiTypes
    exitWith (ExitSuccess)
  where
    fixHierarchy prntmap hierarchyFile exports printTypes = do
        hh <- openFile (hierarchyFile ++ ".new") WriteMode
        current <- readFile hierarchyFile
        let startGuard = "-- AUTO GENERATION STARTS HERE"
            endGuard   = "-- AUTO GENERATION ENDS HERE"
            (start, rest) = span (/= startGuard) $ lines current
            middle = takeWhile (/= startGuard) $ dropWhile (/= endGuard) rest
        mapM_ (hPutStrLn hh) $ start
        hPutStrLn hh startGuard
        forM_ prntmap $ \(n, parents) -> hPutStrLn hh . exports $ typeFor n
        mapM_ (hPutStrLn hh) middle
        hPutStrLn hh startGuard
        hPutStrLn hh "-- The remainder of this file is generated from IDL files using domconv-webkit-jsffi"
        printTypes hh prntmap

--        let underscore "HTMLIFrameElement" = "html_iframe_element"
--            underscore "XPathExpression" = "xpath_expression"
--            underscore "XPathNSResolver" = "xpath_ns_resolver"
--            underscore "XPathResult" = "xpath_result"
--            underscore "WebKitNamedFlow" = "webkit_named_flow"
--            underscore "WebKitPoint" = "webkit_point"
--            underscore "WebKitAnimation" = "webkit_animation"
--            underscore "WebKitAnimationList" = "webkit_animation_list"
--            underscore c = U.toUnderscoreCamel c
--            hierarchy n parent =
--                case M.lookup parent reversedMap of
--                    Just s -> do
--                        forM_ (S.toList s) $ \child -> do
--                            hPutStrLn hh $ replicate n ' ' ++ "WebKitDOM" ++ child ++ " as " ++ typeFor child
--                                    ++ ", webkit_dom_" ++ map toLower (underscore child) ++ "_get_type if webkit-dom"
--                            hierarchy (n+4) child
--                    _ -> return ()
--        hierarchy 8 ""
        hClose hh
        renameFile hierarchyFile (hierarchyFile ++ ".old")
        renameFile (hierarchyFile ++ ".new") hierarchyFile

    ffiExports name =
            "  , " ++ name ++ "(" ++ name ++ "), un" ++ name ++ ", Is" ++ name ++ ", "
            ++ "to" ++ name ++ ", " ++ "castTo" ++ name ++ ", " ++ "gType" ++ name

    ffiTypes hh prntmap =
        forM_ prntmap $ \(n, parents) -> hPutStrLn hh $
            let name = typeFor n in
            "#if (defined(ghcjs_HOST_OS) && defined(USE_JAVASCRIPTFFI)) || !defined(USE_WEBKIT)\n"
            ++ "-- | Functions for this inteface are in \"GHCJS.DOM." ++ name ++ "\".\n"
            ++ (
                if null parents
                    then ""
                    else ( "-- Base interface functions are in:\n"
                        ++ "--\n"
                        ++ concatMap (\parent -> "--     * \"GHCJS.DOM." ++ parent ++ "\"\n") (rights parents)
                        )
            )
            ++ "--\n"
            ++ "-- <https://developer.mozilla.org/en-US/docs/Web/API/"
                       ++ jsname name ++ " Mozilla " ++ jsname name ++ " documentation>\n"
            ++ "newtype " ++ name ++ " = " ++ name ++ " (JSRef " ++ name ++ ") deriving (Eq)\n\n"
            ++ "un" ++ name ++ " (" ++ name ++ " o) = o\n\n"

            ++ "instance ToJSRef " ++ name ++ " where\n"
            ++ "  toJSRef = return . un" ++ name ++ "\n"
            ++ "  {-# INLINE toJSRef #-}\n\n"

            ++ "instance FromJSRef " ++ name ++ " where\n"
            ++ "  fromJSRef = return . fmap " ++ name ++ " . maybeJSNull\n"
            ++ "  {-# INLINE fromJSRef #-}\n\n"

            ++ "class " ++ head (map ("Is"++) (rights parents) ++ ["GObjectClass"]) ++ " o => Is" ++ name ++ " o\n"
            ++ "to" ++ name ++ " :: Is" ++ name ++ " o => o -> " ++ name ++ "\n"
            ++ "to" ++ name ++ " = unsafeCastGObject . toGObject\n\n"

            ++ concatMap (\parent -> "instance Is" ++ parent ++ " " ++ name ++ "\n")
                         (name:rights parents)
            ++ "instance GObjectClass " ++ name ++ " where\n"
            ++ "  toGObject = GObject . castRef . un" ++ name ++ "\n"
            ++ "  unsafeCastGObject = " ++ name ++ " . castRef . unGObject\n\n"

            ++ "castTo" ++ name ++ " :: GObjectClass obj => obj -> " ++ name ++ "\n"
            ++ "castTo" ++ name ++ " = castTo gType" ++ name ++ " \"" ++ name ++ "\"\n\n"

            ++ "foreign import javascript unsafe \"window[\\\"" ++ jsname name ++ "\\\"]\" gType" ++ name ++ "' :: JSRef GType\n"
            ++ "gType" ++ name ++ " = GType gType" ++ name ++ "'\n"
            ++ "#else\n"
            ++ (if inWebKitGtk name then "type Is" ++ name ++ " o = " ++ name ++ "Class o\n" else "")
            ++ "#endif\n\n"

moduleInWebKitGtk "Comment" = False
moduleInWebKitGtk "DocumentFragment" = False
moduleInWebKitGtk "EventTarget" = False
moduleInWebKitGtk x = inWebKitGtk x
inWebKitGtk = (`elem` ["Attr", "BarProp", "Blob", "CDATASection", "Comment", "CSSRule", "CSSRuleList", "CSSStyleDeclaration",
                    "CSSStyleSheet", "CSSValue", "CharacterData", "Console", "Core", "Css",
                    "DOMAttr", "DOMApplicationCache", "DOMImplementation", "DOMMimeType", "DOMMimeTypeArray",
                    "DOMNamedFlowCollection", "DOMPlugin", "DOMPluginArray", "DOMRange", "DOMScreen", "DOMSecurityPolicy",
                    "DOMSelection", "DOMSettableTokenList", "DOMStringList", "DOMTokenList",
                    "DOMWindow", "DOMWindowCSS", "Document", "DocumentFragment", "DocumentType",
                    "Element", "EntityReference", "Event", "EventTarget", "EventTargetClosures", "Events", "File",
                    "FileList", "Geolocation", "HTMLAnchorElement", "HTMLAppletElement",
                    "HTMLAreaElement", "HTMLAudioElement", "HTMLBRElement", "HTMLBaseElement", "HTMLBaseFontElement",
                    "HTMLBodyElement", "HTMLButtonElement", "HTMLCanvasElement", "HTMLCollection",
                    "HTMLDListElement", "HTMLDetailsElement", "HTMLDirectoryElement",
                    "HTMLDivElement", "HTMLDocument", "HTMLElement", "HTMLEmbedElement",
                    "HTMLFieldSetElement", "HTMLFontElement", "HTMLFormElement", "HTMLFrameElement",
                    "HTMLFrameSetElement", "HTMLHRElement", "HTMLHeadElement", "HTMLHeadingElement",
                    "HTMLHtmlElement", "HTMLIFrameElement", "HTMLImageElement", "HTMLInputElement",
                    "HTMLKeygenElement", "HTMLLIElement", "HTMLLabelElement", "HTMLLegendElement",
                    "HTMLLinkElement", "HTMLMapElement", "HTMLMarqueeElement", "HTMLMediaElement",
                    "HTMLMenuElement", "HTMLMetaElement", "HTMLModElement", "HTMLOListElement",
                    "HTMLObjectElement", "HTMLOptGroupElement", "HTMLOptionElement",
                    "HTMLOptionsCollection", "HTMLParagraphElement", "HTMLParamElement",
                    "HTMLPreElement", "HTMLQuoteElement", "HTMLScriptElement", "HTMLSelectElement",
                    "HTMLStyleElement", "HTMLTableCaptionElement", "HTMLTableCellElement",
                    "HTMLTableColElement", "HTMLTableElement", "HTMLTableRowElement",
                    "HTMLTableSectionElement", "HTMLTextAreaElement", "HTMLTitleElement",
                    "HTMLUListElement", "HTMLVideoElement", "History", "Html", "KeyboardEvent",
                    "Location", "MediaError", "MediaList", "MediaQueryList", "MessagePort", "MouseEvent",
                    "NamedNodeMap", "Navigator", "Node", "NodeFilter",
                    "NodeIterator", "NodeList", "Offline", "ProcessingInstruction",
                    "Range", "Ranges", "Screen", "Storage", "StorageInfo", "StyleMedia",
                    "StyleSheet", "StyleSheetList", "Stylesheets", "Text", "TimeRanges",
                    "Traversal", "TreeWalker", "UIEvent", "ValidityState", "View", "WebKitAnimation",
                    "WebKitAnimationList", "WebKitNamedFlow", "WebKitPoint",
                    "XPathExpression", "XPathNSResolver", "XPathResult", "Xml", "Xpath"])

processIDL idl args = do
  let epopts = parseOptions ("-DLANGUAGE_GOBJECT=1":args)
  case epopts of
    Left s -> do
      hPutStrLn stderr $ "domconv: command line parse error " ++ s
      exitWith (ExitFailure 1)
    Right opts -> procopts idl opts

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
  when (length valmsg > 0) $ do
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
  mapM_ (mapM_ putSplit . splitModule) (procmod modst')

  let getParent (a, (Right b):_) = (b, a)
      getParent (a, _) = ("", a)

  return $ M.toList prntmap

getEnums (I.TypeDecl (I.TyEnum (Just (I.Id typename)) _)) = [typename]
getEnums (I.Module _ defs) = concatMap getEnums defs
getEnums _ = []

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
{-
maybeNull = ()
makeNewGObject = ()
mkEventTarget = castRef
-}

putSplit :: H.HsModule -> IO ()

putSplit mod@(H.HsModule _ modid _ _ _) = do
  let components = U.split '.' $ modName modid
      name = head (drop 2 components)

  createDirectoryIfMissing True (concat $ intersperse "/" $ init components)
  Prelude.writeFile ((concat $ intersperse "/" ("src":components)) ++ ".hs") $
        "{-# LANGUAGE CPP #-}\n"
     ++ "#if (defined(ghcjs_HOST_OS) && defined(USE_JAVASCRIPTFFI)) || !defined(USE_WEBKIT)\n"
     ++ "{-# LANGUAGE ForeignFunctionInterface, JavaScriptFFI #-}\n"
     ++ prettyJS mod
     ++ "\n#else\n"
     ++ "module " ++ modName modid ++ " (\n"
     ++ (if moduleInWebKitGtk name then "  module Graphics.UI.Gtk.WebKit.DOM." ++ name ++ "\n" else "")
     ++ "  ) where\n"
     ++ (if moduleInWebKitGtk name then "import Graphics.UI.Gtk.WebKit.DOM." ++ name ++ "\n" else "")
     ++ "#endif\n"

prettyJS (H.HsModule pos m mbExports imp decls) = concat . intersperse "\n" $
       prettyPrint (H.HsModule pos m mbExports imp [])
     : map prettyDecl decls
  where
    prettyDecl d@(H.HsForeignImport nullLoc "javascript" H.HsUnsafe _ (H.HsIdent defop) tpsig) = concat [
        prettyPrint d]
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
    comment n = do
        iname <- interfaceName
        p <- prefix
        f <- U.toLowerInitCamel . stripGetSet <$> stripPrefix p n
        let func' = fix f
            func = if null func' then "" else "." ++ func'
        return $ "\n-- | <https://developer.mozilla.org/en-US/docs/Web/API/"
                      ++ jsname iname ++ func ++ " Mozilla " ++ jsname iname ++ func ++ " documentation>"

-- Split a proto-module created by domLoop. All class, data, and instance definitions
-- remain in the "head" class. All methods are grouped by their `this' argument
-- context and placed into modules with the name of that context (first character removed).
-- All modules get the same imports that the "head" module has plus the "head" module itself.

splitModule :: H.HsModule -> [H.HsModule]

splitModule (H.HsModule _ modid mbexp imps decls) = headmod : submods where
  headns = modNS $ modName modid
  headmod = H.HsModule nullLoc modid headexp imps headdecls
  headdecls = filter (null . nsOf) decls
  headexp = Just $ map (mkEIdent . declname) (classes)
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
  mtsigs = filter (not . null . nsOf) (reverse decls)
  corrn = drop 1 . dropWhile (/= '|')
  methcorrn (H.HsForeignImport a b c d (H.HsIdent s) f) = H.HsForeignImport a b c d (H.HsIdent (corrn s)) f
  methcorrn (H.HsTypeSig x [H.HsIdent s] y) = H.HsTypeSig x [H.HsIdent (corrn s)] y
  methcorrn (H.HsFunBind [H.HsMatch x (H.HsIdent s) y z t]) =
    H.HsFunBind [H.HsMatch x (H.HsIdent (corrn s)) y z t]
  methcorrn (H.HsDataDecl a b (H.HsIdent s) c d e) = H.HsDataDecl a b (H.HsIdent (corrn s)) c d e
  methcorrn (H.HsInstDecl a b (H.UnQual (H.HsIdent s)) c d) = H.HsInstDecl a b (H.UnQual (H.HsIdent (corrn s))) c d
  methcorrn z = z
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
  mkmethmap m [] = m
  mkmethmap m ((i, meth) : ims) = mkmethmap addmeth ims where
    addmeth = case M.lookup i m of
      Nothing -> M.insert i [meth] m
      (Just meths) -> M.insert i (meth : meths) m
  submods = M.elems $ M.mapWithKey mksubmod methmap
  mksubmod iid smdecls =
    H.HsModule nullLoc (H.Module iid) (Just subexp)
               -- (mkModImport modid : (imps ++ docimp))
               (map (mkModImport . H.Module) ([
                      "GHCJS.Types (JSRef(..), JSString, castRef)"
                    , "GHCJS.Foreign (jsNull, ToJSString(..), FromJSString(..), syncCallback, asyncCallback, syncCallback1, asyncCallback1, syncCallback2, asyncCallback2, ForeignRetention(..))"
                    , "GHCJS.Marshal (ToJSRef(..), FromJSRef(..))"
                    , "GHCJS.Marshal.Pure (PToJSRef(..), PFromJSRef(..))"
                    , "Data.Int (Int64)"
                    , "Data.Word (Word, Word64)"
                    , "GHCJS.DOM.Types"
                    , "Control.Applicative ((<$>))"
                    ] ++ if name == "Enums"
                            then []
                            else eventImp iid ++ ["GHCJS.DOM.Enums"]))
               (H.HsFunBind [] : smdecls) where
      name = typeFor . reverse . takeWhile (/= '.') $ reverse iid
      subexp = map mkEIdent . nub $ (filter (not . isSuffixOf "'") $ map declname smdecls) ++
                if name == "Enums" then [] else [name, "Is" ++ name, "castTo" ++ name, "gType" ++ name, "to" ++ name]
      eventImp "GHCJS.DOM.Event" = []
      eventImp "GHCJS.DOM.UIEvent" = []
      eventImp "GHCJS.DOM.MouseEvent" = []
      eventImp _ = ["GHCJS.DOM.EventM"]
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
  getparents i@(I.Interface _ supers _ _ _) = (getDef i, concat $ map parent supers)
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

classFor s = "Is" ++ typeFor s
typeFor  "Range" = "DOMRange"
typeFor  "Screen" = "DOMScreen"
typeFor  "Attr" = "DOMAttr"
typeFor  "Key" = "CryptoKey"
typeFor  "AlgorithmIdentifier" = "DOMString"
typeFor  "KeyFormat" = "DOMString"
typeFor  "XMLHttpRequestResponseType" = "DOMString"
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
    modlst = ["Control.Monad"]
    modid' = renameMod $ getDef md
    imps = [] -- map mkModImport (map H.Module (modlst ++ imp st))
    intfs = filter intfOnly moddefs
    eqop op1 op2 = getDef op1 == getDef op2
    decls = types ++ classes ++ instances ++ methods ++ attrs ++ makers
    makers  = concat $ map intf2maker intfs
    classes = concat $ map intf2class intfs
    methods = concat $ map (intf2meth enums) intfs
    types = concat $ map intf2type moddefs
    attrs = concat $ map (intf2attr enums) intfs
    instances = concat $ map (intf2inst $ pm st) intfs

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

intf2inst pm intf@(I.Interface _ _ _ _ _) = self : parents where
  sid = getDef intf
  self = mkInstDecl sid sid
  parents = case M.lookup sid pm of
    Nothing -> []
    Just ess -> map (flip mkInstDecl sid) (map (either id id) ess)

intf2inst _ _ = []

-- For each interface found, define a newtype with the same name

intf2type :: I.Defn -> [H.HsDecl]


--intf2type intf@(I.Interface _ _ _) =
--  let typename = H.HsIdent (typeFor $ getDef intf) in
--  [H.HsDataDecl nullLoc [] typename []
--    [H.HsConDecl nullLoc typename []] []]

intf2type (I.TypeDecl (I.TyEnum (Just (I.Id typename)) vals)) =
    [H.HsDataDecl nullLoc [] (H.HsIdent $ "Enums|" ++ typename) []
      (map constructor vals) [],
     H.HsInstDecl nullLoc [] (H.UnQual $ H.HsIdent "Enums|PToJSRef")
        [H.HsTyCon . H.UnQual $ H.HsIdent typename]
        [H.HsFunBind (map ptoJsRef vals)],
     H.HsInstDecl nullLoc [] (H.UnQual $ H.HsIdent "Enums|ToJSRef")
        [H.HsTyCon . H.UnQual $ H.HsIdent typename]
        [H.HsFunBind
            [H.HsMatch nullLoc (H.HsIdent "toJSRef") []
                (H.HsUnGuardedRhs $ H.HsInfixApp (mkVar "return") (H.HsQVarOp $ mkSymbol ".") (mkVar "ptoJSRef")) []
            ]
        ],
     H.HsInstDecl nullLoc [] (H.UnQual $ H.HsIdent "Enums|PFromJSRef")
        [H.HsTyCon . H.UnQual $ H.HsIdent typename]
        [H.HsFunBind (map pfromJsRef vals)],
     H.HsInstDecl nullLoc [] (H.UnQual $ H.HsIdent "Enums|FromJSRef")
        [H.HsTyCon . H.UnQual $ H.HsIdent typename]
        [H.HsFunBind
            [H.HsMatch nullLoc (H.HsIdent "fromJSRefUnchecked") []
                (H.HsUnGuardedRhs $ H.HsInfixApp (mkVar "return") (H.HsQVarOp $ mkSymbol ".") (mkVar "pfromJSRef")) [],
             H.HsMatch nullLoc (H.HsIdent "fromJSRef") []
                (H.HsUnGuardedRhs $ H.HsInfixApp (H.HsInfixApp (mkVar "return") (H.HsQVarOp $ mkSymbol ".") (mkVar "pfromJSRef")) (H.HsQVarOp $ mkSymbol ".") (mkVar "castRef")) []
            ]
        ]
    ]
    ++ map jsffi vals
  where
    constructor (Right n, _, Nothing)  = H.HsConDecl nullLoc (H.HsIdent $ typename ++ conName n) []
    constructor val = error $ "Unhandled enum value " ++ show val
    jsffi (Right n, _, Nothing) = H.HsForeignImport nullLoc "javascript" H.HsUnsafe
        ("\"" ++ n ++ "\"")
        (H.HsIdent $ "Enums|js_" ++ typename ++ conName n)
        (H.HsTyApp (mkTIdent "JSRef") (mkTIdent typename))
    jsffi val = error $ "Unhandled enum value " ++ show val
    ptoJsRef (Right n, _, Nothing) =
        H.HsMatch nullLoc (H.HsIdent "ptoJSRef") [H.HsPVar . H.HsIdent $ typename ++ conName n]
            (H.HsUnGuardedRhs . mkVar $ "js_" ++ typename ++ conName n) []
    ptoJsRef val = error $ "Unhandled enum value " ++ show val
    pfromJsRef (Right n, _, Nothing) =
        H.HsMatch nullLoc (H.HsIdent "pfromJSRef") [H.HsPVar $ H.HsIdent "x"]
            (H.HsGuardedRhss [H.HsGuardedRhs nullLoc
                (H.HsInfixApp (mkVar "x") (H.HsQVarOp $ mkSymbol "==") (mkVar $ "js_" ++ typename ++ conName n))
                (H.HsVar . H.UnQual . H.HsIdent $ typename ++ conName n)]) []
    pfromJsRef val = error $ "Unhandled enum value " ++ show val
    conName = concatMap conPart . splitOn "-"
    conPart (initial : rest) = toUpper initial : rest

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
    mkattr (I.Attribute [I.Id "type"] _ _ _ _) = [] -- Use GType instead
    mkattr (I.Attribute [I.Id "URL"] _ _ _ _) | getDef intf `elem` ["EventSource", "WebSocket"] = [] -- The standard is the lowercase url
    mkattr (I.Attribute [I.Id iat] _ (I.TyName "EventListener" _) _ _) = mkevent iid iat
    mkattr (I.Attribute [I.Id iat] False tat raises ext) =
      (if I.ExtAttr (I.Id "Replaceable") [] `elem` ext
        then []
        else mksetter iid iat tat raises)
      ++ mkgetter iid iat tat raises
    mkattr (I.Attribute [I.Id iat] True  tat raises _) = mkgetter iid iat tat raises
    mkattr (I.Attribute (iatt:iats) b tat raises ext) =
      mkattr (I.Attribute [iatt] b tat raises ext) ++ mkattr (I.Attribute iats b tat raises ext)
    mksetter iid iat tat r = [sjsffi iid iat tat, stsig iid iat tat, simpl iid iat tat r]
    monadtv = mkTIdent "IO"
    setf intf iat = U.toLowerInitCamel $ getDef intf ++ "Set" ++ U.toUpperHead iat
    getf intf iat = U.toLowerInitCamel $ getDef intf ++ "Get" ++ U.toUpperHead iat
    eventName iat = maybe iat id (stripPrefix "on" iat)
    eventf intf iat = U.toLowerInitCamel $ getDef intf ++ U.toUpperHead iat
    sjsffi iid iat tat =
      let monadtv = mkTIdent "IO"
          defop = iid ++ "|" ++ "ghcjs_dom_" ++ gtkName (setf intf iat)
          parm = [I.Param I.Required (I.Id "val") tat [I.Mode In]]
          parms = ffiTySelf intf : map (fst . tyParmFFI enums) parm
          tpsig = mkTsig parms (H.HsTyApp monadtv $ H.HsTyCon (H.Special H.HsUnitCon))
          retts = H.HsQualType [] tpsig
          jsimpl = show . renderJs $ [jmacro| $1[`(iat)`] = $2 |] in
       H.HsForeignImport nullLoc "javascript" H.HsUnsafe jsimpl (H.HsIdent defop) tpsig
    simpl iid iat tat raises =
      let defset = iid ++ "|" ++ setf intf iat
          ffi = H.HsVar . H.UnQual . H.HsIdent $ "ghcjs_dom_" ++ gtkName (setf intf iat)
          parms = [H.HsPVar $ H.HsIdent "self", H.HsPVar $ H.HsIdent "val"]
          call = H.HsApp ffi . H.HsParen $
                H.HsApp
                    (H.HsVar . H.UnQual . H.HsIdent $ "un" ++ typeFor (getDef intf))
                    (H.HsParen $
                        H.HsApp
                            (H.HsVar . H.UnQual . H.HsIdent $ "to" ++ typeFor (getDef intf))
                            (H.HsVar . H.UnQual $ H.HsIdent "self"))
          val = I.Param I.Required (I.Id "val") tat [I.Mode In]
          rhs = H.HsUnGuardedRhs $ propExcept (I.setterRaises raises) $ applyParam enums val call
          match = H.HsMatch nullLoc (H.HsIdent defset) parms rhs [] in
      H.HsFunBind [match]
    stsig iid iat tat =
      let defset = iid ++ "|" ++ setf intf iat
          parm = I.Param I.Required (I.Id "val") tat [I.Mode In]
          parms = mkTIdent "self" : (fst $ tyParm enums parm) : []
          contxt = ctxSelf iid : (snd $ tyParm enums parm)
          tpsig = mkTsig parms (H.HsTyApp monadtv $ H.HsTyCon (H.Special H.HsUnitCon))
          retts = H.HsQualType contxt tpsig in
      H.HsTypeSig nullLoc [H.HsIdent defset] retts
    mkgetter iid iat tat r = [gjsffi iid iat tat, gtsig iid iat tat, gimpl iid iat tat r]
    gjsffi iid iat tat =
      let monadtv = mkTIdent "IO"
          defop = iid ++ "|" ++ "ghcjs_dom_" ++ gtkName (getf intf iat)
          parms = [ffiTySelf intf]
          tpsig = mkTsig parms (H.HsTyApp monadtv $ tyRet enums True tat)
          retts = H.HsQualType [] tpsig
          jsimpl = show . renderJs . jsReturn tat $ [jmacroE| $1[`(iat)`] |] in
       H.HsForeignImport nullLoc "javascript" H.HsUnsafe jsimpl (H.HsIdent defop) tpsig
    gimpl iid iat tat raises =
      let defget = iid ++ "|" ++ getf intf iat
          ffi = H.HsVar . H.UnQual . H.HsIdent $ "ghcjs_dom_" ++ gtkName (getf intf iat)
          parm = H.HsPVar $ H.HsIdent "self"
          call = H.HsApp ffi . H.HsParen $
                H.HsApp
                    (H.HsVar . H.UnQual . H.HsIdent $ "un" ++ typeFor (getDef intf))
                    (H.HsParen $
                        H.HsApp
                            (H.HsVar . H.UnQual . H.HsIdent $ "to" ++ typeFor (getDef intf))
                            (H.HsVar . H.UnQual $ H.HsIdent "self"))
          rhs = H.HsUnGuardedRhs $ returnType enums tat $ propExcept (I.getterRaises raises) call
          match = H.HsMatch nullLoc (H.HsIdent defget) [parm] rhs [] in
      H.HsFunBind [match]
    gtsig iid iat tat =
      let defget = iid ++ "|" ++ getf intf iat
          parms = [H.HsIdent "self"]
          contxt = ctxSelf iid : ctxRet tat
          tpsig = mkTsig (map H.HsTyVar parms)
                         (H.HsTyApp monadtv $ tyRet enums False tat)
          retts = H.HsQualType contxt tpsig in
      H.HsTypeSig nullLoc [H.HsIdent defget] retts
    mkevent iid iat = [eventtsig iid iat, eventimpl iid iat]
    eventimpl iid iat =
      let defget = iid ++ "|" ++ eventf intf iat
          ffi = H.HsVar . H.UnQual . H.HsIdent $ "ghcjs_dom_" ++ gtkName (getf intf iat)
          rhs = H.HsUnGuardedRhs $
          --H.HsApp
            --(mkVar "Signal")
            (H.HsParen
              (H.HsApp
                (mkVar "connect")
                (H.HsLit (H.HsString (eventName iat)))
              )
            )
          match = H.HsMatch nullLoc (H.HsIdent defget) [] rhs [] in
      H.HsFunBind [match]
    eventtsig iid iat =
      let defget = iid ++ "|" ++ eventf intf iat
          contxt = [ctxSelf iid]
          tpsig = mkTsig [] $ eventTyRet iat
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

intf2meth enums intf@(I.Interface _ _ [I.Operation (I.FunId _ _ parm) resultType _ _ _] _ (Just (I.Id "callback"))) =
    [f sync withArgs | sync <- [True, False], withArgs <- [False, True], f <- [tsig, timpl]]
  where
    defop sync withArgs = getDef intf ++ "|" ++ U.toLowerInitCamel (getDef intf) ++ "New"
        ++ (if sync then "Sync" else "Async") ++ (if withArgs then "\'" else "")
    tsig sync withArgs =
      let monadtv = mkTIdent "IO"
          cparm _ (I.TyApply (I.TySigned False) (I.TyInteger LongLong)) = mkTIdent "Double"
          cparm pname pType = tyRet' pname enums False pType
          cparms = [cparm pname pType | I.Param _ (I.Id pname) pType _ <- parm]
          cbfunc = [mkTsig cparms (H.HsTyApp monadtv (fst $ tyParm enums resultParam))]
          parms | sync && withArgs = mkTIdent "ForeignRetention":mkTIdent "Bool":cbfunc
                | withArgs = mkTIdent "ForeignRetention":cbfunc
                | otherwise = cbfunc
          contxt = concat [ctxRet' pname pType | I.Param _ (I.Id pname) pType _ <- (resultParam : parm)]
          resultParam = I.Param I.Required (I.Id "result") resultType [I.Mode In]
          tpsig = mkTsig parms (H.HsTyApp monadtv (mkTIdent (typeFor $ getDef intf)))
          retts = H.HsQualType contxt tpsig in
      H.HsTypeSig nullLoc [H.HsIdent (defop sync withArgs)] retts
    timpl sync withArgs =
      let cparms = map (H.HsPVar . H.HsIdent . paramName) parm
          cbfunc = [H.HsPVar $ H.HsIdent "callback"]
          parms | sync && withArgs = H.HsPVar (H.HsIdent "retention"):H.HsPVar (H.HsIdent "continueAsync"):cbfunc
                | withArgs = H.HsPVar (H.HsIdent "retention"):cbfunc
                | otherwise = cbfunc
          castReturn = H.HsInfixApp (
                          H.HsInfixApp
                            (mkVar $ getDef intf)
                            (H.HsQVarOp (H.UnQual (H.HsSymbol ".")))
                            (mkVar "castRef")
                          )
                          (H.HsQVarOp (H.UnQual (H.HsSymbol "<$>")))
          callbackN | null parm = ""
                    | otherwise = show (length parm)
          callbackMaker | sync = "syncCallback" ++ callbackN
                        | otherwise = "asyncCallback" ++ callbackN
          lambda = H.HsParen (H.HsLambda nullLoc cparms (L.foldl applyCParam (mkVar "callback") parm))
          call | sync && withArgs = H.HsApp (H.HsApp (H.HsApp (mkVar $ callbackMaker) (mkVar "retention")) (mkVar "continueAsync")) lambda
               | sync = H.HsApp (H.HsApp (H.HsApp (mkVar $ callbackMaker) (mkVar "AlwaysRetain")) (mkVar "True")) lambda
               | withArgs = H.HsApp (H.HsApp (mkVar $ callbackMaker) (mkVar "retention")) lambda
               | otherwise = H.HsApp (H.HsApp (mkVar $ callbackMaker) (mkVar "AlwaysRetain")) lambda
          rhs = H.HsUnGuardedRhs $ castReturn call
          match  = H.HsMatch nullLoc (H.HsIdent (defop sync withArgs)) parms rhs []
          applyCParam e param =
            (H.HsInfixApp
              (H.HsApp
                (mkVar "fromJSRefUnchecked")
                (mkVar (paramName param))
              )
              (H.HsQVarOp (H.UnQual (H.HsSymbol ">>=")))
              (H.HsLambda nullLoc [H.HsPVar (H.HsIdent $ paramName param ++ "'")]
                (H.HsApp e (mkVar $ paramName param ++ "'"))
              )
            )
      in  H.HsFunBind [match]

intf2meth enums intf@(I.Interface _ _ cldefs at _) =
  (concat $ map mkconstructor constructors) ++
  (concat $ map mkmeth $ collectOps intf) ++
  (concat $ map mkconst $ collectConst intf) where
    getDefHs op = getDef op
    getDefJs op@(I.Operation _ _ _ mbctx _) = case mbctx of
      Nothing -> getDef op
      Just [] -> getDef op
      Just (s:_) -> s
    constructors = zip (map (\n -> take n $ repeat '\'') [0..]) $ reverse [parm | x@(I.ExtAttr (I.Id "Constructor") parm) <- at]
    mkconstructor c = constructorjsffi c : constructortsig c : constructortimpl c
    constructorRaises | I.ExtAttr (I.Id "ConstructorRaisesException") [] `elem` at = [I.Raises ["RaisesException"]]
                      | otherwise = []
    constructorjsffi (postfix, parm) =
      let monadtv = mkTIdent "IO"
          defop = getDef intf ++ "|" ++ jsffiConstructorName ++ postfix
          parms = map (fst . tyParmFFI enums) parm
          tpsig = mkTsig parms (H.HsTyApp monadtv (ffiTySelf intf))
          retts = H.HsQualType [] tpsig
          jsimpl = case parm of
                    [I.Param _ _ (I.TyName "DOMString..." _) _] ->
                        show . renderJs $ [jmacroE| window[`(jsname (getDef intf))`].apply(window, $1) |]
                    _ ->
                        show . renderJs $ ApplExpr [jmacroE| new window[`(jsname (getDef intf))`] |]
                            (map (\(n, _) -> jsv $ '$':show n) $ zip [1..] parm) in
       H.HsForeignImport nullLoc "javascript" H.HsUnsafe jsimpl (H.HsIdent defop) tpsig
    constructortsig (postfix, parm) =
      let monadtv = mkTIdent "IO"
          defop = getDef intf ++ "|" ++ constructorName ++ postfix
          parms = map (fst . tyParm enums) parm
          contxt = concat $ map (snd . tyParm enums) parm
          tpsig = mkTsig parms (H.HsTyApp monadtv (mkTIdent (typeFor $ getDef intf)))
          retts = H.HsQualType contxt tpsig in
      H.HsTypeSig nullLoc [H.HsIdent defop] retts
    constructortimpl (postfix, parm) =
      let defop = getDef intf ++ "|" ++ constructorName ++ postfix
          ffi = H.HsVar . H.UnQual . H.HsIdent $ jsffiConstructorName ++ postfix
          parms = map (H.HsPVar . H.HsIdent . paramName) parm
          call = ffi
          rhs = H.HsUnGuardedRhs (H.HsInfixApp
                  (propExcept constructorRaises $ L.foldl (flip $ applyParam enums) call parm)
                  (H.HsQVarOp (H.UnQual (H.HsSymbol ">>=")))
                  (mkVar "fromJSRefUnchecked")
                )
          match  = H.HsMatch nullLoc (H.HsIdent defop) parms rhs []
      in  [H.HsFunBind [match]]
    mkconst cn@(I.Constant (I.Id cid) _ _ (I.Lit (IntegerLit (ILit base val)))) =
      let defcn = getDef intf ++ "|c" ++ cid
          match = H.HsMatch nullLoc (H.HsIdent defcn) [] crhs []
          crhs = H.HsUnGuardedRhs (H.HsLit (H.HsInt val))
      in  [H.HsFunBind [match]]
--    mkmeth (I.Operation _ _ _ _ ext) | I.ExtAttr (I.Id "V8EnabledAtRuntime") [] `elem` ext = []
    mkmeth op | skip op = []
    mkmeth op = jsffi op : tsig op : timpl op
    jsffi op@(I.Operation (I.FunId _ _ parm) optype _ _ _) =
      let monadtv = mkTIdent "IO"
          defop = getDef intf ++ "|" ++ jsffiName op parm
          parms = ffiTySelf intf : map (fst . tyParmFFI enums) parm
          tpsig = mkTsig parms (H.HsTyApp monadtv (tyRet enums True optype))
          retts = H.HsQualType [] tpsig
          jsimpl = case parm of
                    [I.Param _ _ (I.TyName "DOMString..." _) _] ->
                        show . renderJs . jsReturn optype $ [jmacroE| $1[`(getDef op)`].apply($1, $2) |]
                    _ ->
                        show . renderJs . jsReturn optype $ ApplExpr [jmacroE| $1[`(getDef op)`] |]
                            (map (\(n, _) -> jsv $ '$':show n) $ zip [2..] parm) in
       H.HsForeignImport nullLoc "javascript" H.HsUnsafe jsimpl (H.HsIdent defop) tpsig
    tsig op@(I.Operation (I.FunId _ _ parm) optype _ _ _) =
      let monadtv = mkTIdent "IO"
          -- exprtv = mkTIdent "Expression"
          defop = getDef intf ++ "|" ++ name op parm
          parms =  mkTIdent "self" : (map (fst . tyParm enums) parm)
          contxt = (ctxSelf (getDef intf) : (concat $ map (snd . tyParm enums) parm)) ++ ctxRet optype
          -- monadctx = (mkUIdent "Monad",[monadtv])
          tpsig = mkTsig parms (H.HsTyApp monadtv (tyRet enums False optype))
          retts = H.HsQualType contxt tpsig in
      H.HsTypeSig nullLoc [H.HsIdent defop] retts
    timpl op@(I.Operation (I.FunId _ _ parm) optype raises _ _) =
      let defop = getDef intf ++ "|" ++ name op parm
          ffi = H.HsVar . H.UnQual . H.HsIdent $ jsffiName op parm
          parms = map H.HsPVar (map H.HsIdent ("self" : map paramName parm))
          call = H.HsApp ffi . H.HsParen $
                H.HsApp
                    (H.HsVar . H.UnQual . H.HsIdent $ "un" ++ typeFor (getDef intf))
                    (H.HsParen $
                        H.HsApp
                            (H.HsVar . H.UnQual . H.HsIdent $ "to" ++ typeFor (getDef intf))
                            (H.HsVar . H.UnQual $ H.HsIdent "self"))
          -- params' = map (\I.Param (I.Id "val") tat [I.Mode In] parm
          rhs = H.HsUnGuardedRhs $ returnType enums optype $ propExcept raises $ L.foldl (flip $ applyParam enums) call parm
--          rhs = H.HsUnGuardedRhs $ mkMethod (getDefJs op) parms (tyRet optype)
          match  = H.HsMatch nullLoc (H.HsIdent defop) parms rhs []
      in  [H.HsFunBind [match]]
    constructorName = (U.toLowerInitCamel $ getDef intf) ++ "New"
    rawName op = (U.toLowerInitCamel $ getDef intf) ++ (U.toUpperHead $ getDefHs op)
    name op parm = rawName op ++ disambiguate (rawName op) parm
--    jsffiName op parm = name op parm ++ "'"
    jsffiConstructorName = "ghcjs_dom_" ++ gtkName (getDef intf) ++ "_new"
    jsffiName op parm = "ghcjs_dom_" ++ gtkName (getDef intf ++ U.toUpperHead (getDefHs op))
                                     ++ disambiguate (rawName op) parm
    skip (I.Operation (I.FunId _ _ parm) _ _ _ _) = any excludedParam parm
    excludedParam (I.Param _ _ (I.TyName "EventListener" _) _) = True -- We use event attribute info instead of addEventListenet and removeEventListner
    excludedParam (I.Param _ _ (I.TyName "MediaQueryListListener" _) _) = True
    excludedParam _ = False
    disambiguate "domWindowCSSSupports" [_, _] = "2"
    disambiguate "htmlInputElementSetRangeText" [_, _, _, _] = "4"
    disambiguate "htmlTextAreaElementSetRangeText" [_, _, _, _] = "4"
    disambiguate "navigatorVibrate" [I.Param _ _ (I.TySequence _ _) _] = "Pattern"
    disambiguate "audioContextCreateBuffer" [_, _] = "FromArrayBuffer"
    disambiguate "audioNodeConnect" [I.Param _ _ (I.TyOptional (I.TyName "AudioParam" _)) _, _] = "Param"
    disambiguate name (I.Param _ _ (I.TyName "DOMPath" _) _:_) | name `elem` canvasPathFunctionNames = "Path"
    disambiguate "canvasRenderingContext2DSetStrokeColor" (I.Param _ (I.Id "grayLevel") _ _:_) = "Gray"
    disambiguate "canvasRenderingContext2DSetStrokeColor" (I.Param _ (I.Id "r") _ _:_) = "RGB"
    disambiguate "canvasRenderingContext2DSetStrokeColor" (I.Param _ (I.Id "c") _ _:_) = "CYMK"
    disambiguate "canvasRenderingContext2DSetFillColor" (I.Param _ (I.Id "grayLevel") _ _:_) = "Gray"
    disambiguate "canvasRenderingContext2DSetFillColor" (I.Param _ (I.Id "r") _ _:_) = "RGB"
    disambiguate "canvasRenderingContext2DSetFillColor" (I.Param _ (I.Id "c") _ _:_) = "CYMK"
    disambiguate "canvasRenderingContext2DSetShadow" (_:_:_:I.Param _ (I.Id "grayLevel") _ _:_) = "Gray"
    disambiguate "canvasRenderingContext2DSetShadow" (_:_:_:I.Param _ (I.Id "r") _ _:_) = "RGB"
    disambiguate "canvasRenderingContext2DSetShadow" (_:_:_:I.Param _ (I.Id "c") _ _:_) = "CYMK"
    disambiguate "canvasRenderingContext2DDrawImage" [I.Param _ (I.Id "canvas") _ _,_,_] = "FromCanvas"
    disambiguate "canvasRenderingContext2DDrawImage" [I.Param _ (I.Id "canvas") _ _,_,_,_,_] = "FromCanvasScaled"
    disambiguate "canvasRenderingContext2DDrawImage" [I.Param _ (I.Id "canvas") _ _,_,_,_,_,_,_,_,_] = "FromCanvasPart"
    disambiguate "canvasRenderingContext2DDrawImage" [I.Param _ (I.Id "video") _ _,_,_] = "FromVideo"
    disambiguate "canvasRenderingContext2DDrawImage" [I.Param _ (I.Id "video") _ _,_,_,_,_] = "FromVideoScaled"
    disambiguate "canvasRenderingContext2DDrawImage" [I.Param _ (I.Id "video") _ _,_,_,_,_,_,_,_,_] = "FromVideoPart"
    disambiguate "canvasRenderingContext2DDrawImage" [_,_,_] = ""
    disambiguate "canvasRenderingContext2DDrawImage" [_,_,_,_,_] = "Scaled"
    disambiguate "canvasRenderingContext2DDrawImage" [_,_,_,_,_,_,_,_,_] = "Part"
    disambiguate "canvasRenderingContext2DPutImageData" [_,_,_,_,_,_,_] = "Dirty"
    disambiguate "canvasRenderingContext2DWebkitPutImageDataHD" [_,_,_,_,_,_,_] = "Dirty"
    disambiguate "canvasRenderingContext2DCreatePattern" [I.Param _ (I.Id "canvas") _ _,_] = "FromCanvas"
    disambiguate "canvasRenderingContext2DCreateImageData" [_,_] = "Size"
    disambiguate "canvasRenderingContext2DSetLineWidth" _ = "Function"
    disambiguate "canvasRenderingContext2DSetLineCap" _ = "Function"
    disambiguate "canvasRenderingContext2DSetLineJoin" _ = "Function"
    disambiguate "canvasRenderingContext2DSetMiterLimit" _ = "Function"
    disambiguate "databaseCallbackHandleEvent" [I.Param _ _ (I.TyName "DatabaseSync" _) _] = "Sync"
    disambiguate "dataTransferItemListAdd" [_] = "File"
    disambiguate "mediaControlsHostSortedTrackListForMenu" [I.Param _ _ (I.TyName "AudioTrackList" _) _] = "Audio"
    disambiguate "mediaControlsHostDisplayNameForTrack" [I.Param _ _ (I.TyName "AudioTrack" _) _] = "Audio"
    disambiguate "rtcDataChannelSend" [I.Param _ _ (I.TyName "ArrayBufferView" _) _] = "View"
    disambiguate "rtcDataChannelSend" [I.Param _ _ (I.TyName "Blob" _) _] = "Blob"
    disambiguate "rtcDataChannelSend" [I.Param _ _ (I.TyName "DOMString" _) _] = "String"
    disambiguate "webSocketSend" [I.Param _ _ (I.TyName "ArrayBufferView" _) _] = "View"
    disambiguate "webSocketSend" [I.Param _ _ (I.TyName "Blob" _) _] = "Blob"
    disambiguate "webSocketSend" [I.Param _ _ (I.TyName "DOMString" _) _] = "String"
    disambiguate "sourceBufferAppendBuffer" [I.Param _ _ (I.TyName "ArrayBufferView" _) _] = "View"
    disambiguate "webGLRenderingContextBufferData" [_, I.Param _ _ (I.TyOptional (I.TyName "ArrayBufferView" _)) _, _] = "View"
    disambiguate "webGLRenderingContextBufferData" [_, I.Param _ _ (I.TyName "GLsizeiptr" _) _, _] = "Ptr"
    disambiguate "webGLRenderingContextBufferSubData" [_, _, I.Param _ _ (I.TyOptional (I.TyName "ArrayBufferView" _)) _] = "View"
    disambiguate "webGLRenderingContextTexImage2D" [_, _, _, _, _, _, _, _, I.Param _ _ (I.TyOptional (I.TyName "ArrayBufferView" _)) _] = "View"
    disambiguate "webGLRenderingContextTexImage2D" [_, _, _, _, _, I.Param _ _ (I.TyOptional (I.TyName "ImageData" _)) _] = "Data"
    disambiguate "webGLRenderingContextTexImage2D" [_, _, _, _, _, I.Param _ _ (I.TyOptional (I.TyName "HTMLCanvasElement" _)) _] = "Canvas"
    disambiguate "webGLRenderingContextTexImage2D" [_, _, _, _, _, I.Param _ _ (I.TyOptional (I.TyName "HTMLVideoElement" _)) _] = "Video"
    disambiguate "webGLRenderingContextTexSubImage2D" [_, _, _, _, _, _, _, _, I.Param _ _ (I.TyOptional (I.TyName "ArrayBufferView" _)) _] = "View"
    disambiguate "webGLRenderingContextTexSubImage2D" [_, _, _, _, _, _, I.Param _ _ (I.TyOptional (I.TyName "ImageData" _)) _] = "Data"
    disambiguate "webGLRenderingContextTexSubImage2D" [_, _, _, _, _, _, I.Param _ _ (I.TyOptional (I.TyName "HTMLCanvasElement" _)) _] = "Canvas"
    disambiguate "webGLRenderingContextTexSubImage2D" [_, _, _, _, _, _, I.Param _ _ (I.TyOptional (I.TyName "HTMLVideoElement" _)) _] = "Video"
    disambiguate "domurlCreateObjectURL" [I.Param _ _ (I.TyOptional (I.TyName "MediaSource" _)) _] = "Source"
    disambiguate "domurlCreateObjectURL" [I.Param _ _ (I.TyOptional (I.TyName "MediaStream" _)) _] = "Stream"
    disambiguate "idbDatabaseTransaction" [I.Param _ (I.Id "storeNames") _ _, _] = "'"
    disambiguate "idbIndexOpenCursor" [I.Param _ _ (I.TyOptional (I.TyName "IDBKeyRange" _)) _, _] = "Range"
    disambiguate "idbIndexOpenKeyCursor" [I.Param _ _ (I.TyOptional (I.TyName "IDBKeyRange" _)) _, _] = "Range"
    disambiguate "idbIndexGet" [I.Param _ _ (I.TyOptional (I.TyName "IDBKeyRange" _)) _] = "Range"
    disambiguate "idbIndexGetKey" [I.Param _ _ (I.TyOptional (I.TyName "IDBKeyRange" _)) _] = "Range"
    disambiguate "idbIndexCount" [I.Param _ _ (I.TyOptional (I.TyName "IDBKeyRange" _)) _] = "Range"
    disambiguate "idbObjectStoreDelete" [I.Param _ _ (I.TyOptional (I.TyName "IDBKeyRange" _)) _] = "Range"
    disambiguate "idbObjectStoreGet" [I.Param _ _ (I.TyOptional (I.TyName "IDBKeyRange" _)) _] = "Range"
    disambiguate "idbObjectStoreOpenCursor" [I.Param _ _ (I.TyOptional (I.TyName "IDBKeyRange" _)) _, _] = "Range"
    disambiguate "idbObjectStoreCreateIndex" [_, I.Param _ _ (I.TySequence _ _) _, _] = "'"
    disambiguate "idbObjectStoreCount" [I.Param _ _ (I.TyOptional (I.TyName "IDBKeyRange" _)) _] = "Range"
    disambiguate name _ = ""
    canvasPathFunctionNames = [
        "canvasRenderingContext2DFill",
        "canvasRenderingContext2DStroke",
        "canvasRenderingContext2DClip",
        "canvasRenderingContext2DIsPointInPath",
        "canvasRenderingContext2DIsPointInStroke",
        "canvasRenderingContext2DDrawFocusIfNeeded"]

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

-- A helper function to produce an export identifier.
-- Datas (Txxx) export all their members.

mkEIdent name@(n:_) | n `elem` ['T'] = (H.HsEThingAll . H.UnQual . H.HsIdent) name
                    | otherwise = (H.HsEVar . H.UnQual . H.HsIdent) name


mkTyList = H.HsTyApp (H.HsTyCon $ H.Special H.HsListCon)

-- Obtain a return type signature from a return type


tyRet :: [String] -> Bool -> I.Type -> H.HsType
tyRet = tyRet' "result"

tyRet' :: String -> [String] -> Bool -> I.Type -> H.HsType

tyRet' _ enums ffi (I.TyName "object" Nothing) | ffi = H.HsTyApp (mkTIdent "JSRef") (mkTIdent "GObject")
                                               | otherwise = H.HsTyApp (mkTIdent "Maybe") (mkTIdent "GObject")
tyRet' pname _ ffi (I.TyName "DOMString" Nothing) | ffi = mkTIdent "JSString"
                                                  | otherwise = mkTIdent pname
--tyRet ffi (I.TyOptional t) | ffi = tyRet ffi t
--tyRet ffi (I.TyOptional t@(I.TyName c Nothing)) | isNothing (asIs ffi c) = tyRet ffi t
--tyRet ffi (I.TyOptional t) | otherwise = H.HsTyApp (mkTIdent "Maybe") (tyRet ffi t)
tyRet' pname enums False (I.TyOptional t@(I.TyName c Nothing)) | isNothing (asIs [] False c) = tyRet' pname enums False t
tyRet' pname enums False (I.TyOptional t) = H.HsTyApp (mkTIdent "Maybe") (tyRet' pname enums False t)
--tyRet ffi (I.TySafeArray t) | ffi = H.HsTyApp (mkTIdent "JSRef") (mkTyList $ tyRet ffi t)
--                            | otherwise = mkTyList (tyRet ffi t)
--tyRet ffi (I.TySequence t _)| ffi = H.HsTyApp (mkTIdent "JSRef") (mkTyList $ tyRet ffi t)
--                            | otherwise = mkTyList (tyRet ffi t)
tyRet' pname enums False (I.TySafeArray t) = mkTyList (tyRet' pname enums False t)
tyRet' pname enums False (I.TySequence t _) = mkTyList (tyRet' pname enums False t)
tyRet' _ _ True (I.TyName "GLintptr" Nothing) = mkTIdent "Double"
tyRet' _ _ True (I.TyName "GLsizeiptr" Nothing) = mkTIdent "Double"
tyRet' _ enums ffi (I.TyName c Nothing) = case (asIs enums ffi c) of
  Nothing | ffi -> H.HsTyApp (mkTIdent "JSRef") (mkTIdent $ typeFor c)
  Nothing -> H.HsTyApp (mkTIdent "Maybe") (mkTIdent $ typeFor c)
  Just c' -> c'
tyRet' _ _ _ I.TyVoid  = H.HsTyTuple []
tyRet' _ _ ffi (I.TyInteger LongLong) | ffi = mkTIdent "Double"
                                   | otherwise = mkTIdent "Int64"
tyRet' _ _ _ (I.TyInteger _) = mkTIdent "Int"
tyRet' _ _ _ (I.TyFloat Short) = mkTIdent "Float"
tyRet' _ _ _ (I.TyFloat _) = mkTIdent "Double"
tyRet' _ _ ffi (I.TyApply (I.TySigned False) (I.TyInteger LongLong)) | ffi = mkTIdent "Double"
                                                                     | otherwise = mkTIdent "Word64"
tyRet' _ _ _ (I.TyApply (I.TySigned False) (I.TyInteger _)) = mkTIdent "Word"
tyRet' _ _ ffi (I.TyApply _ (I.TyInteger LongLong)) | ffi = mkTIdent "Double"
                                                 | otherwise = mkTIdent "Int64"
tyRet' _ _ _ (I.TyApply _ (I.TyInteger _)) = mkTIdent "Int"
tyRet' _ _ _ (I.TyObject) = H.HsTyApp (mkTIdent "JSRef") (mkTIdent "GObject")
tyRet' _ _ _ (I.TyAny) = mkTIdent "(JSRef a)"
tyRet' _ enums True (I.TyOptional (I.TyInteger LongLong)) = H.HsTyApp (mkTIdent "JSRef") (H.HsTyApp (mkTIdent "Maybe") (mkTIdent "Double"))
tyRet' _ enums True (I.TyOptional (I.TyApply _ (I.TyInteger LongLong))) = H.HsTyApp (mkTIdent "JSRef") (H.HsTyApp (mkTIdent "Maybe") (mkTIdent "Double"))
tyRet' _ enums True t = H.HsTyApp (mkTIdent "JSRef") (tyRet enums False t)
tyRet' _ _ _ t = error $ "Return type " ++ (show t)

eventType "onclick"       = "MouseEvent"
eventType "oncontextmenu" = "MouseEvent"
eventType "ondblclick"    = "MouseEvent"
eventType "ondrag"        = "MouseEvent"
eventType "ondragstart"   = "MouseEvent"
eventType "ondragenter"   = "MouseEvent"
eventType "ondragover"    = "MouseEvent"
eventType "ondragleave"   = "MouseEvent"
eventType "ondragend"     = "MouseEvent"
eventType "ondrop"        = "MouseEvent"
eventType "onmousedown"   = "MouseEvent"
eventType "onmousemove"   = "MouseEvent"
eventType "onmouseout"    = "MouseEvent"
eventType "onmouseover"   = "MouseEvent"
eventType "onmouseup"     = "MouseEvent"
eventType "onmousewheel"  = "MouseEvent"
eventType _               = "UIEvent"

eventTyRet :: String -> H.HsType
eventTyRet eventName =
  H.HsTyApp
    (H.HsTyApp
      (mkTIdent "Signal")
      (mkTIdent "self")
    )
    (H.HsTyApp
      (H.HsTyApp
        (H.HsTyApp
          (mkTIdent "EventM")
          (mkTIdent $ eventType eventName)
        )
        (mkTIdent "self")
      )
      (H.HsTyCon $ H.Special H.HsUnitCon)
    )

-- The same, for a concrete type
--
--cnRet :: I.Type -> H.HsType
--
--cnRet (I.TyName c Nothing) = case asIs c of
--  Nothing -> mkTIdent ('T' : c)
--  Just c' -> mkTIdent c'
--cnRet z = tyRet z

-- Obtain a return type context (if any) from a return type

ctxSelf :: String -> H.HsAsst
ctxSelf t = (mkUIdent (classFor t),[mkTIdent "self"])

ffiTySelf intf = H.HsTyApp (mkTIdent "JSRef") (mkTIdent (typeFor $ getDef intf))

ctxRet :: I.Type -> [H.HsAsst]
ctxRet = ctxRet' "result"

ctxRet' :: String -> I.Type -> [H.HsAsst]

ctxRet' pname (I.TyName "DOMString" Nothing) = [(mkUIdent "FromJSString", [mkTIdent pname])]
ctxRet' pname (I.TyName "XMLHttpRequestResponseType" Nothing) = [(mkUIdent "FromJSString", [mkTIdent pname])]
--ctxRet (I.TyName c Nothing) = case (asIs False c) of
--  Nothing -> [(mkUIdent $ classFor c, [mkTIdent "self"])]
--  Just c' -> []
ctxRet' pname (I.TySequence t _) = ctxRet' pname t
ctxRet' pname (I.TySafeArray t) = ctxRet' pname t
ctxRet' pname (I.TyOptional t) = ctxRet' pname t
ctxRet' pname _ = []

-- Obtain a type signature from a parameter definition

tyParm :: [String] -> I.Param -> (H.HsType, [H.HsAsst])
tyParm enums = tyParm' enums False

tyParmFFI :: [String] -> I.Param -> (H.HsType, [H.HsAsst])
tyParmFFI enums = tyParm' enums True

tyParm' enums ffi param@(I.Param _ (I.Id _) ptype [I.Mode In]) = lookup ptype where
  p = mkTIdent (paramName param)
  lookup ptype =
   case ptype of
    I.TyOptional t@(I.TyName c Nothing) | ffi -> lookup t
--        case lookup t of
--            (H.HsTyApp (H.HsTyVar (H.HsIdent "JSRef")) a, b) -> (H.HsTyApp (mkTIdent "JSRef")
--                                                                (H.HsTyApp (mkTIdent "Maybe") a), b)
--            (hsType, hsAsst) -> (H.HsTyApp (mkTIdent "Maybe") hsType, hsAsst)
    I.TyOptional t | not ffi ->
        case lookup t of
            r@(H.HsTyApp (H.HsTyVar (H.HsIdent "Maybe")) _, _) -> r
            (hsType, hsAsst) -> (H.HsTyApp (mkTIdent "Maybe") hsType, hsAsst)
    I.TySequence t _ | not ffi -> let (hsType, hsAsst) = lookup t in (mkTyList hsType, hsAsst)
    I.TySafeArray t | not ffi -> let (hsType, hsAsst) = lookup t in (mkTyList hsType, hsAsst)
    I.TyName "DOMString" Nothing | ffi -> (mkTIdent "JSString", [])
                                 | otherwise -> (p, [(mkUIdent "ToJSString", [p])])
    I.TyName "DOMString..." Nothing | ffi -> (mkTIdent "JSRef [a]", [])
                                    | otherwise -> (mkTIdent $ "[" ++ paramName param ++ "]", [(mkUIdent "ToJSString", [p]), (mkUIdent "ToJSRef", [p])])
    I.TyName "GLintptr" Nothing | ffi -> (mkTIdent "Double",[])
    I.TyName "GLsizeiptr" Nothing | ffi -> (mkTIdent "Double",[])
    I.TyName c Nothing -> case asIs enums ffi c of
      Just cc       -> (cc, [])
      Nothing | ffi -> (H.HsTyApp (mkTIdent "JSRef") (mkTIdent (typeFor c)), [])
      _             -> (H.HsTyApp (mkTIdent "Maybe") (p), [(mkUIdent $ classFor c, [p])])
    I.TyInteger LongLong | ffi -> (mkTIdent "Double",[])
                         | otherwise -> (mkTIdent "Int64",[])
    I.TyInteger _ -> (mkTIdent "Int",[])
    I.TyFloat Short -> (mkTIdent "Float",[])
    I.TyFloat _ -> (mkTIdent "Double",[])
    I.TyApply (I.TySigned False) (I.TyInteger LongLong) | ffi -> (mkTIdent "Double",[])
                                                        | otherwise -> (mkTIdent "Word64",[])
    I.TyApply (I.TySigned False) (I.TyInteger _) -> (mkTIdent "Word",[])
    I.TyApply _ (I.TyInteger LongLong) | ffi -> (mkTIdent "Double",[])
                                       | otherwise -> (mkTIdent "Int64",[])
    I.TyApply _ (I.TyInteger _) -> (mkTIdent "Int",[])
    I.TyAny -> (mkTIdent "JSRef a",[])
    _ | ffi -> let (hsType, hsAsst) = tyParm' enums False param in (H.HsTyApp (mkTIdent "JSRef") hsType, hsAsst)
    t -> error $ "Param type " ++ (show t)

tyParm' _ _ param@(I.Param _ _ _ _) = error $ "Unsupported parameter attributes " ++ show param

glTypes = ["GLenum", "GLboolean", "GLbitfield", "GLbyte", "GLshort", "GLint", "GLsizei",
           "GLintptr", "GLsizeiptr", "GLubyte", "GLushort", "GLuint", "GLfloat", "GLclampf"]

-- Some types pass through as is, other are class names

asIs :: [String] -> Bool -- ^ Is JSFFI call
    -> String -> Maybe H.HsType

asIs enums ffi a | a `elem` enums = Just $ if ffi then H.HsTyApp (mkTIdent  "JSRef") (mkTIdent a) else mkTIdent a
asIs _ _ "DOMString"            = Just $ mkTIdent "String"
asIs _ _ "DOMTimeStamp"         = Just $ mkTIdent "Word"
asIs _ _ "CompareHow"           = Just $ mkTIdent "Word"
asIs _ _ x | x `elem` glTypes   = Just $ mkTIdent x
-- asIs True "Bool"      = Just "JSBool"
asIs _ _ "Bool"                 = Just $ mkTIdent "Bool"
asIs _ _ "Int"                  = Just $ mkTIdent "Int"
asIs _ _ _                      = Nothing

paramName (I.Param _ (I.Id "data") _ _)  = "data'"
paramName (I.Param _ (I.Id "type") _ _)  = "type'"
paramName (I.Param _ (I.Id "where") _ _) = "where'"
paramName (I.Param _ (I.Id "family") _ _) = "family'"
paramName (I.Param _ (I.Id p) _ _) = p

-- Apply a parameter to a FFI call

applyParam :: [String] -> I.Param -> H.HsExp -> H.HsExp

applyParam enums param@(I.Param _ (I.Id p) ptype [I.Mode In]) call = lookup ptype where
  pname = mkVar $ paramName param
  lookup ptype =
   case ptype of
    I.TyOptional t@(I.TyName c Nothing) | isNothing (asIs [] False c) -> lookup t
    I.TyOptional (I.TyName "DOMString" Nothing) ->
      H.HsApp
        call
        (H.HsParen
          (H.HsApp
            (H.HsApp
              (H.HsApp
                (mkVar "maybe")
                (mkVar "jsNull")
              )
              (mkVar "toJSString")
            )
            pname
          )
        )
    I.TyOptional (I.TySequence t _) ->
        (H.HsInfixApp
          (H.HsApp
            (mkVar "toJSRef")
            (mkVar (paramName param))
          )
          (H.HsQVarOp (H.UnQual (H.HsSymbol ">>=")))
          (H.HsLambda nullLoc [H.HsPVar (H.HsIdent $ paramName param ++ "'")]
            (H.HsApp call (mkVar $ paramName param ++ "'"))
          )
        )
    I.TySequence t _ ->
        (H.HsInfixApp
          (H.HsApp
            (mkVar "toJSRef")
            (mkVar (paramName param))
          )
          (H.HsQVarOp (H.UnQual (H.HsSymbol ">>=")))
          (H.HsLambda nullLoc [H.HsPVar (H.HsIdent $ paramName param ++ "'")]
            (H.HsApp call (mkVar $ paramName param ++ "'"))
          )
        )
    I.TySafeArray t ->
        (H.HsInfixApp
          (H.HsApp
            (mkVar "toJSRef")
            (mkVar (paramName param))
          )
          (H.HsQVarOp (H.UnQual (H.HsSymbol ">>=")))
          (H.HsLambda nullLoc [H.HsPVar (H.HsIdent $ paramName param ++ "'")]
            (H.HsApp call (mkVar $ paramName param ++ "'"))
          )
        )
    I.TyName "DOMString" Nothing -> H.HsApp call (H.HsParen $ H.HsApp (mkVar $ "toJSString") pname)
    I.TyName "DOMString..." Nothing ->
        (H.HsInfixApp
          (H.HsApp
            (mkVar "toJSRef")
            (mkVar (paramName param))
          )
          (H.HsQVarOp (H.UnQual (H.HsSymbol ">>=")))
          (H.HsLambda nullLoc [H.HsPVar (H.HsIdent $ paramName param ++ "'")]
            (H.HsApp call (mkVar $ paramName param ++ "'"))
          )
        )
    I.TyName "DOMTimeStamp" Nothing -> H.HsApp call pname
    I.TyName "CompareHow" Nothing -> H.HsApp call pname
    I.TyName "GLintptr" Nothing -> H.HsApp call (H.HsParen $ H.HsApp (mkVar $ "fromIntegral") pname)
    I.TyName "GLsizeiptr" Nothing -> H.HsApp call (H.HsParen $ H.HsApp (mkVar $ "fromIntegral") pname)
    I.TyName x Nothing | x `elem` glTypes -> H.HsApp call pname
    I.TyName "Bool" Nothing -> H.HsApp call pname
    I.TyName x Nothing | x `elem` enums ->
      H.HsApp
        call
        (H.HsParen
          (H.HsApp
            (mkVar "ptoJSRef")
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
                (mkVar "jsNull")
              )
              (H.HsParen
                (H.HsApp
                  (H.HsApp
                    (mkVar $ "un" ++ typeFor x)
                    (mkVar $ ".")
                  )
                  (mkVar $ "to" ++ typeFor x)
                )
              )
            )
            pname
          )
        )
    I.TyInteger LongLong -> H.HsApp call (H.HsParen $ H.HsApp (mkVar $ "fromIntegral") pname)
    I.TyInteger _ -> H.HsApp call pname
    I.TyFloat _   -> H.HsApp call pname
    I.TyApply _ (I.TyInteger LongLong) -> H.HsApp call (H.HsParen $ H.HsApp (mkVar $ "fromIntegral") pname)
    I.TyApply _ (I.TyInteger _) -> H.HsApp call pname
    I.TyAny -> H.HsApp call pname
    t -> error $ "Apply Param type " ++ (show t)

applyParam _ param@(I.Param _ _ _ _) _ = error $ "Unsupported parameter attributes " ++ show param

returnType :: [String] ->  I.Type -> H.HsExp -> H.HsExp
returnType _ (I.TyName "DOMString" Nothing) e = H.HsApp (H.HsApp (mkVar "fromJSString") (mkVar "<$>")) (H.HsParen e)
--returnType (I.TySafeArray (I.TyName "DOMString" Nothing)) e = H.HsApp (H.HsApp (mkVar "fromJSRef") (mkVar "<$>")) (H.HsParen e)
--returnType (I.TySequence (I.TyName "DOMString" Nothing) _) e = H.HsApp (H.HsApp (mkVar "fromJSRef") (mkVar "<$>")) (H.HsParen e)
returnType _ (I.TyName "DOMTimeStamp" Nothing) e = e
returnType _ (I.TyName "CompareHow" Nothing) e = e
returnType _ (I.TyName "GLintptr" Nothing) e = H.HsApp (H.HsApp (mkVar "round") (mkVar "<$>")) (H.HsParen e)
returnType _ (I.TyName "GLsizeiptr" Nothing) e = H.HsApp (H.HsApp (mkVar "round") (mkVar "<$>")) (H.HsParen e)
returnType _ (I.TyName x Nothing) e | x `elem` glTypes = e
returnType _(I.TyName "Bool" Nothing) e = e
returnType enums (I.TyName x Nothing) e | x `elem` enums =
        (H.HsInfixApp
          (H.HsParen e)
          (H.HsQVarOp (H.UnQual (H.HsSymbol ">>=")))
          (mkVar "fromJSRefUnchecked")
        )
returnType _ (I.TyName x Nothing) e =
        (H.HsInfixApp
          (H.HsParen e)
          (H.HsQVarOp (H.UnQual (H.HsSymbol ">>=")))
          (mkVar "fromJSRef")
        )
returnType _ (I.TyInteger LongLong) e = H.HsApp (H.HsApp (mkVar "round") (mkVar "<$>")) (H.HsParen e)
--returnType (I.TyFloat _) e = H.HsApp (H.HsApp (mkVar "realToFrac") (mkVar "<$>")) (H.HsParen e)
returnType _ (I.TyApply _ (I.TyInteger LongLong)) e = H.HsApp (H.HsApp (mkVar "round") (mkVar "<$>")) (H.HsParen e)
returnType _ (I.TyOptional (I.TyInteger LongLong)) e =
    H.HsApp (H.HsApp (H.HsApp (H.HsApp (H.HsApp (mkVar "fmap") (mkVar "round")) (mkVar ".")) (mkVar "pfromJSRef")) (mkVar "<$>")) (H.HsParen e)
returnType _ (I.TyOptional (I.TyApply _ (I.TyInteger LongLong))) e =
    H.HsApp (H.HsApp (H.HsApp (H.HsApp (H.HsApp (mkVar "fmap") (mkVar "round")) (mkVar ".")) (mkVar "pfromJSRef")) (mkVar "<$>")) (H.HsParen e)
--returnType (I.Ty (I.TyName "DOMString" Nothing)) e = H.HsApp (H.HsApp (mkVar "fromJSRef") (mkVar "<$>")) (H.HsParen e)
returnType _ (I.TySafeArray _) e =
        (H.HsInfixApp
          (H.HsParen e)
          (H.HsQVarOp (H.UnQual (H.HsSymbol ">>=")))
          (mkVar "fromJSRefUnchecked")
        )
returnType _ (I.TySequence _ _) e =
        (H.HsInfixApp
          (H.HsParen e)
          (H.HsQVarOp (H.UnQual (H.HsSymbol ">>=")))
          (mkVar "fromJSRefUnchecked")
        )
returnType _ (I.TyOptional _) e =
        (H.HsInfixApp
          (H.HsParen e)
          (H.HsQVarOp (H.UnQual (H.HsSymbol ">>=")))
          (mkVar "fromJSRefUnchecked")
        )
returnType _ _ e = e

jsReturn :: I.Type -> JExpr -> JExpr
jsReturn (I.TyName "Bool" Nothing) e = [jmacroE| `(e)`?1:0 |]
jsReturn _ e = e

propExcept [] e = e
propExcept _ e = e
--propExcept _ e = H.HsApp (H.HsApp (mkVar "propagateGError") (mkVar "$"))
--                                   (H.HsLambda (H.SrcLoc "" 1 1) [H.HsPVar . H.HsIdent $ "errorPtr_"]
--                                           $ H.HsApp e (mkVar $ "errorPtr_"))

gtkName s =
    let lower = map toLower (U.toUnderscoreCamel s) in
    case stripPrefix "htmli_" lower of
        Just rest -> "html_i"++rest
        Nothing   -> case stripPrefix "x_path" lower of
                        Just rest -> "xpath"++rest
                        Nothing   -> case stripPrefix "web_kit" lower of
                                        Just rest -> "webkit"++rest
                                        Nothing   -> lower






















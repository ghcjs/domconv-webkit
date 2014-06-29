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
        mapM_ (hPutStrLn hh) . filter (not . isSuffixOf " if webkit-dom") $ lines current
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
                                    ++ ", webkit_dom_" ++ map toLower (underscore child) ++ "_get_type if webkit-dom"
                            hierarchy (n+4) child
                    _ -> return ()
        hierarchy 8 ""
        hClose hh
        renameFile hierarchyFile (hierarchyFile ++ ".old")
        renameFile (hierarchyFile ++ ".new") hierarchyFile

processIDL idl args = do
  let epopts = parseOptions ("-DLANGUAGE_GOBJECT=1":args)
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
  mapM_ (mapM_ putSplit . splitModule) (procmod modst')

  let getParent (a, (Right b):_) = (b, a)
      getParent (a, _) = ("", a)
  let unsupported = ["AbstractView","CSS2Properties","CSSCharsetRule","CSSFontFaceRule",
                     "CSSImportRule","CSSMediaRule","CSSPageRule","CSSPrimitiveValue",
                     "CSSStyleRule","CSSUnknownRule","CSSValueList","Counter",
                     "DOMImplementationCSS","DocumentCSS","DocumentEvent","DocumentRange",
                     "DocumentStyle","DocumentTraversal","DocumentView","DocumentWindow",
                     "DocumentationCSS","ElementCSSInlineStyle","EmbeddingElement",
                     "Entity","EventListener","HTMLAbbrElement","HTMLAcronymElement",
                     "HTMLAddressElement","HTMLBElement","HTMLBdoElement",
                     "HTMLBigElement","HTMLCenterElement","HTMLCiteElement",
                     "HTMLCodeElement","HTMLDdElement","HTMLDfnElement","HTMLDtElement",
                     "HTMLEmElement","HTMLIElement","HTMLIsIndexElement","HTMLKbdElement",
                     "HTMLNoframesElement","HTMLNoscriptElement","HTMLSElement",
                     "HTMLSampElement","HTMLSmallElement","HTMLSpanElement",
                     "HTMLStrikeElement","HTMLStrongElement","HTMLSubElement",
                     "HTMLSupElement","HTMLUElement","HTMLVarElement","KeyEvent",
                     "KeyboardEvent","LinkStyle","MutationEvent","Notation","RGBColor",
                     "Rect","TimerListener","ViewCSS","Window","XMLHttpRequest"]

      filterSupported = filter (not . flip elem unsupported . fst)
  return . map getParent . filterSupported $ M.toList prntmap


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

putSplit :: H.HsModule -> IO ()

putSplit mod@(H.HsModule _ modid _ _ _) = do
  let components = U.split '.' $ modName modid
      fixMods "import Graphics.UI.Gtk.WebKit.Types" = "{#import Graphics.UI.Gtk.WebKit.Types#}"
      fixMods l = l

  createDirectoryIfMissing True (concat $ intersperse "/" $ init components)
  Prelude.writeFile ((concat $ intersperse "/" components) ++ ".chs") . unlines . map fixMods . lines $ prettyPrint mod

-- Split a proto-module created by domLoop. All class, data, and instance definitions
-- remain in the "head" class. All methods are grouped by their `this' argument
-- context and placed into modules with the name of that context (first character removed).
-- All modules get the same imports that the "head" module has plus the "head" module itself.

splitModule :: H.HsModule -> [H.HsModule]

splitModule (H.HsModule _ modid mbexp imps decls) = headmod : submods where
  headns = modNS $ modName modid
  headmod = H.HsModule nullLoc modid headexp imps headdecls
  headdecls = datas ++ classes ++ instances
  headexp = Just $ map (mkEIdent . declname) (datas ++ classes)
  datas = filter datadecl decls
  datadecl (H.HsDataDecl _ _ _ _ _ _) = True
  datadecl (H.HsNewTypeDecl _ _ _ _ _ _) = True
  datadecl _ = False
  classes = filter classdecl decls
  classdecl (H.HsClassDecl _ _ _ _ _) = True
  classdecl _ = False
  instances = filter instdecl decls
  instdecl (H.HsInstDecl _ _ _ _ _) = True
  instdecl _ = False
  expname (H.HsEVar (H.UnQual (H.HsIdent s))) = s
  expname _ = ""
  declname (H.HsDataDecl _ _ (H.HsIdent s) _ _ _) = s
  declname (H.HsNewTypeDecl _ _ (H.HsIdent s) _ _ _) = s
  declname (H.HsClassDecl _ _ (H.HsIdent s) _ _) = s
  declname (H.HsTypeSig _ [H.HsIdent s] _) = s
  declname (H.HsFunBind [H.HsMatch _ (H.HsIdent s) _ _ _]) = s
  declname _ = ""
  mtsigs = filter methtsig (reverse decls)
  methtsig (H.HsTypeSig _ _ _) = True
  methtsig (H.HsFunBind _) = True
  methtsig _ = False
  corrn = drop 1 . dropWhile (/= '|')
  methcorrn (H.HsTypeSig x [H.HsIdent s] y) = H.HsTypeSig x [H.HsIdent (corrn s)] y
  methcorrn (H.HsFunBind [H.HsMatch x (H.HsIdent s) y z t]) =
    H.HsFunBind [H.HsMatch x (H.HsIdent (corrn s)) y z t]
  methcorrn z = z
  methassoc meth =
    let i = ns ++ takeWhile (/= '|') (declname meth)
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
                      "System.Glib.FFI"
                    , "System.Glib.UTFString"
                    , "Control.Applicative"
                    , "Graphics.UI.Gtk.WebKit.Types"
                    , "System.Glib.GError"
                    ] ++ eventImp iid))
               (H.HsFunBind [] : smdecls) where
      name = typeFor . reverse . takeWhile (/= '.') $ reverse iid
      subexp = map mkEIdent . nub $ (filter (not . isSuffixOf "'") $ map declname smdecls) ++
                [name, name ++ "Class", "castTo" ++ name, "gType" ++ name, "to" ++ name]
      eventImp "Graphics.UI.Gtk.WebKit.DOM.Event" = []
      eventImp "Graphics.UI.Gtk.WebKit.DOM.UIEvent" = []
      eventImp "Graphics.UI.Gtk.WebKit.DOM.MouseEvent" = []
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
  getparents i@(I.Interface _ supers _) = (getDef i, concat $ map parent supers)
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
typeFor  s = s

-- Convert an IDL module definition into Haskell module syntax

mod2mod :: DOMState -> I.Defn -> H.HsModule

mod2mod st md@(I.Module _ moddefs) =
  H.HsModule nullLoc (H.Module modid') (Just []) imps decls where
    modlst = ["Control.Monad"]
    modid' = renameMod $ getDef md
    imps = [] -- map mkModImport (map H.Module (modlst ++ imp st))
    intfs = filter intfOnly moddefs
    eqop op1 op2 = getDef op1 == getDef op2
    decls = types ++ classes ++ instances ++ methods ++ attrs ++ makers
    makers  = concat $ map intf2maker intfs
    classes = concat $ map intf2class intfs
    methods = concat $ map intf2meth intfs
    types = concat $ map intf2type intfs
    attrs = concat $ map intf2attr intfs
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

intf2inst pm intf@(I.Interface _ _ _) = self : parents where
  sid = getDef intf
  self = mkInstDecl sid sid
  parents = case M.lookup sid pm of
    Nothing -> []
    Just ess -> map (flip mkInstDecl sid) (map (either id id) ess)

intf2inst _ _ = []

-- For each interface found, define a newtype with the same name

intf2type :: I.Defn -> [H.HsDecl]


intf2type intf@(I.Interface _ _ _) =
  let typename = H.HsIdent (typeFor $ getDef intf) in
  [H.HsDataDecl nullLoc [] typename []
    [H.HsConDecl nullLoc typename []] []]

intf2type _ = []

-- Convert an Interface specification into a class specification

intf2class :: I.Defn -> [H.HsDecl]

intf2class intf@(I.Interface _ supers _) =
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
intfOnly (I.Interface _ _ cldefs) = True
intfOnly _ = False

-- A filter to select only constant definitions

constOnly :: I.Defn -> Bool
constOnly (I.Constant _ _ _ _) = True
constOnly _ = False

-- Collect all operations defined in an interface

collectOps :: I.Defn -> [I.Defn]

collectOps (I.Interface _ _ cldefs) =
  filter opsOnly cldefs

collectOps _ = []

-- Collect all constants defined in an interface

collectConst :: I.Defn -> [I.Defn]

collectConst (I.Interface _ _ cldefs) =
  filter constOnly cldefs

collectConst _ = []

-- Collect all attributes defined in an interface

collectAttrs :: I.Defn -> [I.Defn]

collectAttrs (I.Interface _ _ cldefs) =
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

intf2attr :: I.Defn -> [H.HsDecl]

intf2attr intf@(I.Interface (I.Id iid) _ cldefs) =
  concat $ map mkattr $ collectAttrs intf where
    mkattr (I.Attribute [] _ _ _ _) = []
    mkattr (I.Attribute _ _ (I.TyName "MediaQueryListListener" _) _ _) = []
    mkattr (I.Attribute _ _ (I.TyName "Crypto" _) _ _) = []
    mkattr (I.Attribute [I.Id "type"] _ _ _ _) = []
    mkattr (I.Attribute [I.Id "URL"] _ _ _ _) = []
    mkattr (I.Attribute [I.Id "location"] _ _ _ _) = []
    mkattr (I.Attribute [I.Id "valueAsDate"] _ _ _ _) = []
    mkattr (I.Attribute [I.Id "webkitPeerConnection"] _ _ _ _) = []
    mkattr (I.Attribute _ _ _ _ ext) | I.ExtAttr (I.Id "Custom") `elem` ext = []
    mkattr (I.Attribute _ _ _ _ ext) | I.ExtAttr (I.Id "CustomSetter") `elem` ext = []
    mkattr (I.Attribute _ _ _ _ ext) | I.ExtAttr (I.Id "CustomGetter") `elem` ext = []
    mkattr (I.Attribute [I.Id iat] _ (I.TyName "EventListener" _) _ _) = mkevent iid iat
    mkattr (I.Attribute [I.Id iat] False tat raises ext) =
      (if I.ExtAttr (I.Id "Replaceable") `elem` ext
        then []
        else mksetter iid iat tat raises ext)
      ++ mkgetter iid iat tat raises ext
    mkattr (I.Attribute [I.Id iat] True  tat raises ext) = mkgetter iid iat tat raises ext
    mkattr (I.Attribute (iatt:iats) b tat raises ext) =
      mkattr (I.Attribute [iatt] b tat raises ext) ++ mkattr (I.Attribute iats b tat raises ext)
    mksetter iid iat tat r ext = [stsig iid iat tat, simpl iid iat tat r ext]
    monadtv = mkTIdent "IO"
    setf intf iat = U.toLowerInitCamel $ getDef intf ++ "Set" ++ U.toUpperHead iat
    getf intf iat = U.toLowerInitCamel $ getDef intf ++ "Get" ++ U.toUpperHead iat
    eventName iat = maybe iat id (stripPrefix "on" iat)
    eventf intf iat = U.toLowerInitCamel $ getDef intf ++ U.toUpperHead iat
    simpl iid iat tat raises ext =
      let defset = iid ++ "|" ++ setf intf iat
          ffi = foldl H.HsApp (mkVar "{#") [H.HsVar . H.UnQual . H.HsIdent $ "call",
                H.HsVar . H.UnQual . H.HsIdent $ "webkit_dom_" ++ gtkName (setf intf iat),
                mkVar "#}"]
          parms = [H.HsPVar $ H.HsIdent "self", H.HsPVar $ H.HsIdent "val"]
          call = (H.HsApp ffi . H.HsParen $ H.HsApp
            (H.HsVar . H.UnQual . H.HsIdent $ "to" ++ typeFor (getDef intf))
            (H.HsVar . H.UnQual $ H.HsIdent "self"))
          val = I.Param I.Required (I.Id "val") tat [I.Mode In]
          canRaise = (not $ null (I.setterRaises raises)) || (I.ExtAttr $ I.Id "SetterRaisesException") `elem` ext
          rhs = H.HsUnGuardedRhs $ propExcept canRaise $ applyParam val call
          match = H.HsMatch nullLoc (H.HsIdent defset) parms rhs [] in
      H.HsFunBind [match]
    stsig iid iat tat =
      let ityp = I.TyName iid Nothing
          defset = iid ++ "|" ++ setf intf iat
          parm = [I.Param I.Required (I.Id "val") tat [I.Mode In]]
          parms = mkTIdent "self" : (map (fst . tyParm) parm)
          contxt = (concat $ map (snd . tyParm) parm) ++ ctxRet ityp ++ ctxString (ityp:map paramType parm)
          tpsig = mkTsig parms (H.HsTyApp monadtv $ H.HsTyCon (H.Special H.HsUnitCon))
          retts = H.HsQualType contxt tpsig in
      H.HsTypeSig nullLoc [H.HsIdent defset] retts
    mkgetter iid iat tat r ext = [gtsig iid iat tat, gimpl iid iat tat r ext]
    gimpl iid iat tat raises ext =
      let defget = iid ++ "|" ++ getf intf iat
          ffi = foldl H.HsApp (mkVar "{#") [H.HsVar . H.UnQual . H.HsIdent $ "call",
                H.HsVar . H.UnQual . H.HsIdent $ "webkit_dom_" ++ gtkName (getf intf iat),
                mkVar "#}"]
          parm = H.HsPVar $ H.HsIdent "self"
          call = (H.HsApp ffi . H.HsParen $ H.HsApp
            (H.HsVar . H.UnQual . H.HsIdent $ "to" ++ typeFor (getDef intf))
            (H.HsVar . H.UnQual $ H.HsIdent "self"))
          canRaise = (not $ null (I.getterRaises raises)) || (I.ExtAttr $ I.Id "GetterRaisesException") `elem` ext
          rhs = H.HsUnGuardedRhs $ returnType tat $ propExcept canRaise call
          match = H.HsMatch nullLoc (H.HsIdent defget) [parm] rhs [] in
      H.HsFunBind [match]
    gtsig iid iat tat =
      let ityp = I.TyName iid Nothing
          defget = iid ++ "|" ++ getf intf iat
          parms = [H.HsIdent "self"]
          contxt = ctxRet ityp ++ ctxString [ityp,tat]
          tpsig = mkTsig (map H.HsTyVar parms)
                         (H.HsTyApp monadtv $ tyRet tat)
          retts = H.HsQualType contxt tpsig in
      H.HsTypeSig nullLoc [H.HsIdent defget] retts
    mkevent iid iat = [eventtsig iid iat, eventimpl iid iat]
    eventimpl iid iat =
      let defget = iid ++ "|" ++ eventf intf iat
          ffi = foldl H.HsApp (mkVar "{#") [H.HsVar . H.UnQual . H.HsIdent $ "call",
                H.HsVar . H.UnQual . H.HsIdent $ "webkit_dom_" ++ gtkName (getf intf iat),
                mkVar "#}"]
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
      let ityp = I.TyName iid Nothing
          defget = iid ++ "|" ++ eventf intf iat
          contxt = ctxRet ityp ++ ctxString [ityp]
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



intf2attr _ = []

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

intf2meth :: I.Defn -> [H.HsDecl]

intf2meth intf@(I.Interface _ _ cldefs) =
  (concat $ map mkmeth $ collectOps intf) ++
  (concat $ map mkconst $ collectConst intf) where
    getDefHs op = getDef op
    getDefJs op@(I.Operation _ _ _ mbctx _) = case mbctx of
      Nothing -> getDef op
      Just [] -> getDef op
      Just (s:_) -> s
    mkconst cn@(I.Constant (I.Id cid) _ _ (I.Lit (IntegerLit (ILit base val)))) =
      let defcn = getDef intf ++ "|c" ++ cid
          match = H.HsMatch nullLoc (H.HsIdent defcn) [] crhs []
          crhs = H.HsUnGuardedRhs (H.HsLit (H.HsInt val))
      in  [H.HsFunBind [match]]
    mkmeth op | getDef op `elem` ["getCSSCanvasContext", "getSVGDocument"] = []
    mkmeth (I.Operation _ _ _ _ ext) | not (getDef intf `elem` ["Node"]) && I.ExtAttr (I.Id "Custom") `elem` ext = []
    mkmeth (I.Operation _ _ _ _ ext) | I.ExtAttr (I.Id "V8EnabledAtRuntime") `elem` ext = []
    mkmeth (I.Operation _ _ _ _ ext) | I.ExtAttr (I.Id "CallWith") `elem` ext = []
    mkmeth (I.Operation (I.FunId (I.Getter) _ _) _ _ _ _) = []
    mkmeth op | skip op = []
    mkmeth op = tsig op : timpl op
    tsig op@(I.Operation (I.FunId _ _ parm) optype _ _ _) =
      let monadtv = mkTIdent "IO"
          -- exprtv = mkTIdent "Expression"
          defop = getDef intf ++ "|" ++ (U.toLowerInitCamel $ getDef intf) ++ (U.toUpperHead $ getDefHs op)
          parms =  mkTIdent "self" : (map (fst . tyParm) parm)
          contxt = (concat $ map (snd . tyParm) parm) ++ ctxString (optype:map paramType parm)
          -- monadctx = (mkUIdent "Monad",[monadtv])
          thisctx = (mkUIdent (classFor $ getDef intf),[mkTIdent "self"])
          tpsig = mkTsig parms (H.HsTyApp monadtv (tyRet optype))
          retts = H.HsQualType (thisctx : contxt) tpsig in
      H.HsTypeSig nullLoc [H.HsIdent defop] retts
    timpl op@(I.Operation (I.FunId _ _ parm) optype raises _ attrib) =
      let defop = getDef intf ++ "|" ++ (U.toLowerInitCamel $ getDef intf) ++ (U.toUpperHead $ getDefHs op)
          ffi = foldl H.HsApp (mkVar "{#") [H.HsVar . H.UnQual . H.HsIdent $ "call",
                H.HsVar . H.UnQual . H.HsIdent $ "webkit_dom_" ++ gtkName (getDef intf ++ U.toUpperHead (getDefHs op)),
                mkVar "#}"]
          parms = map H.HsPVar (H.HsIdent "self" : map paramName parm)
          call = (H.HsApp ffi . H.HsParen $ H.HsApp
            (H.HsVar . H.UnQual . H.HsIdent $ "to" ++ typeFor (getDef intf))
            (H.HsVar . H.UnQual $ H.HsIdent "self"))
          -- params' = map (\I.Param (I.Id "val") tat [I.Mode In] parm
          canRaise = (not $ null raises) || (I.ExtAttr $ I.Id "RaisesException") `elem` attrib
          rhs = H.HsUnGuardedRhs $ returnType optype $ propExcept canRaise $ L.foldl (flip applyParam) call parm
--          rhs = H.HsUnGuardedRhs $ mkMethod (getDefJs op) parms (tyRet optype)
          match  = H.HsMatch nullLoc (H.HsIdent defop) parms rhs []
      in  [H.HsFunBind [match]]
    skip (I.Operation (I.FunId _ _ parm) _ _ _ _) = any excludedParam parm
    excludedParam (I.Param _ _ (I.TyName "EventListener" _) _) = True
    excludedParam (I.Param _ _ (I.TyName "MediaQueryListListener" _) _) = True
    excludedParam _ = False

intf2meth _ = []

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


-- Obtain a return type signature from a return type

tyRet :: I.Type -> H.HsType

tyRet (I.TyName c Nothing) = case (asIs c) of
  Nothing -> H.HsTyApp (mkTIdent "Maybe") (mkTIdent $ typeFor c) -- H.HsTyCon $ H.Special H.HsUnitCon
  Just c' -> mkTIdent c'
tyRet  I.TyVoid  = H.HsTyTuple []
tyRet (I.TyInteger LongLong) = mkTIdent "Int64"
tyRet (I.TyInteger _) = mkTIdent "Int"
tyRet (I.TyFloat Short) = mkTIdent "Float"
tyRet (I.TyFloat _) = mkTIdent "Double"
tyRet (I.TyApply (I.TySigned False) (I.TyInteger LongLong)) = mkTIdent "Word64"
tyRet (I.TyApply (I.TySigned False) (I.TyInteger _)) = mkTIdent "Word"
tyRet (I.TyApply _ (I.TyInteger LongLong)) = mkTIdent "Int64"
tyRet (I.TyApply _ (I.TyInteger _)) = mkTIdent "Int"
tyRet (I.TyObject) = mkTIdent "GObject"
tyRet t = error $ "Return type " ++ (show t)

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

cnRet :: I.Type -> H.HsType

cnRet (I.TyName c Nothing) = case asIs c of
  Nothing -> mkTIdent ('T' : c)
  Just c' -> mkTIdent c'
cnRet z = tyRet z

-- Obtain a return type context (if any) from a return type

ctxRet :: I.Type -> [H.HsAsst]

ctxRet (I.TyName c Nothing) = case (asIs c) of
  Nothing -> [(mkUIdent $ classFor c, [mkTIdent "self"])]
  Just c' -> []

ctxRet _ = []

ctxString :: [I.Type] -> [H.HsAsst]
ctxString types | I.TyName "DOMString" Nothing `elem` types =
    [(mkUIdent $ "GlibString", [mkTIdent "string"])]
ctxString _ = []

paramType (I.Param _ _ ptype _) = ptype

-- Obtain a type signature from a parameter definition

tyParm :: I.Param -> (H.HsType, [H.HsAsst])

tyParm (I.Param opt (I.Id p) ptype [I.Mode In]) =
  case ptype of
    I.TyName c Nothing -> case asIs c of
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
    t -> error $ "Param type " ++ (show t)

tyParm param@(I.Param _ _ _ _) = error $ "Unsupported parameter attributes " ++ show param

-- Some types pass through as is, other are class names

asIs :: String -> Maybe String

asIs "DOMString"    = Just "string"
asIs "DOMTimeStamp" = Just "Word"
asIs "CompareHow"   = Just "Word"
asIs "Bool"         = Just "Bool"
asIs "Int"          = Just "Int"
asIs _              = Nothing

paramName (I.Param _ (I.Id "data") _ _)  = H.HsIdent "data'"
paramName (I.Param _ (I.Id "type") _ _)  = H.HsIdent "type'"
paramName (I.Param _ (I.Id "where") _ _) = H.HsIdent "where'"
paramName (I.Param _ (I.Id p) _ _) = H.HsIdent p

-- Apply a parameter to a FFI call

applyParam :: I.Param -> H.HsExp -> H.HsExp

applyParam param@(I.Param _ (I.Id p) ptype [I.Mode In]) call =
  let pname =  H.HsVar . H.UnQual $ paramName param in
  case ptype of
    I.TyName "DOMString" Nothing -> H.HsApp (H.HsApp (H.HsApp (mkVar "withUTFString") pname) (mkVar "$"))
                                   (H.HsLambda (H.SrcLoc "" 1 1) [H.HsPVar . H.HsIdent $ p ++ "Ptr"]
                                           $ H.HsApp call (mkVar $ p ++ "Ptr"))
    I.TyName "DOMTimeStamp" Nothing -> H.HsApp call (H.HsParen $ H.HsApp (mkVar $ "fromIntegral") pname)
    I.TyName "CompareHow" Nothing -> H.HsApp call (H.HsParen $ H.HsApp (mkVar $ "fromIntegral") pname)
    I.TyName "Bool" Nothing -> H.HsApp call (H.HsParen $ H.HsApp (mkVar $ "fromBool") pname)
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
    t -> error $ "Param type " ++ (show t)

applyParam param@(I.Param _ _ _ _) _ = error $ "Unsupported parameter attributes " ++ show param

returnType :: I.Type -> H.HsExp -> H.HsExp
returnType (I.TyName "DOMString" Nothing) e = H.HsApp (H.HsApp (H.HsParen e) (mkVar ">>=")) (mkVar "readUTFString")
returnType (I.TyName "DOMTimeStamp" Nothing) e = H.HsApp (H.HsApp (mkVar "fromIntegral") (mkVar "<$>")) (H.HsParen e)
returnType (I.TyName "CompareHow" Nothing) e = H.HsApp (H.HsApp (mkVar "fromIntegral") (mkVar "<$>")) (H.HsParen e)
returnType (I.TyName "Bool" Nothing) e = H.HsApp (H.HsApp (mkVar "toBool") (mkVar "<$>")) (H.HsParen e)
returnType (I.TyName x Nothing) e =
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
returnType (I.TyInteger _) e = H.HsApp (H.HsApp (mkVar "fromIntegral") (mkVar "<$>")) (H.HsParen e)
returnType (I.TyFloat _) e = H.HsApp (H.HsApp (mkVar "realToFrac") (mkVar "<$>")) (H.HsParen e)
returnType (I.TyApply _ (I.TyInteger _)) e = H.HsApp (H.HsApp (mkVar "fromIntegral") (mkVar "<$>")) (H.HsParen e)
returnType t e = e

propExcept False e = e
propExcept True e = H.HsApp (H.HsApp (mkVar "propagateGError") (mkVar "$"))
                                   (H.HsLambda (H.SrcLoc "" 1 1) [H.HsPVar . H.HsIdent $ "errorPtr_"]
                                           $ H.HsApp e (mkVar $ "errorPtr_"))

gtkName s =
    let lower = map toLower (U.toUnderscoreCamel s) in
    case stripPrefix "htmli_" lower of
        Just rest -> "html_i"++rest
        Nothing   -> case stripPrefix "x_path" lower of
                        Just rest -> "xpath"++rest
                        Nothing   -> case stripPrefix "web_kit" lower of
                                        Just rest -> "webkit"++rest
                                        Nothing   -> lower






















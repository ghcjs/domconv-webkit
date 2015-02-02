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
import Language.Javascript.JMacro (jmacro, jmacroE, jLam, JExpr(..), JVal(..),
  Ident(..), jVarTy, JStat(..), toJExpr, renderJs, jsv)
import Data.Monoid (mconcat)

main = do
  putStrLn "domconv-webkit-js : Makes JavaScript alternatives for webkitgtk"
  p <- getProgName
  args <- getArgs
  case args of
    [] -> putStrLn "Usage: domconv-webkit webkit.idl -Iwebkit-1.8.0/Source/WebCore"
    idl:args -> makeWebkitBindings idl args

makeWebkitBindings idl args = do
  putStrLn $ "Processing IDL: " ++ idl ++ " args " ++ show args
  processIDL idl args
  let underscore "HTMLIFrameElement" = "html_iframe_element"
      underscore "XPathExpression" = "xpath_expression"
      underscore "XPathNSResolver" = "xpath_ns_resolver"
      underscore "XPathResult" = "xpath_result"
      underscore "WebKitNamedFlow" = "webkit_named_flow"
      underscore "WebKitPoint" = "webkit_point"
      underscore "WebKitAnimation" = "webkit_animation"
      underscore "WebKitAnimationList" = "webkit_animation_list"
      underscore c = U.toUnderscoreCamel c
--      hierarchy n parent =
--        case M.lookup parent l of
--            Just s -> do
--                forM_ (S.toList s) $ \child -> do
--                    hPutStrLn hh $ replicate n ' ' ++ "WebKitDOM" ++ child ++ " as " ++ typeFor child
--                            ++ ", webkit_dom_" ++ map toLower (underscore child) ++ "_get_type if webkit-dom"
--                    hierarchy (n+4) child
--            _ -> return ()
  exitWith (ExitSuccess)

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
  f <- openFile "webkit-dom.js" WriteMode
  forM_ (procmod modst') $ \ (interface, js) -> do
    hPutStrLn f $ "// " ++ interface
    hPutStrLn f . show $ renderJs js
  hClose f
  return ()

-- Get a module namespace (all elements of name separated with dots except
-- the last one)

modNS :: String -> String

modNS mn = concat $ intersperse "." mnpts where
  mnpts = case (reverse $ parts (== '.') mn) of
    [] -> []
    [_] -> []
    (p:ps) -> reverse ps

-- Write a module surrounded by split begin/end comments



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
  getparents i@(I.Interface _ supers _ _ _) = (getDef i, concat $ map parent supers)
  parent pidf = case (pidf `M.member` m1) of
    True  -> (Right pidf) : snd (getparents (fromJust $ M.lookup pidf m1))
    False -> [Left pidf]


-- A list of single-letter formal argument names (max. 26)

azList = map (: []) ['a' .. 'z']

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
  ,procmod :: [(String, JStat)]              -- modules already processed
  ,convlog :: [String]                       -- conversion messages
} deriving (Show)


-- Helpers to produce class and datatype identifiers out of DOM identifiers

classFor s = typeFor s ++ "Class"
typeFor  "Range" = "DOMRange"
typeFor  "Screen" = "DOMScreen"
typeFor  "Attr" = "DOMAttr"
typeFor  s = s

-- Convert an IDL module definition into Haskell module syntax

mod2mod :: DOMState -> I.Defn -> (String, JStat)

mod2mod st md@(I.Module _ moddefs) = (renameMod $ getDef md, mconcat decls)
  where
    intfs = filter intfOnly moddefs
    decls = types ++ methods ++ attrs
    methods = concat $ map intf2meth intfs
    types = concat $ map intf2type intfs
    attrs = concat $ map intf2attr intfs

mod2mod _ z = error $ "Input of mod2mod should be a Module but is " ++ show z

-- For each interface found, define a newtype with the same name

intf2type :: I.Defn -> [JStat]
intf2type intf@(I.Interface _ _ _ _ _) = [[jmacro| `(jsv functionName)` = \ -> h$g_get_type(`(jsv typeName)`) |]]
  where
    functionName = "h$webkit_dom_" ++ gtkName (getDef intf) ++ "_get_type"
    typeName = jsType $ getDef intf
    jsType "DOMWindow" = "Window"
    jsType n = n

intf2type _ = []


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

-- For certain interfaces (ancestors of HTMLElement), special maker functions
-- are introduced to simplify creation of the formers.

-- Attributes are represented by methods with proper type signatures.
-- These methods are wrappers around type-neutral unsafe get/set property
-- functions.

intf2attr :: I.Defn -> [JStat]

intf2attr intf@(I.Interface (I.Id iid) _ cldefs _ _) =
    concat $ map mkattr $ collectAttrs intf
  where
    mkattr (I.Attribute [] _ _ _ _) = []
    mkattr (I.Attribute _ _ (I.TyName "MediaQueryListListener" _) _ _) = []
    mkattr (I.Attribute _ _ (I.TyName "Crypto" _) _ _) = []
    mkattr (I.Attribute [I.Id "type"] _ _ _ _) = []
    mkattr (I.Attribute [I.Id "URL"] _ _ _ _) = []
    mkattr (I.Attribute [I.Id "location"] _ _ _ _) = []
    mkattr (I.Attribute [I.Id "valueAsDate"] _ _ _ _) = []
    mkattr (I.Attribute [I.Id "webkitPeerConnection"] _ _ _ _) = []
    mkattr (I.Attribute _ _ _ _ ext) | I.ExtAttr (I.Id "Custom") [] `elem` ext = []
    mkattr (I.Attribute _ _ _ _ ext) | I.ExtAttr (I.Id "CustomSetter") [] `elem` ext = []
    mkattr (I.Attribute _ _ _ _ ext) | I.ExtAttr (I.Id "CustomGetter") [] `elem` ext = []
    mkattr (I.Attribute [I.Id iat] False tat raises ext) =
      (if I.ExtAttr (I.Id "Replaceable") [] `elem` ext
        then []
        else mksetter iid iat tat raises)
      ++ mkgetter iid iat tat raises
    mkattr (I.Attribute [I.Id iat] True  tat raises _) = mkgetter iid iat tat raises
    mkattr (I.Attribute (iatt:iats) b tat raises ext) =
      mkattr (I.Attribute [iatt] b tat raises ext) ++ mkattr (I.Attribute iats b tat raises ext)
    mksetter iid iat tat r = [simpl iid iat tat r]
    setf intf iat = U.toLowerInitCamel $ getDef intf ++ "Set" ++ U.toUpperHead iat
    getf intf iat = U.toLowerInitCamel $ getDef intf ++ "Get" ++ U.toUpperHead iat
    eventName iat = maybe iat id (stripPrefix "on" iat)
    eventf intf iat = U.toLowerInitCamel $ getDef intf ++ U.toUpperHead iat
    simpl iid iat tat raises =
        BlockStat [ DeclStat (StrI ffi) Nothing,
           [jmacro| `(jsv ffi)` = `(func)` |] ]
      where
        ffi = "h$webkit_dom_" ++ gtkName (setf intf iat)
        func = JFunc (map StrI $ ["self", "self_2"] ++ paramName val) [jmacro| self[`(iat)`] = `(rhs)`; |]
        rhs = applyParam val
        val = I.Param I.Required (I.Id "val") tat [I.Mode In]
    mkgetter iid iat tat r = [gimpl iid iat tat r]
    gimpl iid iat tat raises =
        BlockStat [ DeclStat (StrI ffi) Nothing,
           [jmacro| `(jsv ffi)` = `(func)` |] ]
      where
        ffi = "h$webkit_dom_" ++ gtkName (getf intf iat)
        func = JFunc [StrI "self", StrI "self_2"] call
        call = returnType tat $ [jmacroE| self[`(iat)`] |]

intf2attr _ = []

intf2meth :: I.Defn -> [JStat]

intf2meth intf@(I.Interface _ _ cldefs _ _) =
  (concat $ map mkmeth $ collectOps intf) ++
  (concat $ map mkconst $ collectConst intf) where
    getDefHs op = getDef op
    getDefJs op@(I.Operation _ _ _ mbctx _) = case mbctx of
      Nothing -> getDef op
      Just [] -> getDef op
      Just (s:_) -> s
    mkconst cn@(I.Constant (I.Id cid) _ _ (I.Lit (IntegerLit (ILit base val)))) = []
    mkmeth op | getDef op `elem` ["getCSSCanvasContext", "getSVGDocument"] = []
    mkmeth (I.Operation _ _ _ _ ext) | not (getDef intf `elem` ["Node"]) && I.ExtAttr (I.Id "Custom") [] `elem` ext = []
    mkmeth (I.Operation _ _ _ _ ext) | I.ExtAttr (I.Id "V8EnabledAtRuntime") [] `elem` ext = []
    mkmeth (I.Operation _ _ _ _ ext) | I.ExtAttr (I.Id "CallWith") [] `elem` ext = []
    mkmeth op | skip op = []
    mkmeth op = [timpl op]
    timpl op@(I.Operation (I.FunId _ _ parm) optype raises _ _) =
        BlockStat [ DeclStat (StrI ffi) Nothing,
           [jmacro| `(jsv ffi)` = `(func)` |] ]
      where
        func = JFunc (StrI "self" : StrI "self_2" : concatMap (map StrI . paramName) parm) call
        call = returnType optype $ ApplExpr [jmacroE| self[`(getDef op)`] |]
                  (map applyParam parm)
        ffi = "h$webkit_dom_" ++ gtkName (getDef intf ++ U.toUpperHead (getDefHs op))
    skip (I.Operation (I.FunId _ _ parm) _ _ _ _) = any excludedParam parm
    excludedParam (I.Param _ _ (I.TyName "EventListener" _) _) = True
    excludedParam (I.Param _ _ (I.TyName "MediaQueryListListener" _) _) = True
    excludedParam _ = False

intf2meth _ = []

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

-- Some types pass through as is, other are class names

asIs :: String -> Maybe String

asIs "DOMString"    = Just "String"
asIs "DOMTimeStamp" = Just "Word"
asIs "CompareHow"   = Just "Word"
asIs "Bool"         = Just "Bool"
asIs "Int"          = Just "Int"
asIs _              = Nothing

paramName param@(I.Param _ (I.Id p) ptype [I.Mode In]) =
  case ptype of
    I.TyName "DOMString" Nothing -> [p,p++"_2"]
    I.TyName "DOMTimeStamp" Nothing -> [p]
    I.TyName "CompareHow" Nothing -> [p]
    I.TyName "Bool" Nothing -> [p]
    I.TyName _ Nothing -> [p,p++"_2"]
    I.TyInteger _ -> [p]
    I.TyFloat _   -> [p]
    I.TyApply _ (I.TyInteger _) -> [p]
    t -> error $ "Param type " ++ (show t)

-- Apply a parameter to a FFI call

applyParam :: I.Param -> JExpr

applyParam param@(I.Param _ (I.Id p) ptype [I.Mode In]) =
  case ptype of
    I.TyName "DOMString" Nothing -> [jmacroE| h$decodeUtf8z(`(jsv p)`,`(jsv $ p ++ "_2")`) |]
    I.TyName "DOMTimeStamp" Nothing -> jsv p
    I.TyName "CompareHow" Nothing -> jsv p
    I.TyName "Bool" Nothing -> jsv p
    I.TyName x Nothing -> jsv p
    I.TyInteger _ -> jsv p
    I.TyFloat _   -> jsv p
    I.TyApply _ (I.TyInteger _) -> jsv p
    t -> error $ "Param type " ++ (show t)

applyParam param@(I.Param _ _ _ _) = error $ "Unsupported parameter attributes " ++ show param

returnType :: I.Type -> JExpr -> JStat
returnType (I.TyName "DOMString" Nothing) e = [jmacro| h$ret1=0; return h$encodeUtf8(`(e)`) |]
returnType (I.TyName "DOMTimeStamp" Nothing) e = [jmacro| return `(e)` |]
returnType (I.TyName "CompareHow" Nothing) e = [jmacro| return `(e)` |]
returnType (I.TyName "Bool" Nothing) e = [jmacro| return `(e)`?1:0 |]
returnType (I.TyName x Nothing) e = [jmacro|  h$ret1=0; return `(e)` |]
returnType (I.TyInteger _) e = [jmacro| return `(e)` |]
returnType (I.TyFloat _) e = [jmacro| return `(e)` |]
returnType (I.TyApply _ (I.TyInteger _)) e = [jmacro| return `(e)` |]
returnType t e = [jmacro| return `(e)` |]

gtkName s =
    let lower = map toLower (U.toUnderscoreCamel s) in
    case stripPrefix "htmli_" lower of
        Just rest -> "html_i"++rest
        Nothing   -> case stripPrefix "x_path" lower of
                        Just rest -> "xpath"++rest
                        Nothing   -> case stripPrefix "web_kit" lower of
                                        Just rest -> "webkit"++rest
                                        Nothing   -> lower






















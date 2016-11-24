{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiWayIf #-}
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

getDef = jsname' . IDLUtils.getDef

callNew "AudioContext" = [jmacroE| new (window["AudioContext"] || window["webkitAudioContext"]) |]
callNew x = [jmacroE| new window[`(jsname x)`] |]

main = do
  putStrLn "domconv-webkit : Makes Gtk2Hs Haskell bindings for webkitgtk"
  p <- getProgName
  args <- getArgs
  case args of
    [] -> putStrLn "Usage: domconv-webkit webkit.idl -Iwebkit-1.8.0/Source/WebCore"
    idl:args -> makeWebkitBindings idl args

makeWebkitBindings idl args = do
    putStrLn "The package will be created in the current directory which has to be empty."
    putStrLn $ "Processing IDL: " ++ idl ++ " args " ++ show args
    prntmap <- processIDL idl args
    -- let reversedMap = M.fromListWith S.union $ map (\(a,b)->(a,S.singleton b)) prntmap
    fixHierarchy prntmap "src/JSDOM/Types.hs" ffiTypes
    exitSuccess
  where
    fixHierarchy prntmap hierarchyFile printTypes = do
        hh <- openFile (hierarchyFile ++ ".new") WriteMode
        current <- readFile hierarchyFile
        let startGuard = "-- AUTO GENERATION STARTS HERE"
            endGuard   = "-- AUTO GENERATION ENDS HERE"
            (start, rest) = span (/= startGuard) $ lines current
            middle = takeWhile (/= startGuard) $ dropWhile (/= endGuard) rest
            allParents = nub $ concatMap (rights . snd) prntmap
        mapM_ (hPutStrLn hh) start
        hPutStrLn hh startGuard
        forM_ prntmap $ \(n, parents) -> hPutStrLn hh $ ffiExports (typeFor n) allParents
--        hPutStrLn hh $ "#else\n"
--            ++ "    propagateGError, GType(..), DOMString(..), ToDOMString(..), FromDOMString(..)\n"
--            ++ "  , FocusEvent\n"
--            ++ "  , TouchEvent\n"
--            ++ "  , module Graphics.UI.Gtk.WebKit.Types\n"
--            ++ "  , IsGObject\n"
--        forM_ prntmap $ \(n, parents) -> when (inWebKitGtk $ typeFor n) . hPutStr hh .
--             oldWebKitGuard (typeFor n) $ "  , Is" ++ typeFor n ++ "\n"
        mapM_ (hPutStrLn hh) middle
        hPutStrLn hh startGuard
        hPutStrLn hh "-- The remainder of this file is generated from IDL files using domconv-webkit-jsffi"
        printTypes hh prntmap allParents

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

    ffiExports name allParents =
            "  , " ++ name ++ "(" ++ name ++ "), un" ++ name
            ++ (if name `elem` allParents then ", Is" ++ name ++ ", to" ++ name else "")
            ++ ", " ++ "gType" ++ name

    ffiTypes hh prntmap allParents =
        forM_ prntmap $ \(n, parents) -> hPutStrLn hh $
            let name = typeFor n in
            "#if (defined(ghcjs_HOST_OS) && defined(USE_JAVASCRIPTFFI)) || !defined(USE_WEBKIT)\n"
            ++ "-- | Functions for this inteface are in \"JSDOM." ++ name ++ "\".\n"
            ++ (
                if null parents
                    then ""
                    else "-- Base interface functions are in:\n"
                            ++ "--\n"
                            ++ concatMap (\parent -> "--     * \"JSDOM." ++ parent ++ "\"\n") (rights parents)
            )
            ++ "--\n"
            ++ "-- <https://developer.mozilla.org/en-US/docs/Web/API/"
                       ++ jsname name ++ " Mozilla " ++ jsname name ++ " documentation>\n"
            ++ "newtype " ++ name ++ " = " ++ name ++ " { un" ++ name ++ " :: JSVal }\n\n"
--            ++ "instance Eq (" ++ name ++ ") where\n"
--            ++ "  (" ++ name ++ " a) == (" ++ name ++ " b) = js_eq a b\n\n"

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
            ++ "  fromJSVal v = fmap " ++ name ++ " <$> maybeNullOrUndefined v\n"
            ++ "  {-# INLINE fromJSVal #-}\n\n"

            ++ "instance MakeObject " ++ name ++ " where\n"
            ++ "  makeObject = makeObject . un" ++ name ++ "\n\n"

            ++ (if name `elem` allParents
                    then "class " ++ head (map ("Is"++) (rights parents) ++ ["IsGObject"]) ++ " o => Is" ++ name ++ " o\n"
                            ++ "to" ++ name ++ " :: Is" ++ name ++ " o => o -> " ++ name ++ "\n"
                            ++ "to" ++ name ++ " = " ++ name ++ " . coerce\n\n"
                            ++ "instance Is" ++ name ++ " " ++ name ++ "\n"
                    else "")

            ++ concatMap (\parent -> "instance Is" ++ parent ++ " " ++ name ++ "\n") (rights parents)
            ++ "instance IsGObject " ++ name ++ " where\n"
            ++ "  typeGType _ = gType" ++ name ++ "\n"
            ++ "  {-# INLINE typeGType #-}\n\n"

            ++ "gType" ++ name ++ " :: JSM GType\n"
            ++ "gType" ++ name ++ " = GType . Object <$> jsg \"" ++ jsname name ++ "\"\n"
            ++ (if inWebKitGtk name then "#else\n" ++ oldWebKitGuard name ("type Is" ++ name ++ " o = " ++ name ++ "Class o\n") else "")
            ++ "#endif\n\n"
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
        ,ns = "JSDOM." -- this will be the default namespace unless a pragma namespace is used.
        ,procmod = []
        ,convlog = []
      }
      modst' = domLoop modst x
--  mapM_ (putStrLn .show) $ (procmod modst')
  mapM_ (mapM_ putSplit . splitModule allParents) (procmod modst')

  let getParent (a, Right b : _) = (b, a)
      getParent (a, _) = ("", a)

  return $ M.toList prntmap

getEnums (I.TypeDecl (I.TyEnum (Just (I.Id typename)) _)) = [typename]
getEnums (I.Module _ defs) = concatMap getEnums defs
getEnums _ = []

getAllInterfaces (I.Interface (I.Id name) _ _ _ _) = [jsname' name]
getAllInterfaces (I.Module _ defs) = concatMap getAllInterfaces defs
getAllInterfaces _ = []

getParents (I.Interface _ names _ _ _) = map jsname' names
getParents (I.Module _ defs) = concatMap getParents defs
getParents _ = []

-- Write a module surrounded by split begin/end comments
{-
maybeNull = ()
makeNewGObject = ()
mkEventTarget = castRef
-}

putSplit :: (H.HsModule, String -> Maybe String) -> IO ()

putSplit (H.HsModule loc modid exp imp decl, comment) = do
  let components = U.split '.' $ modName modid
      name = components !! 1

  createDirectoryIfMissing True "src/JSDOM"
  createDirectoryIfMissing True "src/JSDOM/Generated"

  customFileExists <- doesFileExist $ "src/JSDOM/Custom" </> name ++ ".hs"
  let jsffiModule = "JSDOM." ++ (if customFileExists then "Custom." else "Generated.") ++ name

  Prelude.writeFile ("src/JSDOM/Generated" </> name ++ ".hs") $
        "{-# LANGUAGE PatternSynonyms #-}\n"
     ++ "-- For HasCallStack compatibility\n"
     ++ "{-# LANGUAGE ImplicitParams, ConstraintKinds, KindSignatures #-}\n"
     ++ "{-# OPTIONS_GHC -fno-warn-unused-imports #-}\n"
     ++ prettyJS (H.HsModule loc (H.Module $ "JSDOM.Generated." ++ name) exp imp decl) comment
     ++ "\n"
  Prelude.writeFile (intercalate "/" ("src" : components) ++ ".hs") $
        "module " ++ modName modid ++ " (\n"
     ++ "  module " ++ jsffiModule ++ "\n"
     ++ "  ) where\n"
     ++ "import " ++ jsffiModule ++ "\n"

prettyJS (H.HsModule pos m mbExports imp decls) comment = intercalate "\n" $
       prettyPrint (H.HsModule pos m mbExports imp [])
     : map prettyDecl decls >>= lines >>= gaurdFromJSValUnchecked
  where
    prettyDecl d@(H.HsForeignImport nullLoc "javascript" H.HsUnsafe _ (H.HsIdent defop) tpsig) = prettyPrint d
    prettyDecl d@(H.HsTypeSig _ [H.HsIdent n] _) = concat $ catMaybes [comment n, Just $ prettyPrint d]
    prettyDecl d = prettyPrint d
    interfaceName = stripPrefix "JSDOM." $ modName m
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
                      "Prelude ((.), (==), (>>=), return, IO, Int, Float, Double, Bool(..), Maybe, maybe, fromIntegral, round, realToFrac, fmap, Show, Read, Eq, Ord, Maybe(..))"
                    , "qualified Prelude (error)"
                    , "Data.Typeable (Typeable)"
                    , "Language.Javascript.JSaddle (JSM(..), JSVal(..), JSString, strictEqual, toJSVal, valToStr, valToNumber, valToBool, js, jss, jsf, jsg, function, new, array)"
                    , "Data.Int (Int64)"
                    , "Data.Word (Word, Word64)"
                    , "JSDOM.Types"
                    , "Control.Applicative ((<$>))"
                    , "Control.Monad (void)"
                    , "Control.Lens.Operators ((^.))"
                    ] ++ if name == "Enums"
                            then []
                            else eventImp iid ++ ["JSDOM.Enums"]))
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
        iname <- stripPrefix "JSDOM." iid
        return $ "\n-- | <https://developer.mozilla.org/en-US/docs/Web/API/"
                      ++ jsname iname ++ realName n ++ " Mozilla " ++ jsname iname ++ realName n ++ " documentation>"
      name = typeFor . reverse . takeWhile (/= '.') $ reverse iid
--      subexp = map mkEIdent . nub $ (filter (not . isSuffixOf "'") $ map declname smdecls) ++
      subexp = nub $ map (mkEIdent . fst) smdecls ++
                case name of
                    "Enums" -> []
                    _ | "Callback" `isSuffixOf` name || name == "MediaQueryListListener" -> map (H.HsEVar . H.UnQual . H.HsIdent) (name : parentExp)
                    _ -> map (H.HsEVar . H.UnQual . H.HsIdent) ([name++"(..)", "gType" ++ name] ++ parentExp)
      parentExp | name `elem` allParents = ["Is" ++ name, "to" ++ name]
                | otherwise = []
      eventImp "JSDOM.Event" = []
      eventImp "JSDOM.UIEvent" = []
      eventImp "JSDOM.MouseEvent" = []
      eventImp "JSDOM.EventTarget" = []
      eventImp _ = ["JSDOM.EventTargetClosures (EventName, unsafeEventName)"]
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

-- Validate a map of interface inheritance. Any "Left" parent identifier
-- causes a log message to be produced. It is also checked that an interface
-- does not have itself as a parent. Circular inheritance is not checked.

valParentMap :: M.Map String [Either String String] -> [String]

valParentMap pm = concat (M.elems m2) where
  m2 = M.mapWithKey lefts pm
  lefts intf = concatMap (leftmsg intf)
  leftmsg intf (Right _) = []
  leftmsg "InternalSettings" _ = []
  leftmsg intf (Left p) = ["Interface " ++ intf ++ " has " ++ p ++ " as a parent, but " ++
                           p ++ " is not defined anywhere"]

-- Prepare a complete map of interfaces inheritance. All ancestors
-- must be defined in the IDL module being processed plus in other
-- modules it includes.

mkParentMap :: [I.Defn] -> M.Map String [Either String String]

mkParentMap defns = m2 where
  allintfs = nub $ concatMap getintfs defns
  getintfs (I.Module _ moddefs) = filter intfOnly moddefs
  getintfs _ = []
  m1 = M.fromList $ zip (map getDef allintfs) allintfs
  m2 = M.fromList (map getparents allintfs)
  getparents i@(I.Interface _ supers _ _ _) = (getDef i, nub . concat $ map parent (map jsname' supers ++ eventTarget i))
  parent pidf = if pidf `M.member` m1
                    then Right pidf : snd (getparents (fromJust $ M.lookup pidf m1))
                    else [Left pidf]
  eventTarget (I.Interface (I.Id name) _ _ at _) | name == "EventTarget" = []
                                                 | I.ExtAttr (I.Id "EventTarget") [] `elem` at = ["EventTarget"]
                                                 | otherwise = []


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
--typeFor  "Range" = "DOMRange"
--typeFor  "Screen" = "DOMScreen"
--typeFor  "Attr" = "DOMAttr"
typeFor  "Key" = "CryptoKey"
typeFor  "AlgorithmIdentifier" = "DOMString"
typeFor  "KeyFormat" = "DOMString"
-- typeFor  "XMLHttpRequestResponseType" = "DOMString"
typeFor  "custom" = "CanvasStyle"
typeFor  s = jsname' s
fixType (I.TyName s x) = I.TyName (typeFor s) x
fixType (I.TyOptional a) = I.TyOptional (fixType a)
fixType x = x
fixDefn (I.Attribute a b c d e) = I.Attribute a b (fixType c) d e
fixDefn (I.Operation a b c d e) = I.Operation (fixId a) (fixType b) c d e
fixDefn (I.Interface a b c d e) = I.Interface a b (map fixDefn c) d e
fixDefn x = x
fixId (I.FunId a b c) = I.FunId (fixId a) b (map fixParam c)
fixId x = x
fixParam (I.Param a b c d e) = I.Param a b (fixType c) d e

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
    decls = types ++ classes ++ instances ++ methods ++ attrs ++ makers
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
     H.HsInstDecl nullLoc [] (H.UnQual $ H.HsIdent "Enums||ToJSVal")
        [H.HsTyCon . H.UnQual $ H.HsIdent typename]
        [H.HsFunBind (map toJsVal vals)],
--     H.HsInstDecl nullLoc [] (H.UnQual $ H.HsIdent "Enums||ToJSVal")
--        [H.HsTyCon . H.UnQual $ H.HsIdent typename]
--        [H.HsFunBind
--            [H.HsMatch nullLoc (H.HsIdent "toJSVal") []
--                (H.HsUnGuardedRhs $ H.HsInfixApp (mkVar "return") (H.HsQVarOp $ mkSymbol ".") (mkVar "pToJSVal")) []
--            ]
--        ],
     H.HsInstDecl nullLoc [] (H.UnQual $ H.HsIdent "Enums||FromJSVal")
        [H.HsTyCon . H.UnQual $ H.HsIdent typename]
        [H.HsFunBind [H.HsMatch nullLoc (H.HsIdent "fromJSVal") [H.HsPVar $ H.HsIdent "x"]
            (H.HsUnGuardedRhs (fromJsVal vals)) []]]
--     H.HsInstDecl nullLoc [] (H.UnQual $ H.HsIdent "Enums||FromJSVal")
--        [H.HsTyCon . H.UnQual $ H.HsIdent typename]
--        [H.HsFunBind
--            [H.HsMatch nullLoc (H.HsIdent "fromJSValUnchecked") []
--                (H.HsUnGuardedRhs $ H.HsInfixApp (mkVar "return") (H.HsQVarOp $ mkSymbol ".") (mkVar "pFromJSVal")) [],
--             H.HsMatch nullLoc (H.HsIdent "fromJSVal") []
--                (H.HsUnGuardedRhs $ H.HsInfixApp (mkVar "return") (H.HsQVarOp $ mkSymbol ".") (mkVar "pFromJSVal")) []
--            ]
--        ]
    ]
    ++ map jsffi vals
  where
    constructor (Right n, _, Nothing)  = H.HsConDecl nullLoc (H.HsIdent $ typename ++ conName n) []
    constructor val = error $ "Unhandled enum value " ++ show val
    jsffi (Right n, _, Nothing) = H.HsFunBind [H.HsMatch
        nullLoc
        (H.HsIdent $ "Enums||js_" ++ typename ++ conName n)
        []
        (H.HsUnGuardedRhs (H.HsLit (H.HsString n)))
        []]
--        nullLoc "javascript" H.HsUnsafe
--        ("\"" ++ n ++ "\"")
--        (H.HsIdent $ "Enums||js_" ++ typename ++ conName n)
--        (mkTIdent "JSVal")
    jsffi val = error $ "Unhandled enum value " ++ show val
    toJsVal (Right n, _, Nothing) =
        H.HsMatch nullLoc (H.HsIdent "toJSVal") [H.HsPVar . H.HsIdent $ typename ++ conName n]
            (H.HsUnGuardedRhs . H.HsApp (mkVar "toJSVal") . mkVar $ "js_" ++ typename ++ conName n) []
    toJsVal val = error $ "Unhandled enum value " ++ show val
    fromJsVal [] = H.HsApp (mkVar "return") (mkVar "Nothing")
    fromJsVal ((Right n, _, Nothing):rest) =
        H.HsInfixApp
            (H.HsInfixApp (mkVar "x") (H.HsQVarOp $ mkSymbol "`strictEqual`") (mkVar $ "js_" ++ typename ++ conName n))
            (H.HsQVarOp $ mkSymbol ">>=")
            (H.HsLambda nullLoc [H.HsPVar $ H.HsIdent "r"] $ H.HsCase (mkVar "r") [
                H.HsAlt nullLoc (H.HsPVar $ H.HsIdent "True") (H.HsUnGuardedAlt
                    (H.HsApp (mkVar "return") (H.HsParen (H.HsApp (mkVar "Just") . mkVar $ typename ++ conName n)))) [],
                H.HsAlt nullLoc (H.HsPVar $ H.HsIdent "False") (H.HsUnGuardedAlt (fromJsVal rest)) []
             ])
    fromJsVal (val:_) = error $ "Unhandled enum value " ++ show val
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

-- Convert a name to a type context assertion (assume single parameter class)

name2ctxt name = (mkUIdent $ classFor name, [H.HsTyVar $ head azHIList])

-- A helper function to produce an unqualified identifier

mkUIdent = H.UnQual . H.HsIdent

mkSymbol = H.UnQual . H.HsSymbol

-- A filter to select only operations (methods)

opsOnly :: I.Defn -> Bool
opsOnly I.Operation{} = True
opsOnly _ = False

-- A filter to select only attributes

attrOnly :: I.Defn -> Bool
attrOnly I.Attribute{} = True
attrOnly _ = False

-- A filter to select only interfaces (classes)

intfOnly :: I.Defn -> Bool
intfOnly (I.Interface _ _ cldefs _ Nothing) = True
intfOnly _ = False

intfAndCallbacks :: I.Defn -> Bool
intfAndCallbacks (I.Interface _ _ cldefs _ _) = True
intfAndCallbacks _ = False

-- A filter to select only constant definitions

constOnly :: I.Defn -> Bool
constOnly I.Constant{} = True
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

intf2attr :: [String] -> (String -> Bool) -> I.Defn -> [H.HsDecl]

monadContext = [(mkUIdent "MonadDOM", [mkTIdent "m"])]
monadTv = mkTIdent "m"
liftDOM = H.HsApp (mkVar "liftDOM") . H.HsParen

intf2attr enums isLeaf intf@(I.Interface (I.Id iid') _ cldefs _ _) =
  concatMap mkattr (collectAttrs intf) where
    iid = jsname' iid'
    mkattr (I.Attribute [] _ _ _ _) = []
    mkattr (I.Attribute [I.Id "URL"] _ _ _ _) | getDef intf `elem` ["EventSource", "WebSocket"] = [] -- The standard is the lowercase url
    mkattr (I.Attribute [I.Id iat] _ (I.TyName "EventListener" _) _ _) = mkevent iid iat
    mkattr (I.Attribute [I.Id _] _ (I.TyName t _) _ _) | getDef intf `elem` ["Window", "WorkerGlobalScope"]
                                                         && "Constructor" `isSuffixOf` t = []
    mkattr (I.Attribute [I.Id iat@"responseText"] _ tat raises ext) | getDef intf == "XMLHttpRequest"
        = mkgetter iid iat tat (I.ExtAttr (I.Id "TreatReturnedNullStringAs") [] : ext) raises
    mkattr (I.Attribute [I.Id iat] False tat raises ext) =
      (if I.ExtAttr (I.Id "Replaceable") [] `elem` ext
        then []
        else mksetter iid iat tat ext raises)
      ++ mkgetter iid iat tat ext raises
    mkattr (I.Attribute [I.Id iat] True  tat raises ext) = mkgetter iid iat tat ext raises
    mkattr (I.Attribute (iatt:iats) b tat raises ext) =
      mkattr (I.Attribute [iatt] b tat raises ext) ++ mkattr (I.Attribute iats b tat raises ext)
    mksetter iid iat tat ext r = [stsig iid iat tat ext, simpl iid iat tat ext r]
    setf intf iat = U.toLowerInitCamel $ "set" ++ U.toUpperHead iat
    getf intf iat = U.toLowerInitCamel $ "get" ++ U.toUpperHead iat
    eventName iat = fromMaybe iat (stripPrefix "on" iat)
    eventf intf iat = U.toLowerInitCamel $ fixEventName (getDef intf) iat
    simpl iid iat tat ext raises =
      let defset = iid ++ "|" ++ iat ++ "|" ++ setf intf iat
          -- ffi = H.HsVar . H.UnQual . H.HsIdent $ "js_" ++ setf intf iat
          parms = [H.HsPVar $ H.HsIdent "self", H.HsPVar $ H.HsIdent "val"]
          call = applySelf isLeaf iid (H.HsApp
                    (mkVar "jss")
                    (H.HsLit $ H.HsString iat))
          val = I.Param I.Required (I.Id "val") tat [I.Mode In] ext
          rhs = H.HsUnGuardedRhs $ liftDOM $ H.HsApp call $ H.HsParen (applyParam val)
          match = H.HsMatch nullLoc (H.HsIdent defset) parms rhs [] in
      H.HsFunBind [match]
    stsig iid iat tat ext =
      let defset = iid ++ "|" ++ iat ++ "|" ++ setf intf iat
          parm = I.Param I.Required (I.Id "val") tat [I.Mode In] ext
          parms = [tySelf isLeaf iid, fst $ tyParm enums isLeaf parm]
          contxt = monadContext ++ ctxSelf isLeaf iid ++ snd (tyParm enums isLeaf parm)
          tpsig = mkTsig parms (H.HsTyApp monadTv $ H.HsTyCon (H.Special H.HsUnitCon))
          retts = H.HsQualType contxt tpsig in
      H.HsTypeSig nullLoc [H.HsIdent defset] retts
    mkgetter iid iat tat ext r =
        concatMap (\wrapType -> gtsig wrapType (rawReturn wrapType) ++ gimpl wrapType (rawReturn wrapType)) [Normal, Unsafe, Unchecked]
      where
        rawReturn = tyRet enums tat ext
        gimpl _ Nothing = []
        gimpl wrapType (Just retType) =
          let defget = iid ++ "|" ++ iat ++ "|" ++ wrapName wrapType (getf intf iat)
              -- ffi = H.HsVar . H.UnQual . H.HsIdent $ "js_" ++ getf intf iat
              parm = H.HsPVar $ H.HsIdent "self"
              call = applySelf isLeaf iid (H.HsApp
                        (mkVar "js")
                        (H.HsLit $ H.HsString iat))
              rhs = H.HsUnGuardedRhs $ liftDOM $ returnType enums tat ext wrapType call
              match = H.HsMatch nullLoc (H.HsIdent defget) [parm] rhs [] in
          [H.HsFunBind [match]]
        gtsig _ Nothing = []
        gtsig wrapType (Just retType) =
          let defget = iid ++ "|" ++ iat ++ "|" ++ wrapName wrapType (getf intf iat)
              parms = [tySelf isLeaf iid]
              contxt = monadContext ++ ctxSelf isLeaf iid ++ ctxRet wrapType tat
              tpsig = mkTsig parms (H.HsTyApp monadTv retType)
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

data CallbackType = SyncContinueAsync | SyncThrowWouldBlock | Async deriving (Show, Eq)
data WrapType = Normal | Underscore | Unsafe | Unchecked deriving (Show, Eq)

wrapName _ [] = error "Empty name passed to wrapName"
wrapName Normal x = x
wrapName Underscore x = x ++ "_"
wrapName Unsafe x = x ++ "Unsafe"
wrapName Unchecked x = x ++ "Unchecked"

callbackPostfix SyncContinueAsync   = ""
callbackPostfix SyncThrowWouldBlock = "Sync"
callbackPostfix Async               = "Async"

intf2meth :: [String] -> (String -> Bool) -> I.Defn -> [H.HsDecl]

intf2meth enums isLeaf intf@(I.Interface _ _ (I.Operation (I.FunId _ _ parm) resultType _ _ _:_) _ (Just (I.Id "callback"))) =
    [f callbackType | callbackType <- [SyncContinueAsync, SyncThrowWouldBlock, Async], f <- [tsig, timpl]]
  where
    defop callbackType = getDef intf ++ "||new" ++ getDef intf ++ callbackPostfix callbackType
    tsig callbackType =
      let cparm _ (I.TyApply (I.TySigned False) (I.TyInteger LongLong)) = mkTIdent "Double"
          cparm pname pType = tyRet' pname enums pType []
          cparms = [cparm pname pType | I.Param _ (I.Id pname) pType _ _ <- parm]
          cbfunc = [mkTsig cparms (H.HsTyApp (mkTIdent "JSM") (mkTIdent "()"))] -- Callbacks can't return values for now (fst $ tyParm enums isLeaf resultParam))]
--          parms | sync && withArgs = mkTIdent "OnBlocked":cbfunc
--                | otherwise = cbfunc
          parms = cbfunc
          contxt = monadContext ++ concat [ctxRet' pname pType | I.Param _ (I.Id pname) pType _ _ <- {-resultParam :-} parm]
          resultParam = I.Param I.Required (I.Id "result") resultType [I.Mode In] []
          retType = case getDef intf of
                        "NotificationPermissionCallback" -> H.HsTyApp (mkTIdent (typeFor $ getDef intf)) (mkTIdent "permission")
                        "StringCallback" -> H.HsTyApp (mkTIdent (typeFor $ getDef intf)) (mkTIdent "data'")
                        _ -> mkTIdent (typeFor $ getDef intf)
          tpsig = mkTsig parms (H.HsTyApp monadTv retType)
          retts = H.HsQualType contxt tpsig in
      H.HsTypeSig nullLoc [H.HsIdent (defop callbackType)] retts
    timpl callbackType =
      let cparms = [H.HsPWildCard, H.HsPWildCard, H.HsPList (map (H.HsPVar . H.HsIdent . paramName) parm)]
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
          callbackN = show (length parm)
          lambda = H.HsParen (H.HsLambda nullLoc cparms (L.foldl applyCParam (mkVar "callback") parm))
          call _ = H.HsApp (mkVar "function") lambda
          rhs = H.HsUnGuardedRhs $ liftDOM $
                H.HsInfixApp
                    (H.HsInfixApp
                        (mkVar $ typeFor $ getDef intf)
                        (H.HsQVarOp (mkSymbol "."))
                        (mkVar "Callback")
                    )
                    (H.HsQVarOp (mkSymbol "<$>"))
                    (call callbackType)
          match  = H.HsMatch nullLoc (H.HsIdent (defop callbackType)) parms rhs []
          applyCParam e param@(I.Param optional (I.Id p) ptype [I.Mode In] ext) =
            H.HsInfixApp
              (H.HsApp
                (mkVar (from ptype))
                (mkVar (paramName param))
              )
              (H.HsQVarOp (mkSymbol ">>="))
              (H.HsLambda nullLoc [H.HsPVar (H.HsIdent $ paramName param ++ "'")]
                (H.HsApp e (mkVar $ paramName param ++ "'"))
              )
          from (I.TySafeArray t) =
                case tyRet enums t [] Normal of
                    Just (H.HsTyApp (H.HsTyVar (H.HsIdent "Maybe")) _) -> "fromJSArray"
                    Just _ -> "fromJSArrayUnchecked"
          from (I.TySequence t _) =
                case tyRet enums t [] Normal of
                    Just (H.HsTyApp (H.HsTyVar (H.HsIdent "Maybe")) _) -> "fromJSArray"
                    Just _ -> "fromJSArrayUnchecked"
          from (I.TyOptional _) = "fromJSVal"
          from I.TyObject = "fromJSVal"
          from (I.TyName "DOMString" Nothing) = "fromJSValUnchecked"
          from (I.TyName x Nothing) | x `elem` enums = "fromJSValUnchecked"
                                    | otherwise = "fromJSVal"
          from _ = "fromJSValUnchecked"
      in  H.HsFunBind [match]

intf2meth enums isLeaf intf@(I.Interface _ _ cldefs at _) =
  concatMap mkconstructor constructors ++
  concatMap mkmeth (collectOps intf) ++
  concatMap mkconst (collectConst intf) where
    getDefHs = getDef
    getDefJs op@(I.Operation _ _ _ mbctx _) = case mbctx of
      Nothing -> getDef op
      Just [] -> getDef op
      Just (s:_) -> s
    constructors = zip (map (`replicate` '\'') [0..]) $ reverse [parm | x@(I.ExtAttr (I.Id name) parm) <- at, name `elem` ["Constructor", "CustomConstructor"]]
    mkconstructor c = constructortsig c : constructortimpl c
    constructorRaises | I.ExtAttr (I.Id "ConstructorRaisesException") [] `elem` at = [I.Raises ["RaisesException"]]
                      | otherwise = []
--    constructorjsffi (postfix, parm) =
--      let monadtv = mkTIdent "IO"
--          defop = getDef intf ++ "||" ++ jsffiConstructorName ++ postfix
--          parms = map (fst . tyParmFFI enums isLeaf) parm
--          tpsig = mkTsig parms (H.HsTyApp monadtv (ffiTySelf intf))
--          retts = H.HsQualType [] tpsig
--          jsimpl = case parm of
--                    [I.Param _ _ (I.TyName "DOMString..." _) _ _] ->
--                        error "Unexpected constuctor parameter DOMString..." -- show . renderJs $ [jmacroE| window[`(jsname (getDef intf))`].apply(window, $1) |]
--                    _ ->
--                        show . renderJs $ ApplExpr (callNew (getDef intf))
--                            (map (\(n, _) -> jsv $ '$':show n) $ zip [1..] parm) in
--       H.HsForeignImport nullLoc "javascript" H.HsUnsafe jsimpl (H.HsIdent defop) tpsig
    constructortsig (postfix, parm) =
      let defop = getDef intf ++ "||" ++ constructorName ++ postfix
          parms = map (fst . tyParm enums isLeaf) parm
          contxt = monadContext ++ concatMap (snd . tyParm enums isLeaf) parm
          tpsig = mkTsig parms (H.HsTyApp monadTv (mkTIdent (typeFor $ getDef intf)))
          retts = H.HsQualType contxt tpsig in
      H.HsTypeSig nullLoc [H.HsIdent defop] retts
    constructortimpl (postfix, parm) =
      let defop = getDef intf ++ "||" ++ constructorName ++ postfix
          ffi = H.HsVar . H.UnQual . H.HsIdent $ jsffiConstructorName ++ postfix
          parms = map (H.HsPVar . H.HsIdent . paramName) parm
          call = H.HsApp (mkVar "new") (H.HsParen $ H.HsApp (mkVar "jsg") $ H.HsLit (H.HsString (getDef intf)))
          rhs = H.HsUnGuardedRhs $ liftDOM $ H.HsInfixApp
                    (mkVar (getDef intf))
                    (H.HsQVarOp (mkSymbol "<$>")) $
                    if null parm
                        then H.HsApp call (mkVar "()")
                        else H.HsApp call (H.HsList (map applyParam parm))
                    -- L.foldl (flip $ applyParam enums isLeaf) call parm
          match  = H.HsMatch nullLoc (H.HsIdent defop) parms rhs []
      in  [H.HsFunBind [match]]
    mkconst cn@(I.Constant (I.Id cid) _ _ (I.Lit (IntegerLit (ILit base val)))) =
      let defcn = getDef intf ++ "||pattern " ++ cid
          match = H.HsMatch nullLoc (H.HsIdent defcn) [] crhs []
          crhs = H.HsUnGuardedRhs (H.HsLit (H.HsInt val))
      in  [H.HsFunBind [match]]
--    mkmeth (I.Operation _ _ _ _ ext) | I.ExtAttr (I.Id "V8EnabledAtRuntime") [] `elem` ext = []
    mkmeth (I.Operation (I.FunId (I.Id "handleEvent") _ _) _ _ _ exAttr) | getDef intf `notElem` ["EventListener"] = error $ "Unexpected handleEvent function in " ++ show intf
    mkmeth op | skip op = []
    mkmeth op@(I.Operation (I.FunId _ _ parm) optype raises _ ext) =
        concatMap (\wrapType -> tsig wrapType (rawReturn wrapType) ++ timpl wrapType (rawReturn wrapType)) [Normal, Underscore, Unsafe, Unchecked]
      where
        rawReturn = tyRet enums optype ext
        tsig _ Nothing = []
        tsig wrapType (Just retType) =
          let -- exprtv = mkTIdent "Expression"
              defop = getDef intf ++ "|" ++ rawName op ++ "|" ++ wrapName wrapType (name op parm)
              parms = tySelf isLeaf (getDef intf) : map (fst . tyParm enums isLeaf) parm
              contxt = monadContext ++ ctxSelf isLeaf (getDef intf) ++ concatMap (snd . tyParm enums isLeaf) parm ++ ctxRet wrapType optype
              -- monadctx = (mkUIdent "Monad",[monadtv])
              tpsig = mkTsig parms (H.HsTyApp monadTv retType)
              retts = H.HsQualType contxt tpsig in
          [H.HsTypeSig nullLoc [H.HsIdent defop] retts]
        timpl _ Nothing = []
        timpl wrapType (Just _) =
          let defop = getDef intf ++ "|" ++ rawName op ++ "|" ++ wrapName wrapType (name op parm)
              -- ffi = H.HsVar . H.UnQual . H.HsIdent $ jsffiName op parm
              parms = map (H.HsPVar . H.HsIdent) ("self" : map paramName parm)
              call = applySelf isLeaf (getDef intf) (H.HsApp
                        (mkVar "jsf")
                        (H.HsLit . H.HsString $ getDef op))
              -- params' = map (\I.Param (I.Id "val") tat [I.Mode In] parm
              rhs = H.HsUnGuardedRhs $ liftDOM $ returnType enums optype ext wrapType
                -- $ L.foldl (flip $ applyParam enums isLeaf) call parm
                        $ if null parm
                            then H.HsApp call (mkVar "()")
                            else H.HsApp call (H.HsList (map applyParam parm))
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
    skip (I.Operation (I.FunId _ _ parm) _ _ _ _) = any excludedParam parm
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

-- Build a variable name

mkVar s = H.HsVar $ mkUIdent s

-- Build a method's type signature

mkTsig :: [H.HsType] -> H.HsType -> H.HsType

mkTsig ps a = foldr H.HsTyFun a ps

-- A helper function to produce a type identifier

mkTIdent = H.HsTyVar . H.HsIdent

-- A helper function to produce an export identifier.
-- Datas (Txxx) export all their members.



mkTyList = H.HsTyApp (H.HsTyCon $ H.Special H.HsListCon)

-- Obtain a return type signature from a return type


tyRet :: [String] -> I.Type -> [I.ExtAttribute] -> WrapType -> Maybe H.HsType
tyRet enums t ext Normal = Just (tyRet' "result" enums t ext)
tyRet enums t ext Underscore = case tyRet' "result" enums t ext of
    H.HsTyTuple [] -> Nothing
    _ -> Just (H.HsTyTuple [])
tyRet enums t ext Unsafe = case tyRet' "result" enums t ext of
    (H.HsTyApp (H.HsTyVar (H.HsIdent "Maybe")) (H.HsTyApp (H.HsTyCon (H.Special H.HsListCon)) x)) -> Nothing
    (H.HsTyApp (H.HsTyVar (H.HsIdent "Maybe")) x) -> Just x
    _ -> Nothing
tyRet enums t ext Unchecked = case tyRet' "result" enums t ext of
    (H.HsTyApp (H.HsTyVar (H.HsIdent "Maybe")) (H.HsTyApp (H.HsTyCon (H.Special H.HsListCon)) x)) -> Nothing
    (H.HsTyApp (H.HsTyVar (H.HsIdent "Maybe")) x) -> Just x
    _ -> Nothing

tyRet' :: String -> [String] -> I.Type -> [I.ExtAttribute] -> H.HsType

tyRet' _ enums (I.TyName "object" Nothing) _
    = H.HsTyApp (mkTIdent "Maybe") (mkTIdent "GObject")
tyRet' pname _ (I.TyName "DOMString" Nothing) ext
    | I.ExtAttr (I.Id "TreatReturnedNullStringAs") []  `elem` ext
           || I.ExtAttr (I.Id "TreatNullAs") [] `elem` ext
        = H.HsTyApp (mkTIdent "Maybe") $ mkTIdent (paramName' pname)
    | otherwise = mkTIdent (paramName' pname)
tyRet' pname enums (I.TyOptional t@(I.TyName c Nothing)) ext
    | isNothing (asIs enums (const False) c) = tyRet' pname enums t ext
tyRet' pname enums (I.TyOptional t) ext
    = H.HsTyApp (mkTIdent "Maybe") (tyRet' pname enums t ext)
tyRet' pname enums (I.TySafeArray t) ext = mkTyList (tyRet' pname enums t ext)
tyRet' pname enums (I.TySequence t _) ext = mkTyList (tyRet' pname enums t ext)
tyRet' _ enums (I.TyName c Nothing) _ = case asIs enums (const False) c of
  Nothing -> H.HsTyApp (mkTIdent "Maybe") (mkTIdent $ typeFor c)
  Just c' -> c'
tyRet' _ _ I.TyVoid _ = H.HsTyTuple []
tyRet' _ _ (I.TyInteger LongLong) _ = mkTIdent "Int64"
tyRet' _ _ (I.TyInteger _) _ = mkTIdent "Int"
tyRet' _ _ (I.TyFloat Short) _ = mkTIdent "Float"
tyRet' _ _ (I.TyFloat _) _ = mkTIdent "Double"
tyRet' _ _ (I.TyApply (I.TySigned False) (I.TyInteger LongLong)) _ = mkTIdent "Word64"
tyRet' _ _ (I.TyApply (I.TySigned False) (I.TyInteger _)) _ = mkTIdent "Word"
tyRet' _ _ (I.TyApply _ (I.TyInteger LongLong)) _ = mkTIdent "Int64"
tyRet' _ _ (I.TyApply _ (I.TyInteger _)) _ = mkTIdent "Int"
tyRet' _ _ I.TyObject _ = H.HsTyApp (mkTIdent "Maybe") (mkTIdent "GObject")
tyRet' _ _ I.TyAny _ = mkTIdent "JSVal"
tyRet' _ _ t _ = error $ "Return type " ++ show t

eventTyRet :: (String -> Bool) -> String -> String -> H.HsType
eventTyRet isLeaf interface eventName =
  H.HsTyApp
    (H.HsTyApp
      (mkTIdent "EventName")
      (tySelf isLeaf interface)
    )
    (mkTIdent $ eventType interface eventName)

-- The same, for a concrete type
--
--cnRet :: I.Type -> H.HsType
--
--cnRet (I.TyName c Nothing) = case asIs c of
--  Nothing -> mkTIdent ('T' : c)
--  Just c' -> mkTIdent c'
--cnRet z = tyRet z

-- Obtain a return type context (if any) from a return type

ctxSelf :: (String -> Bool) -> String -> [H.HsAsst]
ctxSelf isLeaf t | isLeaf t  = []
                 | otherwise = [(mkUIdent (classFor t),[mkTIdent "self"])]

tySelf :: (String -> Bool) -> String -> H.HsType
tySelf isLeaf t | isLeaf t  = mkTIdent (typeFor t)
                | otherwise = mkTIdent "self"

ffiTySelf intf = mkTIdent (typeFor $ getDef intf)

ctxRet :: WrapType -> I.Type -> [H.HsAsst]
ctxRet Underscore _ = []
ctxRet Unsafe t = (mkUIdent "HasCallStack", []) : ctxRet Normal t
ctxRet _ t = ctxRet' "result" t

ctxRet' :: String -> I.Type -> [H.HsAsst]

ctxRet' pname (I.TyName "DOMString" Nothing) = [(mkUIdent "FromJSString", [mkTIdent (paramName' pname)])]
--ctxRet' pname (I.TyName "XMLHttpRequestResponseType" Nothing) = [(mkUIdent "FromJSString", [mkTIdent (paramName pname)])]
--ctxRet (I.TyName c Nothing) = case (asIs False c) of
--  Nothing -> [(mkUIdent $ classFor c, [mkTIdent "self"])]
--  Just c' -> []
ctxRet' pname (I.TySequence t _) = ctxRet' pname t
ctxRet' pname (I.TySafeArray t) = ctxRet' pname t
ctxRet' pname (I.TyOptional t) = ctxRet' pname t
ctxRet' pname _ = []

-- Obtain a type signature from a parameter definition

tyParm :: [String] -> (String -> Bool) -> I.Param -> (H.HsType, [H.HsAsst])
tyParm enums isLeaf param@(I.Param optional (I.Id _) ptype [I.Mode In] ext) =
  lookup ptype
 where
  p = mkTIdent (paramName param)
  canBeNull = I.ExtAttr (I.Id "TreatNullAs") []  `elem` ext
  lookup ptype =
   case ptype of
    I.TyOptional t@(I.TyName c Nothing) | typeFor c == "DOMString" -> lookup t
    I.TyOptional t ->
        case lookup t of
            r@(H.HsTyApp (H.HsTyVar (H.HsIdent "Maybe")) _, _) -> r
            (hsType, hsAsst) -> (H.HsTyApp (mkTIdent "Maybe") hsType, hsAsst)
    I.TySequence t _ -> let (hsType, hsAsst) = lookup t in (mkTyList hsType, hsAsst)
    I.TySafeArray t  -> let (hsType, hsAsst) = lookup t in (mkTyList hsType, hsAsst)
    I.TyName c Nothing | c `elem` ["NotificationPermissionCallback", "StringCallback"] ->
                            (H.HsTyApp (mkTIdent "Maybe")
                                        (H.HsTyApp (mkTIdent (typeFor c)) p), [(mkUIdent "ToJSString", [p])])
    I.TyName "DOMString" Nothing | canBeNull -> (H.HsTyApp (mkTIdent "Maybe") p, [(mkUIdent "ToJSString", [p])])
                                 | otherwise -> (p, [(mkUIdent "ToJSString", [p])])
    I.TyName "DOMString..." Nothing  -> (mkTIdent $ "[" ++ paramName param ++ "]", [(mkUIdent "ToJSString", [p]), (mkUIdent "ToJSVal", [p])])
    I.TyName c Nothing -> case asIs enums isLeaf c of
      Just cc       -> (cc, [])
      _             -> (H.HsTyApp (mkTIdent "Maybe") p, [(mkUIdent $ classFor c, [p])])
    I.TyInteger LongLong -> (mkTIdent "Int64",[])
    I.TyInteger _ -> (mkTIdent "Int",[])
    I.TyFloat Short -> (mkTIdent "Float",[])
    I.TyFloat _ -> (mkTIdent "Double",[])
    I.TyApply (I.TySigned False) (I.TyInteger LongLong) -> (mkTIdent "Word64",[])
    I.TyApply (I.TySigned False) (I.TyInteger _) -> (mkTIdent "Word",[])
    I.TyApply _ (I.TyInteger LongLong) -> (mkTIdent "Int64",[])
    I.TyApply _ (I.TyInteger _) -> (mkTIdent "Int",[])
    I.TyAny -> (mkTIdent "JSVal",[])
    t -> error $ "Param type " ++ show t

tyParm' _ _ _ param@I.Param{} = error $ "Unsupported parameter attributes " ++ show param

glTypes = ["GLenum", "GLboolean", "GLbitfield", "GLbyte", "GLshort", "GLint", "GLsizei",
           "GLintptr", "GLsizeiptr", "GLubyte", "GLushort", "GLuint", "GLfloat", "GLclampf",
           "GLint64", "GLuint64"]

-- Some types pass through as is, other are class names

asIs :: [String] -> (String -> Bool)
    -> String -> Maybe H.HsType
asIs enums isLeaf = asIs' enums isLeaf . typeFor
  where
    asIs' enums _ a  | a `elem` enums = Just $ mkTIdent a
    asIs' _ isLeaf a | isLeaf a       = Just $ H.HsTyApp (mkTIdent "Maybe") (mkTIdent a)
    asIs' _ _ "DOMString"               = Just $ mkTIdent "String"
    asIs' _ _ "DOMTimeStamp"            = Just $ mkTIdent "Word"
    asIs' _ _ "CompareHow"              = Just $ mkTIdent "Word"
    asIs' _ _ x | x `elem` glTypes      = Just $ mkTIdent x
    asIs' _ _ "Bool"                    = Just $ mkTIdent "Bool"
    asIs' _ _ "Int"                     = Just $ mkTIdent "Int"
    asIs' _ _ _                         = Nothing

-- Apply a parameter to a FFI call

applySelf isLeaf iid call | isLeaf iid = H.HsInfixApp (H.HsVar . H.UnQual $ H.HsIdent "self")
                                                      (H.HsQVarOp $ mkSymbol "^.")
                                                      call
                          | otherwise = H.HsInfixApp (H.HsParen $ H.HsApp
                                                        (H.HsVar . H.UnQual . H.HsIdent $ "to" ++ typeFor iid)
                                                        (H.HsVar . H.UnQual $ H.HsIdent "self"))
                                                     (H.HsQVarOp $ mkSymbol "^.")
                                                     call

applyParam :: I.Param -> H.HsExp
applyParam param@(I.Param optional (I.Id p) ptype [I.Mode In] ext) =
    let pname = mkVar $ paramName param in
    case ptype of
        I.TyName "DOMString..." Nothing -> H.HsApp (mkVar "toJSVal") (H.HsParen (H.HsApp (mkVar "array") pname))
        I.TySequence _ _ -> H.HsApp (mkVar "toJSVal") (H.HsParen (H.HsApp (mkVar "array") pname))
        I.TySafeArray _ -> H.HsApp (mkVar "toJSVal") (H.HsParen (H.HsApp (mkVar "array") pname))
        I.TyOptional (I.TySequence _ _) -> H.HsApp (mkVar "toJSVal") (H.HsParen (H.HsApp (mkVar "fmap") (H.HsApp (mkVar "array") pname)))
        I.TyOptional (I.TySafeArray _) -> H.HsApp (mkVar "toJSVal") (H.HsParen (H.HsApp (mkVar "fmap") (H.HsApp (mkVar "array") pname)))
        I.TyName "GLintptr" Nothing -> H.HsApp (mkVar "integralToDoubleToJSVal") pname
        I.TyName "GLsizeiptr" Nothing -> H.HsApp (mkVar "integralToDoubleToJSVal") pname
        I.TyName "GLint64" Nothing -> H.HsApp (mkVar "integralToDoubleToJSVal") pname
        I.TyName "GLuint64" Nothing -> H.HsApp (mkVar "integralToDoubleToJSVal") pname
        I.TyInteger LongLong -> H.HsApp (mkVar "integralToDoubleToJSVal") pname
        I.TyApply _ (I.TyInteger LongLong) -> H.HsApp (mkVar "integralToDoubleToJSVal") pname
        _ -> H.HsApp (mkVar "toJSVal") pname

--applyParam :: [String] -> (String -> Bool) -> I.Param -> H.HsExp -> H.HsExp
--
--applyParam enums isLeaf param@(I.Param optional (I.Id p) ptype [I.Mode In] ext) call = lookup ptype where
--  pname = mkVar $ paramName param
--  canBeNull = I.ExtAttr (I.Id "TreatNullAs") []  `elem` ext
--  lookup ptype =
--   case ptype of
--    I.TyOptional t@(I.TyName c Nothing) | isNothing (asIs [] (const False) c) -> lookup t
--    I.TyOptional (I.TyName "DOMString" Nothing) ->
--      H.HsApp
--        call
--        (H.HsParen
--          (H.HsApp
--            (mkVar (if canBeNull then "toMaybeJSString" else "toJSString"))
--            pname
--          )
--        )
--    I.TyOptional (I.TySequence t _) ->
--        H.HsInfixApp
--          (H.HsApp
--            (mkVar "toJSVal")
--            (mkVar (paramName param))
--          )
--          (H.HsQVarOp (mkSymbol ">>="))
--          (H.HsLambda nullLoc [H.HsPVar (H.HsIdent $ paramName param ++ "'")]
--            (H.HsApp call (mkVar $ paramName param ++ "'"))
--          )
--    I.TySequence t _ ->
--        H.HsInfixApp
--          (H.HsApp
--            (mkVar "toJSVal")
--            (mkVar (paramName param))
--          )
--          (H.HsQVarOp (mkSymbol ">>="))
--          (H.HsLambda nullLoc [H.HsPVar (H.HsIdent $ paramName param ++ "'")]
--            (H.HsApp call (mkVar $ paramName param ++ "'"))
--          )
--    I.TySafeArray t ->
--        H.HsInfixApp
--          (H.HsApp
--            (mkVar "toJSVal")
--            (mkVar (paramName param))
--          )
--          (H.HsQVarOp (mkSymbol ">>="))
--          (H.HsLambda nullLoc [H.HsPVar (H.HsIdent $ paramName param ++ "'")]
--            (H.HsApp call (mkVar $ paramName param ++ "'"))
--          )
----    I.TyName "DOMString" Nothing | optional == I.Optional ->
----      H.HsApp
----        call
----        (H.HsParen
----          (H.HsApp
----            (H.HsApp
----              (H.HsApp
----                (mkVar "maybe")
----                (mkVar "jsNull")
----              )
----              (mkVar "toJSString")
----            )
----            pname
----          )
----        )
--    I.TyName "DOMString" Nothing -> H.HsApp call (H.HsParen $ H.HsApp (mkVar (if canBeNull then "toMaybeJSString" else "toJSString")) pname)
--    I.TyName "DOMString..." Nothing ->
--        H.HsInfixApp
--          (H.HsApp
--            (mkVar "toJSVal")
--            (mkVar (paramName param))
--          )
--          (H.HsQVarOp (mkSymbol ">>="))
--          (H.HsLambda nullLoc [H.HsPVar (H.HsIdent $ paramName param ++ "'")]
--            (H.HsApp call (mkVar $ paramName param ++ "'"))
--          )
--    I.TyName "DOMTimeStamp" Nothing -> H.HsApp call pname
--    I.TyName "CompareHow" Nothing -> H.HsApp call pname
--    I.TyName "GLintptr" Nothing -> H.HsApp call pname -- (H.HsParen $ H.HsApp (mkVar "fromIntegral") pname)
--    I.TyName "GLsizeiptr" Nothing -> H.HsApp call pname -- (H.HsParen $ H.HsApp (mkVar "fromIntegral") pname)
--    I.TyName "GLint64" Nothing -> H.HsApp call pname -- (H.HsParen $ H.HsApp (mkVar "fromIntegral") pname)
--    I.TyName "GLuint64" Nothing -> H.HsApp call pname -- (H.HsParen $ H.HsApp (mkVar "fromIntegral") pname)
--    I.TyName x Nothing | x `elem` glTypes -> H.HsApp call pname
--    I.TyName "Bool" Nothing -> H.HsApp call pname
--    I.TyName x Nothing | x `elem` enums ->
--      H.HsApp
--        call
--        (H.HsParen
--          (H.HsApp
--            (mkVar "pToJSVal")
--            pname
--          )
--        )
--    I.TyName x Nothing | isLeaf x ->
--      H.HsApp
--        call
--        (H.HsParen
--          (H.HsApp
--            (mkVar "maybeToNullable")
--            pname
--          )
--        )
--                       | otherwise ->
--      H.HsApp
--        call
--        (H.HsParen
--          (H.HsApp
--            (mkVar "maybeToNullable")
--            (H.HsParen
--              (H.HsApp
--                (H.HsApp
--                  (mkVar "fmap")
--                  (mkVar $ "to" ++ typeFor x)
--                )
--                pname
--              )
--            )
--          )
--        )
--    I.TyInteger LongLong -> H.HsApp call pname -- (H.HsParen $ H.HsApp (mkVar "fromIntegral") pname)
--    I.TyInteger _ -> H.HsApp call pname
--    I.TyFloat Short -> H.HsApp call pname
--    I.TyFloat _   -> H.HsApp call pname
--    I.TyApply _ (I.TyInteger LongLong) -> H.HsApp call pname -- (H.HsParen $ H.HsApp (mkVar "fromIntegral") pname)
--    I.TyApply _ (I.TyInteger _) -> H.HsApp call pname
--    I.TyAny -> H.HsApp call pname
--    t -> error $ "Apply Param type " ++ show t
--
--applyParam _ _ param@I.Param{} _ = error $ "Unsupported parameter attributes " ++ show param

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
returnType _ (I.TyName "DOMString" Nothing) ext wrapType e
    | wrapType /= Unchecked &&
      ( I.ExtAttr (I.Id "TreatReturnedNullStringAs") [] `elem` ext
     || I.ExtAttr (I.Id "TreatNullAs")               [] `elem` ext)
        = H.HsInfixApp
                    (H.HsParen e)
                    (H.HsQVarOp (mkSymbol ">>="))
                    (mkVar "fromMaybeJSString")
    | otherwise = H.HsInfixApp
                    (H.HsParen e)
                    (H.HsQVarOp (mkSymbol ">>="))
                    (mkVar "fromJSValUnchecked")
returnType _ (I.TyName "DOMTimeStamp" Nothing) _ _ e = H.HsApp (H.HsApp (mkVar "round") (mkVar "<$>")) (H.HsParen (
    H.HsInfixApp
          (H.HsParen e)
          (H.HsQVarOp (mkSymbol ">>="))
          (mkVar "valToNumber")))
returnType _ (I.TyName x Nothing) _ _ e | x `elem` glTypes || x == "CompareHow" =
        H.HsInfixApp
          (H.HsParen e)
          (H.HsQVarOp (mkSymbol ">>="))
          (mkVar $ if x == "GLsizeiptr"
                then "integralFromDoubleFromJSValUnchecked"
                else "fromJSValUnchecked")
returnType _(I.TyName "Bool" Nothing) _ _ e =
    H.HsInfixApp
          (H.HsParen e)
          (H.HsQVarOp (mkSymbol ">>="))
          (mkVar "valToBool")
returnType enums (I.TyName x Nothing) _ _ e | x `elem` enums =
        H.HsInfixApp
          (H.HsParen e)
          (H.HsQVarOp (mkSymbol ">>="))
          (mkVar "fromJSValUnchecked")
returnType enums t@(I.TyName x Nothing) _ wrapType e =
        H.HsInfixApp
          (H.HsParen e)
          (H.HsQVarOp (mkSymbol ">>=")) $
          case tyRet enums t [] wrapType of
            Just (H.HsTyApp (H.HsTyVar (H.HsIdent "Maybe")) _) -> mkVar "fromJSVal"
            Just _ -> mkVar "fromJSValUnchecked"
            _ -> error "Issue with wrapType?"
returnType _ (I.TyInteger _) _ _ e = H.HsApp (H.HsApp (mkVar "round") (mkVar "<$>")) (H.HsParen (
    H.HsInfixApp
          (H.HsParen e)
          (H.HsQVarOp (mkSymbol ">>="))
          (mkVar "valToNumber")))
returnType _ (I.TyFloat Short) _ _ e = H.HsApp (H.HsApp (mkVar "realToFrac") (mkVar "<$>")) (H.HsParen (
    H.HsInfixApp
          (H.HsParen e)
          (H.HsQVarOp (mkSymbol ">>="))
          (mkVar "valToNumber")))
returnType _ (I.TyFloat _) _ _ e =
    H.HsInfixApp
          (H.HsParen e)
          (H.HsQVarOp (mkSymbol ">>="))
          (mkVar "valToNumber")
returnType _ (I.TyApply _ (I.TyInteger _)) _ _ e = H.HsApp (H.HsApp (mkVar "round") (mkVar "<$>")) (H.HsParen (
    H.HsInfixApp
          (H.HsParen e)
          (H.HsQVarOp (mkSymbol ">>="))
          (mkVar "valToNumber")))
returnType enums (I.TySafeArray t) _ wrapType e =
    H.HsInfixApp
      (H.HsParen e)
      (H.HsQVarOp (mkSymbol ">>=")) $
    case tyRet enums t [] wrapType of
        Just (H.HsTyApp (H.HsTyVar (H.HsIdent "Maybe")) _) -> mkVar "fromJSArray"
        Just _ -> mkVar "fromJSArrayUnchecked"
        _ -> error "Issue with wrapType?"
returnType enums (I.TySequence t _) _ wrapType e =
    H.HsInfixApp
      (H.HsParen e)
      (H.HsQVarOp (mkSymbol ">>=")) $
    case tyRet enums t [] wrapType of
        Just (H.HsTyApp (H.HsTyVar (H.HsIdent "Maybe")) _) -> mkVar "fromJSArray"
        Just _ -> mkVar "fromJSArrayUnchecked"
        _ -> error "Issue with wrapType?"
returnType enums (I.TyOptional (I.TySafeArray t)) _ wrapType e =
        H.HsInfixApp
          (H.HsParen e)
          (H.HsQVarOp (mkSymbol ">>=")) $
          case tyRet enums t [] wrapType of
            Just (H.HsTyApp (H.HsTyVar (H.HsIdent "Maybe")) _) ->
                H.HsApp (mkVar "maybeNullOrUndefined'") $ mkVar "fromJSArray"
            Just _ -> H.HsApp (mkVar "maybeNullOrUndefined'") $ mkVar "fromJSArrayUnchecked"
            _ -> error "Issue with wrapType?"
returnType enums (I.TyOptional (I.TySequence t _)) _ wrapType e =
        H.HsInfixApp
          (H.HsParen e)
          (H.HsQVarOp (mkSymbol ">>=")) $
          case tyRet enums t [] wrapType of
            Just (H.HsTyApp (H.HsTyVar (H.HsIdent "Maybe")) _) ->
                H.HsApp (mkVar "maybeNullOrUndefined'") $ mkVar "fromJSArray"
            Just _ -> H.HsApp (mkVar "maybeNullOrUndefined'") $ mkVar "fromJSArrayUnchecked"
            _ -> error "Issue with wrapType?"
returnType enums t@(I.TyOptional _) _ wrapType e =
        H.HsInfixApp
          (H.HsParen e)
          (H.HsQVarOp (mkSymbol ">>=")) $
          case tyRet enums t [] wrapType of
            Just (H.HsTyApp (H.HsTyVar (H.HsIdent "Maybe")) (H.HsTyVar (H.HsIdent t)))
                | t `elem` ["Word64", "GLsizeiptr"] -> mkVar "integralFromDoubleFromJSVal"
            Just (H.HsTyVar (H.HsIdent t))
                | t `elem` ["Word64", "GLsizeiptr"] -> mkVar "integralFromDoubleFromJSValUnchecked"
            Just (H.HsTyApp (H.HsTyVar (H.HsIdent "Maybe")) _) -> mkVar "fromJSVal"
            Just _ -> mkVar "fromJSValUnchecked"
            _ -> error "Issue with wrapType?"
returnType enums t@I.TyObject _ wrapType e =
        H.HsInfixApp
          (H.HsParen e)
          (H.HsQVarOp (mkSymbol ">>=")) $
          case tyRet enums t [] wrapType of
            Just (H.HsTyApp (H.HsTyVar (H.HsIdent "Maybe")) _) -> mkVar "fromJSVal"
            Just _ -> mkVar "fromJSValUnchecked"
            _ -> error "Issue with wrapType?"
returnType _ I.TyVoid _ _ e =
        H.HsApp
          (mkVar "void")
          (H.HsParen e)
returnType _ _ _ _ e =
        H.HsInfixApp
          (H.HsParen e)
          (H.HsQVarOp (mkSymbol ">>="))
          (mkVar "toJSVal")

jsReturn :: I.Type -> JExpr -> JExpr
jsReturn (I.TyName "Bool" Nothing) e = [jmacroE| `(e)`?1:0 |]
jsReturn _ e = e

gtkName s =
    let lower = map toLower (U.toUnderscoreCamel s) in
    case stripPrefix "htmli_" lower of
        Just rest -> "html_i"++rest
        Nothing   -> case stripPrefix "x_path" lower of
                        Just rest -> "xpath"++rest
                        Nothing   -> case stripPrefix "web_kit" lower of
                                        Just rest -> "webkit"++rest
                                        Nothing   -> lower






















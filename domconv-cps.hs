-- DOM interface converter: a tool to convert Haskell files produced by
-- H/Direct into properly structured DOM wrapper
-- CPS-style interface generator

module Main where

import Prelude hiding (putStrLn)
import System.Environment.UTF8
import System.Directory
import System.FilePath
import System.Exit
import System.IO (stderr, stdin, openFile, IOMode (..))
import System.IO.UTF8
import Control.Monad
import Data.Maybe
import Data.Either
import Data.List
import Data.Char
import Language.Haskell.Pretty
import Language.Preprocessor.Cpphs
import qualified Language.Haskell.Syntax as H
import qualified Data.Map as M
import qualified Data.Set as S
import qualified OmgParser
import LexM
import qualified IDLSyn as I
import IDLUtils
import Literal
import BasicTypes
import JS.Jcode
import JS.Show
import SplitBounds

main = do
  args <- getArgs
  let epopts = parseOptions args
  case epopts of
    Left s -> do
      hPutStrLn stderr $ "domconv: command line parse error " ++ s
      exitWith (ExitFailure 1)
    Right opts -> procopts opts

procopts opts = do
  let cppfiles = infiles opts
      (hsrc, inclfile) =
        case cppfiles of
          [] -> (hGetContents stdin, "<stdin>")
          ["-"] -> (hGetContents stdin, "<stdin>")
          (a:_) -> (openFile a ReadMode >>= hGetContents, a)
  let baseopt = [("boolean", "Bool")]
      optsb = opts {defines = ((defines opts) ++ baseopt)
                   ,boolopts = ((boolopts opts) {pragma = True}) }
  hsrc' <- hsrc
  hsrcpr <- runCpphs optsb inclfile hsrc'
  x <- runLexM [] inclfile hsrcpr OmgParser.parseIDL
  let showMod (I.Module i d) = do
        putStrLn $ "module" ++ show i
        mapM (putStrLn . show) d
        return ()
      showMod _ = return ()
  putStrLn "{--"
  mapM showMod x
  putStrLn "--}"
  let prntmap = mkParentMap x
--   mapM (putStrLn . show) (M.assocs prntmap)
  let valmsg = valParentMap prntmap
  when (length valmsg > 0) $ do
    mapM_ (hPutStrLn stderr) valmsg
    exitWith (ExitFailure 2)
  let modst = DOMState {
         pm = prntmap
        ,imp = []
        ,ns = ""
        ,modpref = ""
        ,procmod = []
        ,convlog = []
      }
      modst' = domLoop modst x
  mapM_ (hPutStrLn stderr) (convlog modst')
  let splitmod = splitModule (head $ procmod modst')
  mapM_ putSplit splitmod



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
  putStrLn $ "\n" ++ splitBegin ++ "/" ++ (modName modid) ++ "\n"
  putStrLn $ prettyPrint mod
  putStrLn $ "\n" ++ splitEnd ++ "\n"

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
               (mkModImport modid : (imps ++ docimp)) smdecls where
      subexp = map mkEIdent $ nub $ filter (not . isSuffixOf "'") $ map declname smdecls
      docimp = case "createElement" `elem` (map declname smdecls) of
        True -> []
        _ -> [(mkModImport (H.Module docmod))
               {H.importSpecs = Just (False, [H.HsIVar $ H.HsIdent "createElement"])}] where
          docmod = concat $ intersperse "." $ reverse
                   ("Document" : tail (reverse $ parts (== '.') iid))


-- Loop through the list of toplevel parse results (modules, pragmas).
-- Pragmas modify state, modules don't.

domLoop :: DOMState -> [I.Defn] -> DOMState

domLoop st [] = st
domLoop st (def : defs) = case def of
  I.Pragma prgm -> domLoop (prgm2State st (dropWhile isSpace prgm)) defs
  I.Module id moddef ->
    let prmod = mod2mod st (I.Module id' moddef)
        modn = ns st ++ (renameMod $ concat $ intersperse "." $
                           reverse $ parts ( == '.') (getDef def ++ modpref st))
        id' = I.Id modn
        imp' = modn : imp st
        modl = prmod : (procmod st) in
    domLoop st {procmod = modl, imp = imp'} defs
  z ->
    let logmsg = "Expected a Module or a Pragma; found " ++ (show z) in
    domLoop st {convlog = convlog st ++ [logmsg]} defs

-- Modify DOMState based on a pragma encountered

prgm2State :: DOMState -> String -> DOMState

{--
prgm2State st ('p':'r':'e':'f':'i':'x':pref) =
  let prefst = read (dropWhile isSpace pref)
      dot = if length prefst == 0 then "" else "." in
  st {modpref = dot ++ prefst}
--}

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
  ,modpref :: String                         -- module name prefix (#pragma prefix)
  ,procmod :: [H.HsModule]                   -- modules already processed
  ,convlog :: [String]                       -- conversion messages
} deriving (Show)


-- Helpers to produce class and datatype identifiers out of DOM identifiers

classFor s = "C" ++ s
typeFor  s = "T" ++ s

-- Convert an IDL module definition into Haskell module syntax

mod2mod :: DOMState -> I.Defn -> H.HsModule

mod2mod st md@(I.Module _ moddefs) =
  H.HsModule nullLoc (H.Module modid') (Just []) imps decls where
    modid' = renameMod $ getDef md
    imps = map mkModImport (map H.Module (["CPS" ,"UnsafeJS"] ++ imp st))
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

-- For each interface found, locate all constants it contains

intf2const :: I.Defn -> [H.HsDecl]

intf2const intf@(I.Interface _ _ defs) =
  map oneconst (filter constOnly defs)

-- Produce a Haskell nullary function declaration out of a constant

oneconst :: I.Defn -> H.HsDecl

oneconst cnst@(I.Constant (I.Id cid) _ ctyp (I.Lit (IntegerLit clit))) =
  let match = H.HsMatch nullLoc (H.HsIdent cid) []
                        (H.HsUnGuardedRhs (H.HsLit (H.HsString (show clit)))) []
  in H.HsFunBind [match]

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

intf2maker intf@(I.Interface (I.Id iid) _ _) =
  case (tagFor iid) of
    "" -> []
    tag -> [mktsig, mkimpl] where
      mkimpl =
        let defmaker = iid ++ "|mk" ++ renameMod tag
            flipv = H.HsVar (mkUIdent "flip")
            crelv = H.HsVar (mkUIdent "createElement")
            tagv  = H.HsLit (H.HsString tag)
            rhs   = H.HsUnGuardedRhs (H.HsApp flipv (H.HsApp crelv tagv))
            match = H.HsMatch nullLoc (H.HsIdent defmaker) [] rhs [] in
        H.HsFunBind [match]
      mktsig =
        let cpstv = mkTIdent "CPS c"
            defmaker = iid ++ "|mk" ++ renameMod tag
            parms = [H.HsIdent "a"]
            actx = (mkUIdent (classFor "HTMLDocument"),[mkTIdent "a"])
            tpsig = mkTsig parms (H.HsTyApp cpstv (mkTIdent (typeFor iid)))
            retts = H.HsQualType (actx : []) tpsig in
        H.HsTypeSig nullLoc [H.HsIdent defmaker] retts

intf2maker _ = []

-- Tag values corresponding to certain HTML element interfaces

{--

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
tagFor "HTMLSpanElement" = "span"
--}

tagFor "HTMLParagraphElement" = "p"
tagFor "HTMLTableCellElement" = "td"
tagFor "HTMLDListElement" = "dl"
tagFor "HTMLOListElement" = "ol"
tagFor "HTMLUListElement" = "ul"
tagFor "HTMLTableRowElement" = "tr"
tagFor "HTMLTableCaptionElement" = "caption"
tagFor "HTMLImageElement" = "img"
tagFor "HTMLTableSectionElement" = ""
tagFor "HTMLAnchorElement" = "a"

tagFor ('H':'T':'M':'L':s) =
  let suff = "Element"
  in if isSuffixOf suff s
        then map toLower (take (length s - length suff) s)
        else ""

tagFor _ = ""

-- Attributes are represented by methods with proper type signatures.
-- These methods are wrappers around type-neutral unsafe get/set property
-- functions.

intf2attr :: I.Defn -> [H.HsDecl]

intf2attr intf@(I.Interface (I.Id iid) _ cldefs) =
  concat $ map mkattr $ collectAttrs intf where
    mkattr (I.Attribute [] _ _ _ _) = []
    mkattr (I.Attribute [I.Id iat] False tat _ _) = mksetter iid iat tat ++ mkgetter iid iat tat
    mkattr (I.Attribute [I.Id iat] True  tat _ _) = mkgetter iid iat tat
    mkattr (I.Attribute (iatt:iats) b tat raises ext) =
      mkattr (I.Attribute [iatt] b tat raises ext) ++ mkattr (I.Attribute iats b tat raises ext)
    mksetter iid iat tat = [stsig iid iat tat, simpl iid iat]
    simpl iid iat =
      let defset = iid ++ "|set'" ++ iat
          unssetp = H.HsVar (mkUIdent "unsafeSetProperty")
          propnam = H.HsLit (H.HsString iat)
          rhs = H.HsUnGuardedRhs (H.HsApp unssetp propnam)
          match = H.HsMatch nullLoc (H.HsIdent defset) [] rhs [] in
      H.HsFunBind [match]
    stsig iid iat tat =
      let cpstv = mkTIdent "CPS c"
          ityp = I.TyName iid Nothing
          defset = iid ++ "|set'" ++ iat
          parm = [I.Param (I.Id "val") tat [I.Mode In]]
          parms = (map (fst . tyParm) parm) ++ [H.HsIdent "zz"]
          contxt = (concat $ map (snd . tyParm) parm) ++ ctxRet ityp
          tpsig = mkTsig parms (H.HsTyApp cpstv (tyRet ityp))
          retts = H.HsQualType contxt tpsig in
      H.HsTypeSig nullLoc [H.HsIdent defset] retts
    mkgetter iid iat tat = [gtsig iid iat tat, gimpl iid iat]
    gimpl iid iat =
      let defget = iid ++ "|get'" ++ iat
          unsgetp = H.HsVar (mkUIdent "unsafeGetProperty")
          propnam = H.HsLit (H.HsString iat)
          rhs = H.HsUnGuardedRhs (H.HsApp unsgetp propnam)
          match = H.HsMatch nullLoc (H.HsIdent defget) [] rhs [] in
      H.HsFunBind [match]
    gtsig iid iat tat =
      let cpstv = mkTIdent "CPS c"
          defget = iid ++ "|get'" ++ iat
          parms = [H.HsIdent "this"]
          thisctx = (mkUIdent (classFor iid),[mkTIdent "this"])
          tpsig = mkTsig parms (H.HsTyApp cpstv (tyRet tat))
          retts = H.HsQualType (thisctx : ctxRet tat) tpsig in
      H.HsTypeSig nullLoc [H.HsIdent defget] retts


intf2attr _ = []

-- Methods are lifted to top level. Declared argument types are converted
-- into type constraints unless they are of primitive types. First argument
-- always gets a type of the interface where the method is declared.
-- Only `In' parameters are supported at this time.
-- Constants are converted into nullary methods always returning their values

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
    mkmeth op = tsig op : timpl op
    tsig op@(I.Operation (I.FunId _ _ parm) optype _ _ _) =
      let cpstv = mkTIdent "CPS c"
          defop = getDef intf ++ "|" ++ getDefHs op
          parms = (H.HsIdent "this") : (map (fst . tyParm) parm)
          contxt = (concat $ map (snd . tyParm) parm) ++ ctxRet optype
          thisctx = (mkUIdent (classFor $ getDef intf),[mkTIdent "this"])
          tpsig = mkTsig parms (H.HsTyApp cpstv (tyRet optype))
          retts = H.HsQualType (thisctx : contxt) tpsig in
      H.HsTypeSig nullLoc [H.HsIdent defop] retts
    timpl op@(I.Operation (I.FunId _ _ parm) optype _ _ _) =
      let defop = getDef intf ++ "|" ++ getDefHs op
          defop' = defop ++ "'"
          tocpsev = H.HsVar (mkUIdent "toCPE")
          parms = map H.HsPVar (take (1 + length parm) azHIList)
          parmv = map (H.HsVar . H.UnQual)
                      (H.HsIdent (getDefHs op ++ "'") : take (1 + length parm) azHIList)
          mkApp [] = error "Application with empty list"
          mkApp [a] = a
          mkApp (a:as) = H.HsApp a (mkApp as)
          rhs' = H.HsUnGuardedRhs (H.HsApp (H.HsVar (mkUIdent "unsafeJS"))
                                           (H.HsLit (H.HsString mbody)))
          mbody = strJexp (mkMethBody (getDefJs op) (take (1 + length parm) azList)) ++ ";"
          rhs = H.HsUnGuardedRhs (mkApp [tocpsev, H.HsParen $ mkApp parmv])
          match  = H.HsMatch nullLoc (H.HsIdent defop) parms rhs []
          match' = H.HsMatch nullLoc (H.HsIdent defop') parms rhs' [] in
      [H.HsFunBind [match], H.HsFunBind [match']]

intf2meth _ = []

-- Create a Javascript body for a method

mkMethBody :: String -> [String] -> Jexp

mkMethBody _ [] = error $ "A method should have at least one argument"

mkMethBody meth args =
  let (this:rgs) = map (\p -> JCall (JStr "exprEval") [JStr p]) args
      body = JMethod meth this rgs in
  JCall (JStr "return") [body]

-- Build a method's type signature

mkTsig :: [H.HsName] -> H.HsType -> H.HsType

mkTsig [] a = a
mkTsig (p:ps) a = H.HsTyFun (H.HsTyVar p) (mkTsig ps a)

-- A helper function to produce a type identifier

mkTIdent = H.HsTyVar . H.HsIdent

-- A helper function to produce an export identifier

mkEIdent = H.HsEVar . H.UnQual . H.HsIdent

-- Obtain a return type signature from a return type

tyRet :: I.Type -> H.HsType

tyRet (I.TyName c Nothing) = case (asIs c) of
  Nothing -> mkTIdent "zz"
  Just c' -> mkTIdent c'
tyRet (I.TyInteger _) = mkTIdent "Int"
tyRet (I.TyApply _ (I.TyInteger _)) = mkTIdent "Int"
tyRet (I.TyFloat _) = mkTIdent "Float"
tyRet (I.TyApply _ (I.TyFloat _)) = mkTIdent "Float"
tyRet  I.TyVoid  = H.HsTyTuple []
tyRet t = error $ "Return type " ++ (show t)

-- Obtain a return type context (if any) from a return type

ctxRet :: I.Type -> [H.HsAsst]

ctxRet (I.TyName c Nothing) = case (asIs c) of
  Nothing -> [(mkUIdent $ classFor c, [mkTIdent "zz"])]
  Just c' -> []

ctxRet _ = []

-- Obtain a type signature from a parameter definition

tyParm :: I.Param -> (H.HsName, [H.HsAsst])

tyParm (I.Param (I.Id p) ptype [I.Mode In]) =
  let hsidp = H.HsIdent p in
  case ptype of
    I.TyName c Nothing -> case asIs c of
      Just cc ->  (H.HsIdent cc, [])
      Nothing -> (hsidp, [(mkUIdent $ classFor c, [mkTIdent p])])
    I.TyInteger _ -> (H.HsIdent "Int", [])
    I.TyFloat _ -> (H.HsIdent "Float", [])
    I.TyApply _ (I.TyInteger _) -> (H.HsIdent "Int", [])
    I.TyApply _ (I.TyFloat _) -> (H.HsIdent "Float", [])
    t -> error $ "Param type " ++ (show t)

tyParm (I.Param _ _ _) = error "Unsupported parameter attributes"

-- Some types pass through as is, other are class names

asIs :: String -> Maybe String

asIs "DOMString" = Just "String"
asIs "Bool"      = Just "Bool"
asIs _ = Nothing



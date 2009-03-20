%
% @(#) $Docid: Feb. 9th 2003  16:35  Sigbjorn Finne $
% @(#) $Contactid: sof@galois.com $
%

\begin{code}
module BasicTypes 

	(
	  Name

	, QualName
	, qName
	, qOrigName
	, qModule
	, qDefModule
	, mkQualName
	, toQualName
	, prefixQName
	, prefixAppQName
	, setOrigQName

	, ScopedName
	, Inherit
	, GUID
	, Size(..)
	, CallConv(..)
	, BinaryOp(..)
	, UnaryOp(..)
	, ShiftDir(..)
	, Qualifier(..)
	, PointerType(..)

	, ParamDir(..)
	, isInOut

	, ppBinaryOp
	, ppUnaryOp
	, ppQualifier
	, ppSize
	, ppCallConv
	, ppName
	, ppQualName
	, ppDirection
	
	, strToCallConv
	
	, EnumKind(..)
	, classifyProgression

	) where

import PP
import Maybe ( fromMaybe )
import Opts  ( optNoQualNames, optIntIsInt )
import Utils ( mapMb )
import Int
{- BEGIN_GHC_ONLY
import GlaExts
   END_GHC_ONLY -}
\end{code}

\begin{code}
-- a generic.. name.
type Name = String

-- a qualified.. Name - used throughout the backend to keep
-- track of home & name of types & values.
data QualName = QName {
		  qName       :: Name,
		  qOrigName   :: Name,
		  qModule     :: Maybe Name,
		  qDefModule  :: Maybe Name  -- where the name was originally defined.
		}
	        deriving Eq

mkQualName :: Maybe String -> String -> QualName
mkQualName md nm = QName nm nm md Nothing

toQualName :: String -> QualName
toQualName str = 
  case (break (=='.') (reverse str)) of
    (_,[])    -> mkQualName Nothing str
    (mn,_:dm) -> mkQualName (Just (reverse dm)) (reverse mn)

setOrigQName :: Name -> QualName -> QualName
setOrigQName nm qn = qn{qOrigName=nm}

prefixQName :: String -> QualName -> QualName
prefixQName v qn = qn{qName=v++qName qn , qDefModule=Nothing}

prefixAppQName :: String -> QualName -> QualName
prefixAppQName v qn = qn{qName=("(" ++v++" "++(qName qn)++")") , qDefModule=Nothing}

-- scoped names is OMG CORBA, as it allows
-- multi-level qualifiers on names, e.g., a::b::c
type ScopedName = [String]

-- an OMG interface can inherit from one or more interfaces.
-- DCE/MS IDL: just the one (with COM, you get the effect of
-- multiple inheritance from IUnknown.)
type Inherit = [Name]

-- A five element list
type GUID = [String]

data Size 
 = Short | Natural | Long | LongLong
   deriving (
              Show -- for Lex debugging only
	    , Eq
	    ) 

data CallConv = Stdcall | Pascal | Cdecl | Fastcall
	        deriving ( Eq, Show )

strToCallConv :: String -> Maybe CallConv
strToCallConv "stdcall" = Just Stdcall
strToCallConv "cdecl"   = Just Cdecl
strToCallConv _		= Nothing
\end{code}

Arithmetic and logical operators allowed in IDL:

\begin{code}
data BinaryOp 
 = Xor | Or  | And | Shift ShiftDir 
 | Add | Sub | Div | Mod | Mul   
 | LogAnd | LogOr
 | Gt | Ge | Eq | Le | Lt | Ne
 deriving ( Eq, Show ) 

data UnaryOp  
 = Minus | Plus | Not | Negate | Deref
   deriving ( Eq, Show )

data ShiftDir 
 = L | R
   deriving ( Eq, Show )

data Qualifier 
 = Const | Volatile
   deriving (
              Show
	    , Eq
	    )

data PointerType 
  = Ptr 
  | Ref 
  | Unique
  deriving ( Eq, Show )

data ParamDir   = In | Out | InOut
                  deriving (Eq,Show) -- for Lex debugging only

isInOut :: ParamDir -> Bool
isInOut InOut = True
isInOut _     = False

\end{code}

\begin{code}
ppBinaryOp :: BinaryOp -> PPDoc a
ppBinaryOp op =
   case op of
     Xor     -> char '^'
     Or      -> char '|'
     And     -> char '&'
     Shift d -> text (case d of { L -> "<<" ; R -> ">>" })
     Add     -> char '+'
     Sub     -> char '-'
     Div     -> char '/'
     Mod     -> char '%'
     Mul     -> char '*'
     LogAnd  -> text "&&"
     LogOr   -> text "||"
     Gt      -> char '>'
     Ge      -> text ">="
     Eq      -> text "=="
     Le      -> text "<="
     Lt      -> char '<'
     Ne      -> text "/="

ppUnaryOp :: UnaryOp -> PPDoc a
ppUnaryOp op =
 case op of
  Minus  -> char '-'
  Plus   -> char '+'
  Not    -> char '~'
  Negate -> char '!'
  Deref  -> char '*'

ppQualifier :: Qualifier -> PPDoc a
ppQualifier Const    = text "const"
ppQualifier Volatile = text "volatile"
\end{code}

\begin{code}
ppSize :: Size -> PPDoc a
ppSize Short     = text "short"
ppSize Long      = text "long"
ppSize Natural 
  | optIntIsInt  = text "int"
  | otherwise    = text "long"
ppSize LongLong  = text "long long"

ppCallConv :: Bool -> CallConv -> PPDoc a
ppCallConv for_c c =
 text $
 case c of 
  Stdcall -> if for_c then "__stdcall" else "stdcall"
   -- it is hard to find definite information on this, but I believe
   -- that the Pascal calling convention is after all identical to
   -- Stdcall. (The MSDN docs seems to be utterly confused as to whether
   -- arguments are pushed L-to-R or R-to-L.)
  Pascal  -> if for_c then "__stdcall" else "stdcall"
--  Pascal  -> "pascal"
   -- _cdecl is not provided with gcc, just omit.
  Cdecl -> if for_c then "" else "ccall"
--  Cdecl -> if for_c then "_cdecl" else "ccall"
  Fastcall -> "fastcall"

ppName :: Name -> PPDoc a
ppName nm = text nm

ppQualName :: QualName -> PPDoc a
ppQualName (QName nm _ md def_mod)  
 | optNoQualNames = text nm
 | otherwise    =
    case def_mod of
      Nothing -> (fromMaybe empty (mapMb (\ m -> text m <> char '.') md)) <> text nm
      Just m  -> text m <> char '.' <> text nm

instance Show QualName where
  showsPrec _ q = showString (showPPDoc (ppQualName q) ())

\end{code}

\begin{code}
ppDirection :: ParamDir -> PPDoc a
ppDirection d =
 text $
 case d of
   In    -> "in"
   Out   -> "out"
   InOut -> "in, out"
\end{code}

A sequence of enumeration tags is classified according to
what common class of progression it represent an instance of.
Knowing this may help us generate less Haskell code in the end.
(by using the "deriving" mechanism or, in the case of ghc, use
its support for going straight between tag values and enum dtors.)

\begin{code}
data EnumKind
 = EnumProgression
       Int     -- start offset
       Int     -- step. Note: *may* be < 0.
 | EnumFlags      -- 0, 1, 2, 4, 8, ..
       Int     -- start value
 | Unclassified
   deriving ( Show -- for debugging
            , Eq
	    )

 -- assume that the tag sequence is appropriately sorted.
 -- ('weird' int type of the tags is due to the fact that's
 --  the one we're using in Core.)
classifyProgression :: [Int32] -> EnumKind
classifyProgression []  = Unclassified
classifyProgression [x] = EnumProgression (fromIntegral x) 0
classifyProgression ls@(x:y:_)
  | x == y    			          = Unclassified
  | and (zipWith (==) ls (pow2Series x))  = EnumFlags (fromIntegral x)
  | and (zipWith (==) ls [x,(x+(y-x))..]) = EnumProgression (fromIntegral x) (fromIntegral (y-x))
  | otherwise                             = Unclassified
  where
   pow2Series n = n : pow2Series (doub n)

   doub n | n == 0    = 1
   	  | otherwise = 2*n

\end{code}

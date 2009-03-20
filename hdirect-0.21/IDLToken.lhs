%
% @(#) $Docid: Sep. 19th 2001  12:01  Sigbjorn Finne $
% @(#) $Contactid: sof@galconn.com $
%


\begin{code}
module IDLToken where

import IDLSyn
import BasicTypes
import Literal
import SrcLoc
import Opts ( optCompilingDceIDL
	    , optCompilingMsIDL
	    , optCompilingOmgIDL
	    , optJNI
	    )
\end{code}

Tedious type giving all the lexemes that are
passed between lexer and parser. Highlights:

  * "uuid(guid)" is one lexeme - the interaction
    between lexing guids and numbers made this
    convenient. [No loss, I'm not aware of guids
    being used outside the uuid() attribute in IDL.]
  * As is the case when parsing C with Yacc, we choose
    to get around the problem of parsing typedefs by
    installing parsed typedef names into the symbol
    table being threaded by the lexer monad.  

    Hence, subsequent uses of the typedef'ed name will
    be reported as T_tdef_name lexemes rather than as
    ordinary ids (T_id.)

    [ToDo: check if there's not alternative ways of doing
     this.]

\begin{code}
data IDLToken
 = T_semi
 | T_module
 | T_interface
 | T_oparen
 | T_cparen
 | T_ocurly
 | T_ccurly
 | T_colon
 | T_dcolon
 | T_const
 | T_volatile
 | T_equal
 | T_eqeq
 | T_neq
 | T_negate
 | T_rel_or
 | T_or
 | T_xor
 | T_rel_and
 | T_and
 | T_shift ShiftDir
 | T_div
 | T_mod
 | T_not
 | T_typedef
 | T_extern
 | T_comma
 | T_dot
 | T_dotdotdot
 | T_float    Size
 | T_int      Size
 | T_uint     Size
 | T_unsigned
 | T_signed
 | T_sizeof
 | T_char
 | T_wchar
 | T_boolean
 | T_octet
 | T_any
 | T_object
 | T_struct
 | T_union
 | T_switch
 | T_case
 | T_default
 | T_enum
 | T_question
 | T_lt
 | T_le
 | T_gt
 | T_ge
 | T_osquare
 | T_csquare
 | T_exception
 | T_void
 | T_mode ParamDir
 | T_raises
 | T_context
 | T_fixed
 | T_idl_type	Type
 | T_type       String
 | T_literal    Literal
 | T_string_lit String
 | T_wstring_lit String
 | T_callconv   CallConv
 | T_pragma     String -- everything after '#pragma'
 | T_id String
 | T_dispinterface
 | T_coclass
 | T_library
 | T_plus
 | T_times
 | T_minus
 | T_safearray
 | T_sequence
 | T_string
 | T_wstring
 | T_readonly
 | T_attribute
 | T_methods
 | T_properties
 | T_import
 | T_include_start String
 | T_include_end
 | T_gnu_attribute
 | T_importlib
 | T_oneway
 | T_cpp_quote
 | T_hs_quote
 | T_include String
 | T_hdefine
 | T_unknown (SrcLoc,String)
 | T_ignore_start
 | T_eof
--   deriving Show
\end{code}

Keywords for the different flavours of IDL we support:

\begin{code}
idlKeywords :: [(String, IDLToken)]
idlKeywords 
  | optCompilingDceIDL = dce_keywords
  | optCompilingMsIDL  = midl_keywords
  | optCompilingOmgIDL = omg_keywords
  | otherwise	       = omg_keywords

-- keywords shared by all the IDL dialects we support.
std_idl_keywords :: [(String, IDLToken)]
std_idl_keywords =
    (if optJNI then
        (("Object",   T_idl_type TyObject):)
     else
        id)
    [ ("boolean",	T_type "bool")
    , ("case",          T_case)
    , ("char",          T_char)
    , ("const",  	T_const)
    , ("default",  	T_default)
    , ("double",  	T_float Long)
    , ("enum",		T_enum)
    , ("float",  	T_float Short)
    , ("in",  	        T_mode In)
    , ("interface",	T_interface)
    , ("import",	T_import)
    , ("long",          T_int Long)
    , ("int",           T_int Natural)
    , ("module",        T_module)
    , ("out",           T_mode Out)
    , ("short",         T_int Short)
    , ("signed",        T_signed)
    , ("sizeof",        T_sizeof)
    , ("struct",        T_struct)
    , ("switch",        T_switch)
    , ("typedef",	T_typedef)
    , ("unsigned",      T_unsigned)
    , ("union",		T_union)
    , ("void",		T_void)
    , ("wchar",		T_wchar)
    , ("wstring",	T_wstring)
    , ("FALSE",         T_literal (BooleanLit False))
    , ("TRUE",          T_literal (BooleanLit True))
    , ("NULL",		T_literal NullLit)
    , ("byte",		T_type "octet")
    , ("stablePtr",	T_idl_type TyStable)
    , ("extern",	T_extern)
          -- FIXME: normalise callconv naming with cpp
    , ("__stdcall",	T_callconv Stdcall)  
    , ("__stdcall__",	T_callconv Stdcall)  
    , ("__cdecl",	T_callconv Cdecl)
    , ("__cdecl__",	T_callconv Cdecl)
    , ("_stdcall",	T_callconv Stdcall)
    , ("_cdecl",	T_callconv Cdecl)
    , ("stdcall",	T_callconv Stdcall)
    , ("cdecl",	        T_callconv Cdecl)
    , ("__attribute__", T_gnu_attribute)
    ] 

dce_keywords :: [(String, IDLToken)]
dce_keywords = std_idl_keywords ++ dce_idl_keywords

dce_idl_keywords :: [(String, IDLToken)]
dce_idl_keywords =
 [ ("byte",		T_type "octet")
 , ("error_status_t",   T_type "error_status_t")
 , ("small",		T_int Short)
 ] 


midl_keywords :: [(String, IDLToken)]
midl_keywords = std_idl_keywords ++ dce_idl_keywords ++ ms_idl_keywords

ms_idl_keywords :: [(String, IDLToken)]
ms_idl_keywords = 
      [ ("IDispatch",	   T_type "IDispatch")
      , ("IUnknown",	   T_type "IUnknown")
      , ("HRESULT",	   T_type "HRESULT")
      , ("DATE",	   T_type "DATE")
      , ("CURRENCY",	   T_type "CURRENCY")
      , ("VARIANT",	   T_type "VARIANT")
      , ("VARIANT_BOOL",   T_type "VARIANT_BOOL")
      , ("BSTR",	   T_type "BSTR")
      , ("SAFEARRAY",      T_safearray)
      , ("hyper",	   T_int LongLong)
      , ("__int8",	   T_octet)
      , ("__int16",	   T_int Short)
      , ("__int32",	   T_int Long)
      , ("__int64",	   T_int LongLong)
--      , ("int64",	   T_int LongLong)
--      , ("uint64",	   T_uint LongLong)  -- this one, ugh!
					    
      , ("coclass",	   T_coclass)
      , ("cpp_quote",  	   T_cpp_quote)
      , ("dispinterface",  T_dispinterface)
      , ("single",  	   T_float Short)  -- VBism?
      , ("importlib",	   T_importlib)
      , ("library",	   T_library)
      , ("methods",        T_methods)
      , ("properties",     T_properties)
      , ("bool",	   T_type "bool")
      , ("volatile",  	   T_volatile)
      , ("wchar_t",  	   T_type "wchar_t")
         -- The next two are local extensions.
      , ("hs_quote",  	   T_hs_quote)
      , ("stub_include",   T_include "")
      , ("__ignore_start__", T_ignore_start)
      ]      

omg_keywords :: [(String, IDLToken)]
omg_keywords = std_idl_keywords ++ omg_idl_keywords

omg_idl_keywords :: [(String, IDLToken)]
omg_idl_keywords = 
  [ ("any",	   T_any)
  , ("attribute",  T_attribute)
  , ("context",    T_context)
  , ("exception",  T_exception)
  , ("fixed",  	   T_fixed)
  , ("inout",      T_mode InOut)
  , ("Object",     T_object)
  , ("octet",      T_octet)
  , ("oneway",     T_oneway)
  , ("raises",     T_raises)
  , ("readonly",   T_readonly)
  , ("sequence",   T_sequence)
  , ("string",     T_string)
  ]

\end{code}

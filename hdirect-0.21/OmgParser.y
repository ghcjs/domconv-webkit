{
{-
%
% (c) The GRASP/AQUA Project, Glasgow University, 1998
%
% @(#) $Docid: Jul. 12th 2001  10:08  Sigbjorn Finne $
% @(#) $Contactid: sof@galconn.com $
%

A grammar for OMG CORBA IDL

-}
module OmgParser ( parseIDL ) where

import LexM
import Lex
import IDLToken
import IDLSyn
import BasicTypes
import Literal
{-
BEGIN_GHC_ONLY
import GlaExts
END_GHC_ONLY
-}
}

%name parseIDL
%tokentype { IDLToken }
%monad { LexM } {thenLexM } { returnLexM }
%lexer { lexIDL } { T_eof }
%token
	';'	      { T_semi }
        MODULE	      { T_module }
        INTERFACE     { T_interface }
        '('           { T_oparen }
        ')'           { T_cparen }
        '{'           { T_ocurly }
        '}'           { T_ccurly }
        ':'           { T_colon  }
        '::'          { T_dcolon  }
        ','           { T_comma }
        '.'           { T_dot }
        CONST         { T_const }
        '='           { T_equal }
        '=='          { T_eqeq }
        '!='          { T_neq }
        '|'           { T_or }
        '||'          { T_rel_or }
        '^'           { T_xor }
        '&'           { T_and }
        '&&'          { T_rel_and }
        SHIFT         { T_shift $$ }
        '/'           { T_div }
        '%'           { T_mod }
        '~'           { T_not }
        '!'           { T_negate }
        '?'           { T_question }
        TYPEDEF       { T_typedef }
        TYPE          { T_type $$ }
        FLOAT         { T_float $$ }
        INTEGER       { T_int $$ }
        UNSIGNED      { T_unsigned }
        SIGNED        { T_signed }
        CHAR          { T_char }
        WCHAR         { T_wchar }
        BOOLEAN       { T_boolean }
        STRUCT        { T_struct }
        UNION         { T_union }
        SWITCH        { T_switch }
        CASE          { T_case }
        DEFAULT       { T_default }
        ENUM          { T_enum }
        '<'           { T_lt }
        '<='          { T_le }
        '>'           { T_gt }
        '>='          { T_ge }
        '['           { T_osquare }
        ']'           { T_csquare }
        VOID          { T_void }
        MODE          { T_mode $$ }
        LITERAL       { T_literal $$ }
        STRING_LIT    { T_string_lit $$ }
	ID	      { T_id $$ }
	ATTRIBUTE     { T_attribute }
	'+'	      { T_plus }
	'*'	      { T_times }
	'-'	      { T_minus }
	STRING        { T_string }
	WSTRING       { T_wstring }
	SEQUENCE      { T_sequence  }
	OBJECT        { T_object }
	ANY           { T_any }
        OCTET         { T_octet }
	ONEWAY	      { T_oneway }
	FIXED	      { T_fixed }
	EXCEPTION     { T_exception }
	RAISES        { T_raises }
	CONTEXT       { T_context }
	READONLY      { T_readonly }
        INCLUDE_START { T_include_start $$ }
	INCLUDE_END   { T_include_end }
	PRAGMA        { T_pragma $$ }
        UNKNOWN       { T_unknown $$ }
%%

specification :: { [Defn] }
   : definitions	     { (reverse $1) }

definitions  :: { [Defn] }
   :                         {    []   }
   | definitions definition  { $2 : $1 }

definition   :: { Defn }
   : type_decl   ';'	    { $1 }
   | const_decl  ';'        { $1 }
   | except_decl ';'        { $1 }
   | interface   ';'        { $1 }
   | module      ';'        { $1 }
   | PRAGMA                 { Pragma $1 }
   | INCLUDE_START          { IncludeStart $1 }
   | INCLUDE_END            { IncludeEnd }

module :: { Defn }
   : MODULE identifier '{' definitions '}'  { Module $2 (reverse $4) }

interface :: { Defn }
   : interface_decl	    { $1 }
   | INTERFACE identifier   { Forward $2 }

interface_decl :: { Defn }
   : interface_header '{' exports '}'  { let (ids,inherit) = $1 in Interface ids inherit (reverse $3) }

interface_header :: { (Id, Inherit) }
   : INTERFACE identifier opt_inheritance_spec { ($2,$3) }

exports :: { [Defn] }
   : {-empty-}			{    []   }
   | exports export		{ $2 : $1 }

export  :: { Defn }
   : type_decl ';'	      { $1 }
   | const_decl ';'	      { $1 }
   | except_decl ';'          { $1 }
   | attr_decl ';'	      { $1 }
   | op_decl ';'	      { $1 }

opt_inheritance_spec :: { Inherit }
   : {-empty-}	              { [] }
   | inheritance_spec	      { $1 }

inheritance_spec :: { Inherit }
   : ':' scoped_name more_scoped_names  { $2:(reverse $3) }

scoped_name :: { String }
   : ID					{ $1  }
   | '::' ID				{ ("::"++ $2)  }
   | scoped_name '::' ID		{ $1 ++ ':':':':$3 }

more_scoped_names :: { Inherit }
   : {-empty-}			       { [] }
   | ',' scoped_name more_scoped_names { $2 : $3 }

{- constant declarations -}

const_decl :: { Defn }
   : CONST const_type identifier '=' const_expr  { Constant $3 [] $2 $5 }

const_type :: { Type }
   : integer_type		{ $1 }
   | char_type			{ $1 }
   | wide_char_type		{ $1 }
   | boolean_type		{ $1 }
   | floating_pt_type		{ $1 }
   | string_type		{ $1 }
   | wide_string_type		{ $1 }
   | fixed_pt_const_type	{ $1 }
   | scoped_name		{ TyName $1 Nothing }

const_expr :: { Expr }
   : or_expr			{ $1 }

or_expr :: { Expr }
   : xor_expr			{ $1 }
   | or_expr '|' xor_expr	{ Binary Or $1 $3 }

xor_expr :: { Expr }
   : and_expr			{ $1 }
   | xor_expr '^' and_expr	{ Binary Xor $1 $3 }

and_expr :: { Expr }
   : shift_expr			{ $1 }
   | and_expr '&' shift_expr    { Binary And $1 $3 }
   
shift_expr :: { Expr }
   : add_expr			{ $1 }
   | shift_expr SHIFT add_expr  { Binary (Shift $2) $1 $3 }

add_expr :: { Expr }
   : mult_expr			{ $1 }
   | add_expr '+' mult_expr     { Binary Add $1 $3 }
   | add_expr '-' mult_expr     { Binary Sub $1 $3 }

mult_expr :: { Expr }
   : unary_expr			{ $1 }
   | mult_expr '*' unary_expr   { Binary Mul $1 $3 }
   | mult_expr '/' unary_expr   { Binary Div $1 $3 }
   | mult_expr '%' unary_expr   { Binary Mod $1 $3 }

unary_expr :: { Expr }
   : unary_operator primary_expr { Unary $1 $2 }
   | primary_expr		 { $1 }

unary_operator :: { UnaryOp }
   : '-'	{ Minus }
   | '+'	{ Plus  }
   | '~'	{ Not   }

primary_expr :: { Expr }
   : scoped_name		{ Var $1 }
   | literal			{ Lit $1 }

literal :: { Literal }
   : LITERAL			{ $1 }

positive_int_const :: { Expr }
   : const_expr				{ $1 }

{---- type declarations ------}

type_decl   :: { Defn }
   : TYPEDEF type_declarator		{ let (spec, decls) = $2 in Typedef spec [] decls }
   | struct_type			{ TypeDecl $1 }
   | union_type 			{ TypeDecl $1 }
   | enum_type  			{ TypeDecl $1 }

type_declarator :: { (Type, [Id]) }
   : type_spec declarators		{ ($1,$2) }

type_spec :: { Type }
   : simple_type_spec			{ $1 }
   | constr_type_spec			{ $1 }
   

simple_type_spec :: { Type }
   : base_type_spec			{ $1 }
   | template_type_spec			{ $1 }
   | scoped_name			{ TyName $1 Nothing }

base_type_spec :: { Type }
   : floating_pt_type			{ $1 }
   | integer_type			{ $1 }
   | char_type				{ $1 }
   | wide_char_type			{ $1 }
   | boolean_type			{ $1 }
   | octet_type				{ $1 }
   | any_type				{ $1 }
   | object_type			{ $1 }

template_type_spec :: { Type }
   : sequence_type			{ $1 }
   | string_type			{ $1 }
   | wide_string_type			{ $1 }
   | fixed_pt_type			{ $1 }

constr_type_spec :: { Type }
   : struct_type			{ $1 }
   | union_type				{ $1 }
   | enum_type				{ $1 }

declarators :: { [Id] }
   : declarator				{ [$1] }
   | declarators ',' declarator		{ $3 : $1 }

simple_declarator ::  { Id }
   : identifier				{ $1 }

declarator :: { Id }
   : identifier				{ $1 }
   | identifier fixed_array_sizes	{ ArrayId $1 $2 }

floating_pt_type :: { Type }
   : FLOAT			{ TyFloat $1 }

integer_type :: { Type }
   : INTEGER			{ TyInteger $1 }
   | SIGNED INTEGER		{ TyApply (TySigned True)  (TyInteger $2) }
   | UNSIGNED INTEGER		{ TyApply (TySigned False) (TyInteger $2) }

char_type :: { Type }
   : CHAR	{ TyChar }

wide_char_type :: { Type }
   : WCHAR	{ TyWChar }


boolean_type :: { Type }
   : BOOLEAN	{ TyBool }

octet_type   :: { Type }
   : OCTET      { TyOctet }

any_type     :: { Type }
   : ANY	{ TyAny }

object_type  :: { Type }
   : OBJECT     { TyObject }

struct_type :: { Type }
   : STRUCT identifier '{' member_list '}' { TyStruct (Just $2) $4 Nothing }

member_list :: { [Member] }
   : member		{ [$1] }
   | member_list member { $2:$1 }
   
member ::      { Member }
   : type_spec declarators ';'	{ ($1,[],$2) }

union_type  ::	{ Type }
   : UNION identifier SWITCH '(' switch_type_spec ')' '{' switch_body '}' 
 			{ TyUnion (Just $2) $5 (Id "tagged_union") Nothing (reverse $8) }

switch_type_spec :: { Type }
   : integer_type	{ $1 }
   | char_type		{ $1 }
   | boolean_type	{ $1 }
   | enum_type		{ $1 }
   | scoped_name	{ TyName $1 Nothing }

switch_body :: { [Switch] }
   : case		{ [$1]  }
   | switch_body case   { $2:$1 }

case :: { Switch }
   : case_labels element_spec ';' { Switch $1 (Just $2) }

case_labels :: { [CaseLabel] }
   : case_label			{ [$1]  }
   | case_labels case_label     { $2:$1 }

case_label :: { CaseLabel }
   : CASE const_expr ':'	{ Case [$2] }
   | DEFAULT ':'		{ Default }

element_spec :: { SwitchArm }
   : type_spec declarator	{ (Param $2 $1 []) }

enum_type :: { Type }
   : ENUM identifier '{' enumerators '}' { TyEnum (Just $2) (reverse $4) }

enumerators :: { [(Id,[Attribute],Maybe Expr)] }
   : identifier				{ [($1,[],Nothing)] }
   | enumerators ',' identifier		{ (($3,[],Nothing):$1) }

sequence_type :: { Type }
   : SEQUENCE '<' simple_type_spec ',' positive_int_const '>' { TySequence $3 (Just $5) }
   | SEQUENCE '<' simple_type_spec '>' { TySequence $3 Nothing }

string_type   :: { Type }
   : STRING '<' positive_int_const '>'	{ TyString (Just $3) }
   | STRING				{ TyString Nothing   }

wide_string_type   :: { Type }
   : WSTRING '<' positive_int_const '>'	{ TyWString (Just $3) }
   | WSTRING				{ TyWString Nothing   }

array_declarator :: { (Id, [Expr]) }
   : identifier fixed_array_sizes		{ ($1, reverse $2) }

fixed_array_sizes :: { [Expr] }
   : fixed_array_size			{  [$1] }
   | fixed_array_sizes fixed_array_size { $2:$1 }

fixed_array_size  :: { Expr }
   : '[' positive_int_const ']'		{ $2 }

attr_decl   :: { Defn }
   : opt_readonly ATTRIBUTE param_type_spec simple_declarators { Attribute (reverse $4) $1 $3 }

opt_readonly :: { Bool }
   : READONLY   { True } | {-empty-} { False }

simple_declarators :: { [Id] }
   : simple_declarator		{ [$1] }
   | simple_declarators ',' simple_declarator { $3:$1 }

except_decl :: { Defn }
   : EXCEPTION identifier '{' mb_members '}' { Exception $2 $4 }

mb_members :: { [Member] }
   : {-empty-}			{ [] }
   | members			{ $1 }

members :: { [Member] }
   : member			{ [$1] }
   | members member		{ $2:$1 }

op_decl :: { Defn }
   : opt_op_attribute op_type_spec identifier parameter_decls mb_raises_expr mb_context_expr
		{ Operation (FunId $3 Nothing $4) $2 $5 $6 }

opt_op_attribute :: { Bool }
   : {-nothing-}			{ False }
   | ONEWAY				{ True  }

op_type_spec :: { Type }
   : param_type_spec			{ $1 }
   | VOID				{ TyVoid }

parameter_decls :: { [Param] }
   : '(' param_decls ')'		{ (reverse $2) }
   | '(' ')'				{ [] }

param_decls :: { [Param] }
   : param_decl				{ [$1] }
   | param_decls ',' param_decl		{ $3:$1 }

param_decl :: { Param }
   : param_attribute param_type_spec simple_declarator { Param $3 $2 [$1] }

param_attribute :: { Attribute }
   : MODE	{ Mode $1 }

mb_raises_expr :: { Maybe Raises }
   : {-nothing-}		{ Nothing }
   | RAISES '(' scoped_name_list ')' { Just (reverse $3) }

mb_context_expr :: { Maybe Context }
   : {-nothing-}		     { Nothing }
   | CONTEXT '(' string_lit_list ')' { Just (reverse $3) }

scoped_name_list :: { [String] }
   : scoped_name			{ [$1] }
   | scoped_name_list ',' scoped_name	{ $3:$1 }

string_lit_list  :: { [String] }
   : STRING_LIT				{ [$1] }
   | string_lit_list ',' STRING_LIT     { $3:$1 }

param_type_spec :: { Type }
   : base_type_spec		{ $1 }
   | string_type		{ $1 }
   | wide_string_type		{ $1 }
   | fixed_pt_type		{ $1 }
   | scoped_name		{ TyName $1 Nothing }

fixed_pt_type	:: { Type }
   : FIXED '<' positive_int_const ',' integer_literal '>' { TyFixed (Just ($3,$5)) }

fixed_pt_const_type :: { Type }
   : FIXED	{ TyFixed Nothing }

integer_literal :: { IntegerLit }
   : LITERAL	{ let (IntegerLit il) = $1 in il }

string_literal  :: { String }
   : STRING_LIT { $1 }

identifier :: { Id }
    : ID   { (Id $1) }
    
{------------------ END OF GRAMMAR --------------}

{
happyError :: LexM a
happyError = do
 l   <- getSrcLoc
 str <- getStream
 ioToLexM (ioError (userError (show l ++ ": Parse error: " ++ takeWhile (/='\n') str)))
}

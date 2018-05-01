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
        ';'           { T_semi }
        MODULE        { T_module }
        INTERFACE     { T_interface }
        DICTIONARY    { T_dictionary }
        CALLBACK      { T_callback }
        SERIALIZER    { T_serializer }
        STRINGIFIER   { T_stringifier }
        ITERABLE      { T_iterable }
        MAPLIKE       { T_maplike }
        SETLIKE       { T_setlike }
        IMPLEMENTS    { T_implements }
        '('           { T_oparen }
        ')'           { T_cparen }
        '{'           { T_ocurly }
        '}'           { T_ccurly }
        ':'           { T_colon  }
        '::'          { T_dcolon  }
        '...'         { T_dotdotdot }
        ','           { T_comma }
        '.'           { T_dot }
        CONST         { T_const }
        '='           { T_equal }
        '=='          { T_eqeq }
        '!='          { T_neq }
        '|'           { T_or }
        OR            { T_or_keyword }
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
        OPTIONAL      { T_optional }
        REQUIRED      { T_required }
        UNRESTRICTED  { T_unrestricted }
        INHERIT       { T_inherit }
        IMPORT        { T_import }
        LITERAL       { T_literal $$ }
        STRING_LIT    { T_string_lit $$ }
        ID            { T_id $$ }
        ATTRIBUTE     { T_attribute }
        '+'           { T_plus }
        '*'           { T_times }
        '-'           { T_minus }
        WSTRING       { T_wstring }
        SEQUENCE      { T_sequence }
        RECORD        { T_record }
        PROMISE       { T_promise }
        FROZENARRAY   { T_frozenarray }
        OBJECT        { T_object }
        ANY           { T_any }
        BYTE          { T_int $$ }
        OCTET         { T_uint $$ }
        ONEWAY        { T_oneway }
        STATIC        { T_static }
        FIXED         { T_fixed }
        EXCEPTION     { T_exception }
        RAISES        { T_raises }
        CONTEXT       { T_context }
        GETTER        { T_getter }
        SETTER        { T_setter }
        DELETER       { T_deleter }
        READONLY      { T_readonly }
        INCLUDE_START { T_include_start $$ }
        INCLUDE_END   { T_include_end }
        PRAGMA        { T_pragma $$ }
        UNKNOWN       { T_unknown $$ }
%%

specification :: { [Defn] }
   : definitions             { (reverse $1) }

definitions  :: { [Defn] }
   :                         {    []   }
   | definitions definition  { $2 : $1 }

definition   :: { Defn }
   : type_decl opt_semiColon { $1 }
   | const_decl opt_semiColon { $1 }
   | except_decl opt_semiColon { $1 }
   | interface opt_semiColon { $1 }
   | dictionary opt_semiColon { $1 }
   | implements opt_semiColon { $1 }
   | module opt_semiColon   { $1 }
   | PRAGMA                 { Pragma $1 }
   | INCLUDE_START          { IncludeStart $1 }
   | INCLUDE_END            { IncludeEnd }

opt_semiColon :: { Bool }
   : ';' { True }
   | {-empty-} { False }

module :: { Defn }
   : MODULE identifier '{' definitions '}'  { Module $2 (reverse $4) }

interface :: { Defn }
   : interface_decl         { $1 }
   | opt_extended_attributes INTERFACE identifier   { Forward $3 }

interface_decl :: { Defn }
   : interface_header '{' exports '}'  { let (at, ty,ids,inherit) = $1 in Interface ids inherit (reverse $3) at ty }
   | opt_extended_attributes CALLBACK identifier '=' op_type_spec parameter_decls raises_exprs mb_context_expr { Interface $3 [] [Operation (FunId (Id "callback") Nothing $6) False $5 $7 $8 []] $1 (Just (Id "callback")) }
   | CALLBACK identifier '=' op_type_spec parameter_decls raises_exprs mb_context_expr { Interface $2 [] [Operation (FunId (Id "callback") Nothing $5) False $4 $6 $7 []] [] (Just (Id "callback")) }

interface_header :: { ([ExtAttribute], Maybe Id, Id, Inherit) }
   : opt_extended_attributes INTERFACE identifier opt_inheritance_spec { ($1,Nothing,$3,$4) }
   | opt_extended_attributes CALLBACK INTERFACE identifier opt_inheritance_spec { ($1,Just (Id "callback"),$4,$5) }
   | opt_extended_attributes identifier INTERFACE identifier opt_inheritance_spec { ($1,Just $2,$4,$5) }
   | identifier INTERFACE identifier opt_inheritance_spec { ([],Just $1,$3,$4) }

dictionary :: { Defn }
   : dictionary_decl         { $1 }
   | opt_extended_attributes DICTIONARY identifier   { Forward $3 }

dictionary_decl :: { Defn }
   : dictionary_header '{' dictionary_exports '}'  { let (at, ty,ids,inherit) = $1 in Interface ids inherit (reverse $3) at ty }

dictionary_header :: { ([ExtAttribute], Maybe Id, Id, Inherit) }
   : opt_extended_attributes DICTIONARY identifier opt_inheritance_spec { ($1,Nothing,$3,$4) }
   | opt_extended_attributes identifier DICTIONARY identifier opt_inheritance_spec { ($1,Just $2,$4,$5) }
   | identifier DICTIONARY identifier opt_inheritance_spec { ([],Just $1,$3,$4) }


implements :: { Defn }
   : identifier IMPLEMENTS identifier   { Implements $1 $3 }

exports :: { [Defn] }
   : {-empty-}                  {    []   }
   | exports export             { $2 : $1 }

export  :: { Defn }
   : type_decl ';'            { $1 }
   | const_decl ';'           { $1 }
   | except_decl ';'          { $1 }
   | attr_decl ';'            { $1 }
   | op_decl ';'              { $1 }
   | serializer_decl ';'      { $1 }
   | stringifier_decl ';'     { $1 }
   | iterable_decl ';'        { $1 }
   | maplike_decl ';'         { $1 }
   | setlike_decl ';'         { $1 }

dictionary_exports :: { [Defn] }
   : {-empty-}                  {    []   }
   | dictionary_exports dictionary_export             { $2 : $1 }

dictionary_export  :: { Defn }
   : dictionary_attr_decl ';'            { $1 }

opt_inheritance_spec :: { Inherit }
   : {-empty-}                { [] }
   | inheritance_spec         { $1 }

inheritance_spec :: { Inherit }
   : ':' scoped_name more_scoped_names  { $2:(reverse $3) }

scoped_name :: { String }
   : ID                                 { $1  }
   | '::' ID                            { ("::"++ $2)  }
   | scoped_name '::' ID                { $1 ++ ':':':':$3 }

more_scoped_names :: { Inherit }
   : {-empty-}                         { [] }
   | ',' scoped_name more_scoped_names { $2 : $3 }

{- constant declarations -}

const_decl :: { Defn }
   : CONST opt_extended_attributes const_type identifier '=' const_expr  { Constant $4 [] $3 $6 }
   | opt_extended_attributes CONST const_type identifier '=' const_expr  { Constant $4 [] $3 $6 }

const_type :: { Type }
   : integer_type               { $1 }
   | char_type                  { $1 }
   | wide_char_type             { $1 }
   | boolean_type               { $1 }
   | floating_pt_type           { $1 }
   | wide_string_type           { $1 }
   | fixed_pt_const_type        { $1 }
   | scoped_name                { TyName $1 Nothing }

const_expr :: { Expr }
   : or_expr                    { $1 }

or_expr :: { Expr }
   : xor_expr                   { $1 }
   | or_expr '|' xor_expr       { Binary Or $1 $3 }

xor_expr :: { Expr }
   : and_expr                   { $1 }
   | xor_expr '^' and_expr      { Binary Xor $1 $3 }

and_expr :: { Expr }
   : shift_expr                 { $1 }
   | and_expr '&' shift_expr    { Binary And $1 $3 }

shift_expr :: { Expr }
   : add_expr                   { $1 }
   | shift_expr SHIFT add_expr  { Binary (Shift $2) $1 $3 }

add_expr :: { Expr }
   : mult_expr                  { $1 }
   | add_expr '+' mult_expr     { Binary Add $1 $3 }
   | add_expr '-' mult_expr     { Binary Sub $1 $3 }

mult_expr :: { Expr }
   : unary_expr                 { $1 }
   | mult_expr '*' unary_expr   { Binary Mul $1 $3 }
   | mult_expr '/' unary_expr   { Binary Div $1 $3 }
   | mult_expr '%' unary_expr   { Binary Mod $1 $3 }

unary_expr :: { Expr }
   : unary_operator primary_expr { Unary $1 $2 }
   | primary_expr                { $1 }

unary_operator :: { UnaryOp }
   : '-'        { Minus }
   | '+'        { Plus  }
   | '~'        { Not   }

primary_expr :: { Expr }
   : scoped_name                { Var $1 }
   | literal                    { Lit $1 }

literal :: { Literal }
   : LITERAL                    { $1 }

positive_int_const :: { Expr }
   : const_expr                         { $1 }

{---- type declarations ------}

type_decl   :: { Defn }
   : TYPEDEF type_declarator            { let (spec, decls) = $2 in Typedef spec [] decls }
   | struct_type                        { TypeDecl $1 }
   | union_type                         { TypeDecl $1 }
   | enum_type                          { TypeDecl $1 }

type_declarator :: { (Type, [Id]) }
   : type_spec declarators              { ($1,$2) }

type_spec :: { Type }
   : simple_type_spec                   { $1 }
   | constr_type_spec                   { $1 }
   | type_spec '[' ']'                  { TySafeArray $1 }
   | '(' sum_type ')'                   { TySum $2 }

simple_type_spec :: { Type }
   : base_type_spec                     { $1 }
   | template_type_spec                 { $1 }
   | scoped_name                        { TyName $1 Nothing }

base_type_spec :: { Type }
   : floating_pt_type                   { $1 }
   | integer_type                       { $1 }
   | char_type                          { $1 }
   | wide_char_type                     { $1 }
   | boolean_type                       { $1 }
   | any_type                           { $1 }
   | object_type                        { $1 }

template_type_spec :: { Type }
   : sequence_type                      { $1 }
   | record_type                        { $1 }
   | promise_type                       { $1 }
   | frozenarray_type                   { $1 }
   | wide_string_type                   { $1 }
   | fixed_pt_type                      { $1 }

constr_type_spec :: { Type }
   : struct_type                        { $1 }
   | union_type                         { $1 }
   | enum_type                          { $1 }

declarators :: { [Id] }
   : declarator                         { [$1] }
   | declarators ',' declarator         { $3 : $1 }

simple_declarator ::  { Id }
   : identifier                         { $1 }

exposure_decls :: { [Id] }
   : '(' expose_decls ')'                { (reverse $2) }
   | '(' ')'                            { [] }

expose_decls :: { [Id] }
   : identifier                         { [$1] }
   | expose_decls ',' identifier         { $3:$1 }

extended_attribute ::  { ExtAttribute }
   : identifier                         { ExtAttr $1 [] }
   | identifier '=' identifier          { ExtAttr $1 [] }
   | identifier '=' DEFAULT             { ExtAttr $1 [] }
   | identifier '=' const_type          { ExtAttr $1 [] }
   | identifier '=' const_expr          { ExtAttr $1 [] }
   | identifier '=' literal             { ExtAttr $1 [] }
   | identifier '=' STRING_LIT          { ExtAttr $1 [] }
   | identifier '=' '(' string_lit_list ')' { ExtAttr $1 [] }
   | identifier '=' identifier parameter_decls { ExtAttr $1 $4 }
   | identifier '=' exposure_decls { ExtAttr $1 [] }
   | identifier parameter_decls { ExtAttr $1 $2 }

extended_attributes :: { [ExtAttribute] }
   : extended_attribute                         { [$1] }
   | extended_attributes ',' extended_attribute { $3 : $1 }
   | extended_attributes ','                    { $1 }
   | {-empty-} { [] }

declarator :: { Id }
   : identifier                         { $1 }
   | identifier fixed_array_sizes       { ArrayId $1 $2 }

floating_pt_type :: { Type }
   : FLOAT                      { TyFloat $1 }
   | UNRESTRICTED FLOAT         { TyFloat $2 }

integer_type :: { Type }
   : INTEGER                    { TyInteger $1 }
   | INTEGER INTEGER            { TyInteger LongLong }
   | SIGNED INTEGER             { TyApply (TySigned True)  (TyInteger $2) }
   | SIGNED INTEGER INTEGER     { TyApply (TySigned True)  (TyInteger LongLong) }
   | UNSIGNED INTEGER           { TyApply (TySigned False) (TyInteger $2) }
   | UNSIGNED INTEGER INTEGER   { TyApply (TySigned False) (TyInteger LongLong) }
   | BYTE                       { TyApply (TySigned True)  (TyInteger Byte) }
   | OCTET                      { TyApply (TySigned False) (TyInteger Byte) }

char_type :: { Type }
   : CHAR       { TyChar }

wide_char_type :: { Type }
   : WCHAR      { TyWChar }


boolean_type :: { Type }
   : BOOLEAN    { TyBool }

any_type     :: { Type }
   : ANY        { TyAny }

object_type  :: { Type }
   : OBJECT     { TyObject }

struct_type :: { Type }
   : STRUCT identifier '{' member_list '}' { TyStruct (Just $2) $4 Nothing }

member_list :: { [Member] }
   : member             { [$1] }
   | member_list member { $2:$1 }

member ::      { Member }
   : type_spec declarators ';'  { ($1,[],$2) }

union_type  ::  { Type }
   : UNION identifier SWITCH '(' switch_type_spec ')' '{' switch_body '}'
                        { TyUnion (Just $2) $5 (Id "tagged_union") Nothing (reverse $8) }

switch_type_spec :: { Type }
   : integer_type       { $1 }
   | char_type          { $1 }
   | boolean_type       { $1 }
   | enum_type          { $1 }
   | scoped_name        { TyName $1 Nothing }

switch_body :: { [Switch] }
   : case               { [$1]  }
   | switch_body case   { $2:$1 }

case :: { Switch }
   : case_labels element_spec ';' { Switch $1 (Just $2) }

case_labels :: { [CaseLabel] }
   : case_label                 { [$1]  }
   | case_labels case_label     { $2:$1 }

case_label :: { CaseLabel }
   : CASE const_expr ':'        { Case [$2] }
   | DEFAULT ':'                { Default }

element_spec :: { SwitchArm }
   : type_spec declarator       { (Param Required $2 $1 [] []) }

enum_type :: { Type }
   : opt_extended_attributes ENUM identifier '{' enumerators '}' { TyEnum (Just $3) (reverse $5) }

enumerators :: { [(Either Id String,[Attribute],Maybe Expr)] }
   : enumItem                           { [$1] }
   | enumerators ',' enumItem           { $3:$1 }

enumItem :: { (Either Id String,[Attribute],Maybe Expr) }
   : identifier { (Left $1,[],Nothing) }
   | STRING_LIT { (Right $1,[],Nothing) }

sequence_type :: { Type }
   : SEQUENCE '<' type_spec '>' { TySequence $3 Nothing }

record_type :: { Type }
   : RECORD '<' simple_type_spec ',' simple_type_spec '>' { TyRecord $3 $5 }

promise_type :: { Type }
   : PROMISE '<' op_type_spec '>' { TyPromise $3 }
   | PROMISE { TyPromise TyAny }

frozenarray_type :: { Type }
   : FROZENARRAY '<' simple_type_spec '>' { TyFrozenArray $3 }

wide_string_type   :: { Type }
   : WSTRING '<' positive_int_const '>' { TyWString (Just $3) }
   | WSTRING                            { TyWString Nothing   }

array_declarator :: { (Id, [Expr]) }
   : identifier fixed_array_sizes               { ($1, reverse $2) }

fixed_array_sizes :: { [Expr] }
   : fixed_array_size                   {  [$1] }
   | fixed_array_sizes fixed_array_size { $2:$1 }

fixed_array_size  :: { Expr }
   : '[' positive_int_const ']'         { $2 }

attr_decl   :: { Defn }
   : opt_extended_attributes opt_stringifier opt_op_attribute opt_readonly ATTRIBUTE param_type_spec simple_declarators raises_exprs { Attribute (reverse $7) $4 $6 $8 $1 }
   | opt_extended_attributes opt_stringifier opt_op_attribute opt_readonly ATTRIBUTE opt_extended_attributes param_type_spec simple_declarators raises_exprs { Attribute (reverse $8) $4 $7 $9 ($1 ++ $6) }
   | opt_extended_attributes opt_op_attribute opt_readonly ATTRIBUTE param_type_spec simple_declarators raises_exprs { Attribute (reverse $6) $3 $5 $7 $1 }
   | opt_extended_attributes opt_op_attribute opt_readonly ATTRIBUTE opt_extended_attributes param_type_spec simple_declarators raises_exprs { Attribute (reverse $7) $3 $6 $8 ($1 ++ $5) }
   | opt_extended_attributes INHERIT opt_readonly ATTRIBUTE param_type_spec simple_declarators raises_exprs { Attribute (reverse $6) $3 $5 $7 $1 }

dictionary_attr_decl   :: { Defn }
   : opt_extended_attributes opt_required param_type_spec simple_declarators raises_exprs { DictionaryAttribute (reverse $4) $2 $3 $5 $1 }
   | opt_extended_attributes opt_required param_type_spec simple_declarators raises_exprs '=' param_default { DictionaryAttribute (reverse $4) $2 $3 $5 $1 }
   | opt_required opt_extended_attributes param_type_spec simple_declarators raises_exprs { DictionaryAttribute (reverse $4) $1 $3 $5 $2 }
   | opt_required opt_extended_attributes param_type_spec simple_declarators raises_exprs '=' param_default { DictionaryAttribute (reverse $4) $1 $3 $5 $2 }

opt_readonly :: { Bool }
   : READONLY   { True } | {-empty-} { False }

opt_required :: { Bool }
   : REQUIRED   { True } | {-empty-} { False }

opt_stringifier :: { Bool }
   : STRINGIFIER   { True } | {-empty-} { False }

opt_extended_attributes :: { [ExtAttribute] }
   : '[' extended_attributes ']' { $2 }
   | {-empty-} { [] }

simple_declarators :: { [Id] }
   : simple_declarator          { [$1] }
   | simple_declarators ',' simple_declarator { $3:$1 }

except_decl :: { Defn }
   : opt_extended_attributes EXCEPTION identifier '{' exports '}' { Interface $3 [] (reverse $5) $1 Nothing }

serializer_decl :: { Defn }
   : SERIALIZER '=' '{' serializer_declarators '}' { Serializer }

serializer_declarators :: { [Id] }
   : serializer_declarator          { [$1] }
   | serializer_declarators ',' serializer_declarator { $3:$1 }

serializer_declarator ::  { Id }
   : identifier                         { $1 }
   | INHERIT                            { Id "inherit" }
   | ATTRIBUTE                          { Id "attribute" }

stringifier_decl :: { Defn }
   : opt_extended_attributes STRINGIFIER { Stringifier }
--   | opt_extended_attributes STRINGIFIER identifier '(' ')' { Stringifier }

iterable_decl :: { Defn }
   : opt_extended_attributes ITERABLE '<' iterable_declarators '>' { Iterable }

maplike_decl :: { Defn }
   : opt_extended_attributes opt_op_attribute opt_readonly MAPLIKE '<' identifier ',' identifier '>' { MapLike }

setlike_decl :: { Defn }
   : opt_extended_attributes opt_op_attribute opt_readonly SETLIKE '<' identifier '>' { SetLike }

iterable_declarators :: { [Id] }
   : iterable_declarator          { [$1] }
   | iterable_declarators ',' iterable_declarator { $3:$1 }

iterable_declarator ::  { Id }
   : identifier                         { $1 }
   | ATTRIBUTE                          { Id "attribute" }

op_decl :: { Defn }
   : opt_extended_attributes opt_op_attribute op_type_spec identifier parameter_decls raises_exprs mb_context_expr
                { Operation (FunId $4 Nothing $5) $2 $3 $6 $7 $1 }
   | opt_extended_attributes GETTER getter_type_spec getter_decl { let (id, pr, re, ctx) = $4 in Operation (FunId id Nothing pr) False $3 re ctx (ExtAttr (Id "Getter") []:$1) }
   | opt_extended_attributes SETTER op_type_spec setter_decl { let (id, pr, re, ctx) = $4 in Operation (FunId id Nothing pr) False $3 re ctx $1 }
   | opt_extended_attributes DELETER op_type_spec deleter_decl { let (id, pr, re, ctx) = $4 in Operation (FunId id Nothing pr) False $3 re ctx $1 }

-- getter_id :: { Id }
--    : identifier  { $1 }
--    : {-empty-} { Getter }

getter_decl :: { (Id, [Param], [Raises], Maybe Context)  }
  : parameter_decls raises_exprs mb_context_expr
                { (Getter, $1, $2, $3) }
  | identifier parameter_decls raises_exprs mb_context_expr
                { ($1, $2, $3, $4) }

setter_decl :: { (Id, [Param], [Raises], Maybe Context)  }
  : parameter_decls raises_exprs mb_context_expr
                { (Setter, $1, $2, $3) }
  | identifier parameter_decls raises_exprs mb_context_expr
                { ($1, $2, $3, $4) }

deleter_decl :: { (Id, [Param], [Raises], Maybe Context)  }
  : parameter_decls raises_exprs mb_context_expr
                { (Deleter, $1, $2, $3) }
  | identifier parameter_decls raises_exprs mb_context_expr
                { ($1, $2, $3, $4) }

opt_op_attribute :: { Bool }
   : {-nothing-}                        { False }
   | ONEWAY                             { False  }
   | STATIC                             { True  }
--    | GETTER                             { False  } -- TODO

getter_type_spec :: { Type }
   : base_type_spec                     { $1 }
   | scoped_name                        { TyName $1 Nothing }
   | getter_type_spec '?'               { TyOptional $1 }
   | '(' sum_type ')'                   { TySum $2 }

op_type_spec :: { Type }
   : param_type_spec                    { $1 }
   | VOID                               { TyVoid }

parameter_decls :: { [Param] }
   : '(' param_decls ')'                { (reverse $2) }
   | '(' ')'                            { [] }

param_decls :: { [Param] }
   : param_decl                         { [$1] }
   | param_decls ',' param_decl         { $3:$1 }

param_default :: { () }
   : literal     { () }
   | STRING_LIT  { () }
   | '(' string_lit_list ')' { () }
   | const_expr  { () }
   | '[' ']'     { () }

param_decl :: { Param }
   : param_attribute opt_extended_attributes OPTIONAL param_type_spec simple_declarator '=' param_default { Param Optional $5 $4 [$1] $2 }
   | param_attribute opt_extended_attributes OPTIONAL param_type_spec simple_declarator { Param Optional $5 $4 [$1] $2 }
   | param_attribute opt_extended_attributes param_type_spec simple_declarator { Param Required $4 $3 [$1] $2 }
   | param_attribute OPTIONAL opt_extended_attributes param_type_spec simple_declarator '=' param_default { Param Optional $5 $4 [$1] $3 }
   | param_attribute OPTIONAL opt_extended_attributes param_type_spec simple_declarator { Param Optional $5 $4 [$1] $3 }

param_attribute :: { Attribute }
   : {-nothing-}        { Mode In }
   | MODE               { Mode $1 }

raises_expr :: { Raises }
   : RAISES '(' scoped_name_list ')' { Raises (reverse $3) }
   | GETTER RAISES '(' scoped_name_list ')' { GetRaises (reverse $4) }
   | SETTER RAISES '(' scoped_name_list ')' { SetRaises (reverse $4) }

raises_exprs :: { [Raises] }
   : raises_expr { [$1] }
   | raises_exprs ',' raises_expr     { $3:$1 }
   | {-nothing-}                { [] }

mb_context_expr :: { Maybe Context }
   : {-nothing-}                     { Nothing }
   | CONTEXT '(' string_lit_list ')' { Just (reverse $3) }

scoped_name_list :: { [String] }
   : scoped_name                        { [$1] }
   | scoped_name_list ',' scoped_name   { $3:$1 }

string_lit_list  :: { [String] }
   : STRING_LIT                         { [$1] }
   | string_lit_list ',' STRING_LIT     { $3:$1 }

param_type_spec :: { Type }
   : base_type_spec                     { $1 }
   | template_type_spec                 { $1 }
   | scoped_name                        { TyName $1 Nothing }
   | param_type_spec '[' ']'            { TySafeArray $1 }
   | param_type_spec '...'              { TySafeArray $1 }
   | '(' sum_type ')'                   { TySum $2 }
   | param_type_spec '?'                { TyOptional $1 }

sum_type  :: { [Type] }
   : param_type_spec                         { [$1] }
   | sum_type OR param_type_spec     { $3:$1 }

fixed_pt_type   :: { Type }
   : FIXED '<' positive_int_const ',' integer_literal '>' { TyFixed (Just ($3,$5)) }

fixed_pt_const_type :: { Type }
   : FIXED      { TyFixed Nothing }

integer_literal :: { IntegerLit }
   : LITERAL    { let (IntegerLit il) = $1 in il }

string_literal  :: { String }
   : STRING_LIT { $1 }

identifier :: { Id }
    : ID       { (Id $1) }
--    | ATTRIBUTE { (Id "attribute") }
    | DEFAULT  { (Id "default") }
    | OPTIONAL { (Id "optional") }
    | CONTEXT  { (Id "context") }
    | CALLBACK { (Id "callback") }
    | REQUIRED { (Id "required") }
    | RECORD   { (Id "record") }
    | IMPORT   { (Id "import") }

{------------------ END OF GRAMMAR --------------}

{
happyError :: LexM a
happyError = do
 l   <- getSrcLoc
 str <- getStream
 ioToLexM (ioError (userError (show l ++ ": Parse error: " ++ takeWhile (/='\n') str)))
}

%
% (c) The Foo Project, University of Glasgow, 1998
%
% @(#) $Docid: Aug. 28th 2001  12:06  Sigbjorn Finne $
% @(#) $Contactid: sof@galconn.com $
%
% Modified by Dimitry Golubovsky for W3DOM IDL -> Haskell Converter
%

Abstract Syntax Tree for OMG/MS IDL

Declarations:
\begin{code}
module IDLSyn where

import Literal
import BasicTypes

\end{code}

The abstract syntax for declarations/definitions in IDL.

\begin{code}
data Defn 
 = Typedef    Type [Attribute] [Id]
 | TypeDecl   Type  --structs, unions (w/ tag) and enums.
 | ExternDecl Type [Id]
 | Constant   Id [Attribute] Type Expr
 | Attributed [Attribute] Defn
 | Attribute  [Id] Bool Type [Raises] [ExtAttribute]
 | DictionaryAttribute  [Id] Bool Type [Raises] [ExtAttribute]
 | Operation  Id Bool Type {-[Param]-} [Raises] (Maybe Context) [ExtAttribute]
 | Exception  Id [Defn]
 | Interface  Id Inherit [Defn] [ExtAttribute] (Maybe Id)
 | Implements Id Id
 | Forward    Id
   -- MS specific declaration groups:
 | Module        Id [Defn]
 | DispInterface Id [([Attribute],Type,Id)] [Defn]
 | DispInterfaceDecl Id Id
 | CoClass Id [CoClassMember]
 | Library Id [Defn]
 | CppQuote String
 | HsQuote  String
 | CInclude String
 | Import    [(String,[Defn])]
 | ImportLib    String -- importing a type library.
 | Pragma       String
 | IncludeStart String
 | IncludeEnd
 | Serializer
 | Stringifier
 | Iterable
 | SetLike
 | MapLike
   deriving ( Eq, Show )

\end{code}

An identifier could be followed by bracketed expressions(?)
specifying the dimensions of the array, i.e., foo[2][3]

Notice that it is convenient for the parser to associate
attributes with definitions and parameters, and not with IDL
Ids directly (as is done in CoreIDL).

That being said, an 'attributed Id' is also provided, not used
by the parser, but the desugarer employs it to decorate Ids
prior to CoreIDL translation.

\begin{code}
data Id 
 = Id String 
 | AttrId  [Attribute] Id  
 | ArrayId Id [Expr]  -- 0,1 or 2 expressions.
 | Pointed [[Qualifier]] Id
 | CConvId CallConv Id
 | BitFieldId Int   Id
 | Getter
 | Setter
 | Deleter
 | FunId Id (Maybe CallConv) [Param]
   deriving ( Eq, Ord, Show )
 
\end{code}

Big type encoding type constants and constructors provided
with IDL.

\begin{code}
data Type
 = TyApply Type Type
 | TyInteger Size
 | TyFloat   Size
 | TyStable
 | TyChar
 | TySigned  Bool
 | TyWChar
 | TyBool   
 | TyAny    
 | TyObject
 | TyStruct (Maybe Id) [Member] (Maybe Int)
 | TyString (Maybe Expr)
 | TyWString (Maybe Expr)
 | TySequence Type (Maybe Expr)
 | TyRecord Type Type
 | TyPromise Type
 | TyFrozenArray Type
 | TyFixed (Maybe (Expr, IntegerLit))
 | TyName String (Maybe Type)
 | TyIface Name
 | TyUnion (Maybe Id) Type Id (Maybe Id) [Switch] -- encapsulated union.
 | TyEnum (Maybe Id) [(Either Id String, [Attribute], Maybe Expr)]
   -- MS specific types:
 | TyUnionNon (Maybe Id) [Switch] -- non-encapsulated union.
 | TyCUnion (Maybe Id) [Member]
            (Maybe Int)     -- C-style union.
 | TyBString
 | TyPointer Type
 | TyArray Type [Expr]
 | TySafeArray Type
 | TySum [Type]
 | TyOptional Type
 | TyFun (Maybe CallConv) Type [Param]
 | TyVoid
 | TyQualifier Qualifier
   deriving ( Eq, Ord, Show )

data Expr
 = Binary BinaryOp Expr Expr
 | Cond   Expr Expr Expr
 | Unary  UnaryOp Expr
 | Var    Name
 | Lit    Literal
 | Cast   Type Expr
 | Sizeof Type
   deriving ( Eq, Ord, Show )

data Raises = GetRaises [Name] | SetRaises [Name] | Raises [Name] deriving ( Eq, Ord, Show )
getterRaises = filter isGet
    where
        isGet (SetRaises _) = False
        isGet _ = True
setterRaises = filter isSet
    where
        isSet (GetRaises _) = False
        isSet _ = True

type Context = [String]

type CoClassMember = (Bool, Id, [Attribute])

data Optionality = Optional | Required deriving ( Eq, Ord, Show )

data Param      = Param Optionality Id Type [Attribute] [ExtAttribute]
                  deriving ( Eq, Ord, Show )

type Member     = (Type, [Attribute], [Id])

data Attribute  
 = Attrib Id [AttrParam]  -- name(e1,..,en)
 | Mode ParamDir
   deriving ( Eq, Ord, Show )

data AttrParam 
  = AttrExpr Expr
  | EmptyAttr         -- size_is(,e) => [EmptyAttr,attr_param e]
  | AttrLit Literal
  | AttrPtr AttrParam
   deriving ( Eq, Ord, Show )

data ExtAttribute
  = ExtAttr Id [Param]
   deriving ( Eq, Ord, Show )

data Switch    = Switch [CaseLabel] (Maybe SwitchArm) 
                 deriving ( Eq, Ord, Show )
data CaseLabel = Case [Expr] | Default
                 deriving ( Eq, Ord, Show )

-- switch arms can have attributes along with type and
-- declarator, just as proc. params.
type SwitchArm = Param

data GNUAttrib
 = Packed
 | CConv         CallConv
 | Unsupported   String
   deriving ( Eq, Show )
\end{code}

\begin{code}

-- Portions of IDLUtils.lhs necessary for DOM -> Haskell converter

module IDLUtils where

import IDLSyn
import BasicTypes

tyTag :: Type -> String
tyTag (TyStruct   (Just i) _ _)     = iName i
tyTag (TyEnum     (Just i) _)       = iName i
tyTag (TyUnion    (Just i) _ _ _ _) = iName i
tyTag (TyUnionNon (Just i) _)       = iName i
tyTag (TyCUnion   (Just i) _ _)     = iName i
tyTag (TyName nm _)                 = nm
tyTag _                             = ""


iName :: Id -> Name
iName (Id s)        = s
iName (ArrayId i _) = iName i
iName (Pointed _ i) = iName i
iName (CConvId _ i) = iName i
iName (AttrId _ i)  = iName i
iName (BitFieldId _ i) = iName i
iName (FunId i _ _) = iName i

getDef :: Defn -> String
getDef d =
 case d of
   Typedef _ _ (i:_)        -> iName i
   Attributed _ d1          -> getDef d1
   ExternDecl _ [i]         -> iName i
   Operation i _ _ _        -> iName i
   Interface (Id i) _ _     -> i
   Module (Id i) _          -> i
   DispInterface (Id i) _ _ -> i
   CoClass (Id i) _         -> i
   Library (Id i) _         -> i
   TypeDecl t               -> tyTag t
   _                        -> ""

getUses :: Defn -> [String]
getUses d = 
  case d of 
    Typedef ty _ _        -> getTyUses ty
    Constant _ _ ty _     -> getTyUses ty  -- expressions will never, 
                                           -- ever have free variables (in fact, const is a foreign co
                                           -- to typelibs.)
    Interface _ is ds     -> is ++ concatMap getUses ds
    Module _ ds           -> concatMap getUses ds
    DispInterface _ ps ds -> concatMap (\ (_,t, _) -> getTyUses t) ps ++ concatMap getUses ds
    CoClass _ cs          -> map (\ (_,Id i,_) -> i) cs
    Library _ ds          -> concatMap getUses ds
    Attributed _ d1       -> getUses d1
    TypeDecl t            -> getTyUses t
    ExternDecl t _        -> getTyUses t
    Operation (FunId _ _ ps) r _ _ -> getTyUses r ++ concatMap (\ (Param _ t _) -> getTyUses t) ps
    _                     -> []

-- Since the types were constructed from type libraries, we can make
-- a number of simplifying assumptions.
getTyUses :: Type -> [String]
getTyUses ty =
  case ty of
    TyName  n _     -> [n]
    TyIface n       -> [n]
    TySafeArray t   -> getTyUses t
    TyArray t _     -> getTyUses t
    TyPointer t     -> getTyUses t
    TyCUnion _ fs _ -> concatMap (\ (t,_,_) -> getTyUses t) fs
    TyStruct (Just (Id n)) [] _ -> [n]
    TyStruct _ fs _ -> concatMap (\ (t,_,_) -> getTyUses t) fs
    TyEnum (Just (Id n)) []   -> [n]
    TyApply t1 t2             -> getTyUses t1 ++ getTyUses t2
    _                         -> []

\end{code}

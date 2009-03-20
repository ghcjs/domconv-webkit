%
%
%

A symbol table abstraction that stores keywords as separate from
user defined type IDs.

\begin{code}
module SymbolTable 

       (
         SymbolTable
       , mkSymbolTable   -- :: [(String, tok)] -> SymbolTable tok
       , newContext      -- :: SymbolTable tok -> SymbolTable tok
       , lookupSymbol    -- :: SymbolTable tok -> String -> Maybe tok
       , lookupType      -- :: SymbolTable tok -> String -> Maybe tok

       , addType         -- :: SymbolTable tok -> String -> tok -> SymbolTable tok
       , addKeyword      -- :: SymbolTable tok -> String -> tok -> SymbolTable tok

       , combineSyms     -- :: SymbolTable tok -> SymbolTable tok -> SymbolTable tok

       ) where

import FiniteMap

\end{code}

\begin{code}
data SymbolTable elt =
  SymbolTable 
    (FiniteMap String elt)   -- keywords
    (FiniteMap String elt)   -- user defined types (via typedef.)

mkSymbolTable :: [(String,tok)] -> SymbolTable tok
mkSymbolTable ls = SymbolTable (listToFM ls) emptyFM

newContext :: SymbolTable tok -> SymbolTable tok
newContext (SymbolTable keyw _) = SymbolTable keyw emptyFM

combineSyms :: SymbolTable tok -> SymbolTable tok -> SymbolTable tok
combineSyms (SymbolTable kw1 ty1) (SymbolTable _ ty2) =
  -- at the moment, we consider the kw bit to be constant.
  SymbolTable kw1 (plusFM ty1 ty2)

addKeyword :: SymbolTable tok -> String -> tok -> SymbolTable tok
addKeyword (SymbolTable kw ty) str tok =
 SymbolTable (addToFM kw str tok) ty

addType :: SymbolTable tok -> String -> tok -> SymbolTable tok
addType (SymbolTable kw ty) str tok =
 SymbolTable kw (addToFM ty str tok)

lookupSymbol :: SymbolTable tok -> String -> Maybe tok
lookupSymbol (SymbolTable kw ty) str = 
  case lookupFM kw str of
    Nothing -> lookupFM ty str
    v       -> v

lookupType :: SymbolTable tok -> String -> Maybe tok
lookupType (SymbolTable _ ty) str = lookupFM ty str

\end{code}

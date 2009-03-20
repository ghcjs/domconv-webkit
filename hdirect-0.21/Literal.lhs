%
% (c) The Foo Project, University of Glasgow 1998
% @(#) $Docid: Oct. 9th 1999  15:28  Sigbjorn Finne $
% @(#) $Contactid: v-sfinne@microsoft.com $
%

\begin{code}
module Literal 
	(
	  Literal(..)

	, IntegerLit(..)
	, iLit			-- :: Integral a => a -> Literal
	, iLitToIntegral	-- :: Integral a => IntegerLit -> a
	, iLitToInteger		-- :: IntegerLit -> Integer
	
	, ppLit
	, ppILit
	, litToString
	
	) where

import PP
import Utils
import List ( intersperse )
import Opts ( optNoQualNames )
\end{code}

\begin{code}
data Literal
 = IntegerLit  IntegerLit
 | StringLit   String
 | TypeConst   String    -- type constant.
 | WStringLit  String
 | CharLit     Char
 | WCharLit    Char
 | FixedPtLit  Rational
 | FloatingLit (String, Double)
 | BooleanLit  Bool
 | NullLit
 | GuidLit     [String]
 | LitLit      String   -- lit-lits live on.
   deriving ( 
              Show -- for Lex debugging only
            , Eq
	    ) 

data IntegerLit = ILit Int{-base-} Integer
                  deriving (
		             Show -- for Lex debugging only
			   , Eq
			   ) 
iLit :: Integral a => a -> Literal
iLit x = IntegerLit (ILit 10 (toInteger x))

iLitToIntegral :: Integral a => IntegerLit -> a
iLitToIntegral (ILit _ x) = fromInteger x

iLitToInteger :: IntegerLit -> Integer
iLitToInteger (ILit _ x) = x

litToString :: Literal -> String
litToString l = 
  case l of
    IntegerLit  (ILit _ v) -> show v
    StringLit   s -> s
    TypeConst   s -> s
    WStringLit  s -> s
    CharLit     c -> [c]
    WCharLit    c -> [c]
    FixedPtLit  r -> show r
    FloatingLit (s,_) -> s
    BooleanLit  b     -> show b
    GuidLit     [g]   -> g
    NullLit           -> "0"
    GuidLit     gs    -> '{':concat (intersperse "-" gs) ++ "}"
    LitLit      s     -> s

\end{code}

%*
%
\subsection{Pretty printing literals}
%
%*

\begin{code}
ppLit :: Literal -> PPDoc a
ppLit (IntegerLit ilit)   = ppILit ilit
ppLit (StringLit s)       = text (show s)
ppLit (WStringLit ws)     = text (show ws)
ppLit (TypeConst s)       = text s
ppLit (CharLit c)         = text (show c)
ppLit (WCharLit c)        = text (show c)
ppLit (FixedPtLit r)      = rational r
ppLit (FloatingLit (d,x)) 
  | x < 0.0   = parens (text d)
  | otherwise = text d
ppLit (BooleanLit b)
  | optNoQualNames = text (if b then "True" else "False")
  | otherwise      = text (if b then "Prelude.True" else "Prelude.False")

ppLit (GuidLit ls)        = hcat (punctuate (char '-') (map text ls))
ppLit (LitLit ls)         = text ls
ppLit NullLit             = text "NULL"

ppILit :: IntegerLit -> PPDoc a
ppILit (ILit base val) = 
  case base of
     8  -> text (showOct val "")
     16 -> text (showHex val "")
     10 | val < 0   -> parens (integer val)
        | otherwise -> integer val
     _  -> trace ("ppILit: No one told me that base " ++ 
                  show base ++ " was supported!\n") $
           integer val

\end{code}

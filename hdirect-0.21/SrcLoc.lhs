%
% @(#) $Docid: Nov. 2nd 2000  13:43  Sigbjorn Finne $
% @(#) $Contactid: sof@microsoft.com $
%

\begin{code}
module SrcLoc 
       (
          SrcLoc
        , mkSrcLoc
	, modSrcLoc
	, dummySrcLoc
        , incSrcLineNo
        , ppSrcLoc
	) where

import PP
\end{code}

\begin{code}
data SrcLoc
 = SrcLoc String  -- module name
          Int     -- line number
          
 | NoSrcLoc

mkSrcLoc :: String -> Int -> SrcLoc
mkSrcLoc = SrcLoc

modSrcLoc :: SrcLoc -> String
modSrcLoc (SrcLoc s _) = s
modSrcLoc NoSrcLoc     = error "SrcLoc.modSrcLoc: tried to get the module of a NoSrcLoc"

dummySrcLoc :: SrcLoc
dummySrcLoc = NoSrcLoc

incSrcLineNo :: SrcLoc -> SrcLoc
incSrcLineNo NoSrcLoc = NoSrcLoc
incSrcLineNo (SrcLoc mod l) = SrcLoc mod (l+1)
\end{code}

\begin{code}
ppSrcLoc :: SrcLoc -> PPDoc a
ppSrcLoc (SrcLoc mod lno) = text mod <> char ':' <> int lno
ppSrcLoc NoSrcLoc = text "<unknown module, unknown line>"

instance Show SrcLoc where 
  showsPrec _ loc = \ str -> (showPPDoc (ppSrcLoc loc) undefined) ++ str

\end{code}

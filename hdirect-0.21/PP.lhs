%
% Derived pretty printing functionality
%

The @PP@ interface augments the standard @Pretty@ interface
a little for the task at hand.

\begin{code}
module PP
       (
	 PPDoc
       , showPPDoc
       , getPPEnv
       , setPPEnv
       
	 -- additional pp combinators
	 -- (pinched from GC source.)
       , vsep
       , joinedBy
       , ppDecls
       , withSemi
       , ppTuple
       , ppTupleVert
       , ppList
       , ppListVert

       -- re-export of (lifted) Pretty 
       -- functionality
       , empty
       , isEmpty
       , nest
       , text
       , ptext
       , char
       , int
       , integer
       , float
       , double
       , rational
       , parens
       , brackets
       , braces
       , quotes
       , doubleQuotes
       , semi
       , comma
       , colon
       , space
       , equals
       , lparen, rparen
       , lbrack, rbrack
       , lbrace, rbrace
       , (<>)
       , (<+>)
       , hcat
       , hsep
       , ($$)
       , ($+$)
       , vcat
       , sep
       , cat
       , fsep
       , fcat
       , hang
       , punctuate

       , render
	
       ) where

import qualified Pretty as P

type PPDoc a = a -> P.Doc

showPPDoc ::PPDoc a -> a -> String
showPPDoc f v = show (f v)

\end{code}

\begin{code}
vsep :: [PPDoc a] -> PPDoc a
vsep ds = ds `joinedBy` ($+$)

joinedBy :: [PPDoc a] -> (PPDoc a -> PPDoc a -> PPDoc a) -> PPDoc a
[] `joinedBy` _    = empty
xs `joinedBy` sep1 = foldr1 sep1 xs

ppDecls :: [PPDoc a] -> PPDoc a
ppDecls [] = empty
ppDecls ls = vsep (punctuate semi ls) <> semi

withSemi :: PPDoc a -> PPDoc a
withSemi d 
  | isEmpty d = d
  | otherwise = d <> semi

ppTuple :: [PPDoc a] -> PPDoc a
ppTuple ls = parens (hsep (punctuate comma ls))

ppTupleVert :: [PPDoc a] -> PPDoc a
ppTupleVert []     = ppTuple []
ppTupleVert (a:as) =
  vsep (char '(' <+> a : map ((<+>) comma) as) <+> char ')'

ppListVert :: [PPDoc a] -> PPDoc a
ppListVert []     = brackets empty
ppListVert [a]    = brackets ( a )
ppListVert (a:as) =
  vsep (char '[' <+> a : map ((<+>) comma) as) $$
  char ']'

ppList :: [PPDoc a] -> PPDoc a
ppList ls = brackets (hsep (punctuate comma ls))

getPPEnv :: (a -> PPDoc a) -> PPDoc a
getPPEnv f = \ v -> (f v) v

setPPEnv :: a -> PPDoc a -> PPDoc b
setPPEnv v f = \ _ -> f v
\end{code}

Lifting Pretty's functionality up into PPDocs.

\begin{code}
empty :: PPDoc a
empty _ = P.empty

isEmpty :: PPDoc a -> Bool
isEmpty d      = P.isEmpty (d undefined)

nest :: Int -> PPDoc a -> PPDoc a
nest i d       = \ v -> P.nest i (d v)

text :: String -> PPDoc a
text nm        = \ _ -> P.text nm

ptext :: String -> PPDoc a
ptext nm       = \ _ -> P.ptext nm

char :: Char -> PPDoc a
char c         = \ _ -> P.char c

int :: Int -> PPDoc a
int i          = \ _ -> P.int i

integer :: Integer -> PPDoc a
integer i      = \ _ -> P.integer i

float :: Float -> PPDoc a
float f        = \ _ -> P.float f

double :: Double -> PPDoc a
double d       = \ _ -> P.double d

rational :: Rational -> PPDoc a
rational r     = \ _ -> P.rational r

parens :: PPDoc a -> PPDoc a
parens d       = \ v -> P.parens   (d v)

brackets :: PPDoc a -> PPDoc a
brackets d     = \ v -> P.brackets (d v)

braces :: PPDoc a -> PPDoc a
braces d       = \ v -> P.braces (d v)

quotes :: PPDoc a -> PPDoc a
quotes d       = \ v -> P.quotes (d v)

doubleQuotes :: PPDoc a -> PPDoc a
doubleQuotes d = \ v -> P.doubleQuotes (d v)

semi :: PPDoc a
semi           = \ _ -> P.semi

comma :: PPDoc a
comma          = \ _ -> P.comma

colon :: PPDoc a
colon          = \ _ -> P.colon

space :: PPDoc a
space          = \ _ -> P.space

equals :: PPDoc a
equals         = \ _ -> P.equals

lparen :: PPDoc a
lparen         = \ _ -> P.lparen

rparen :: PPDoc a
rparen         = \ _ -> P.rparen

lbrack :: PPDoc a
lbrack         = \ _ -> P.lbrack

rbrack :: PPDoc a
rbrack         = \ _ -> P.rbrack

lbrace :: PPDoc a
lbrace         = \ _ -> P.lbrace

rbrace :: PPDoc a
rbrace         = \ _ -> P.rbrace

(<>) :: PPDoc a -> PPDoc a -> PPDoc a
(<>) d1 d2     = \ v -> (P.<>) (d1 v) (d2 v)

(<+>) :: PPDoc a -> PPDoc a -> PPDoc a
(<+>) d1 d2    = \ v -> (P.<+>) (d1 v) (d2 v)

hcat :: [PPDoc a] -> PPDoc a
hcat ds        = \ v -> P.hcat (map ($ v) ds)

hsep :: [PPDoc a] -> PPDoc a
hsep ds        = \ v -> P.hsep (map ($ v) ds)

($$) :: PPDoc a -> PPDoc a -> PPDoc a
($$) d1 d2     = \ v -> (P.$$)  (d1 v) (d2 v)

($+$) :: PPDoc a -> PPDoc a -> PPDoc a
($+$) d1 d2    = \ v -> (P.$+$) (d1 v) (d2 v)

vcat :: [PPDoc a] -> PPDoc a
vcat ds        = \ v -> P.vcat (map ($ v) ds)

sep  :: [PPDoc a] -> PPDoc a
sep  ds        = \ v -> P.sep (map ($ v) ds)

cat  :: [PPDoc a] -> PPDoc a
cat  ds        = \ v -> P.cat (map ($ v) ds)

fsep :: [PPDoc a] -> PPDoc a
fsep ds        = \ v -> P.fsep (map ($ v) ds)

fcat :: [PPDoc a] -> PPDoc a
fcat ds        = \ v -> P.fcat (map ($ v) ds)

hang :: PPDoc a -> Int -> PPDoc a -> PPDoc a
hang d1 i d2   = \ v -> P.hang (d1 v) i (d2 v)

render :: PPDoc a -> a -> String
render d       = \ v -> P.render (d v)

punctuate :: PPDoc a -> [PPDoc a] -> [PPDoc a]
punctuate _ []     = []
punctuate p (d:ds) = go d ds
		   where
		     go s [] = [s]
		     go s (e:es) = (s <> p) : go e es

\end{code}

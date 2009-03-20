%
% @(#) $Docid: Mar. 31th 2003  08:33  Sigbjorn Finne $
% @(#) $Contactid: sof@galois.com $
%

\begin{code}
module Utils 
       ( showOct
       , showHex
       , mapFromMb
       , mapMb
       , mapMbM
       , concMaybe
       , toMaybe
       , split
       , splitLast
       , splitLastBy
       , prefix
       , traceIf
       , elemBy
       , mapUnzip
       , diff

       , deEscapeString
       , ( # )

       --,UNUSED: catMapMaybes
       
       , dropSuffix

         -- re-exported
       , trace
       
       , tryOpen
       
       , basename
       , splitdir
       , prefixDir

       , hdirect_root
       , bailIf
       
       , decons
       , safe_init
       , snoc
       
       , mapAccumLM
       
       , notNull		-- :: [a] -> Bool
       
       ) where

import Char (chr, ord, readLitChar)
-- import IOExts
import Debug.Trace
import IO
import Int
{- BEGIN_GHC_ONLY
import Directory
   END_GHC_ONLY -}
import Monad ( when )
import List  ( mapAccumL, isPrefixOf )

infixl 1 #
\end{code}

A convenience operator for invoking methods on objects:

\begin{code}
( # ) :: a -> (a -> b) -> b
obj # meth	= meth obj
\end{code}

Until NumExts is commonly available, we define the following show functions here:

\begin{code}
showIntAtBase :: Integral a => a -> (a -> Char) -> a -> ShowS
showIntAtBase base toChr n r
  | n < 0     = '-':showIntAtBase 10 toChr (negate n) r
  | otherwise = 
    case quotRem n base of { (n', d) ->
    case toChr d        of { ch ->
    let
	r' = ch : r
    in
    if n' == 0 then r' else showIntAtBase base toChr n' r'
    }}

showHex :: Integral a => a -> ShowS
showHex n r = 
 showString "0x" $
 showIntAtBase 16 (toChrHex) n r
 where  
  toChrHex d
    | d < 10    = chr (ord_0   + fromIntegral d)
    | otherwise = chr (ord 'a' + fromIntegral (d - 10))

showOct :: Integral a => a -> ShowS
showOct n r = 
 showString "0o" $
 showIntAtBase 8 (toChrOct) n r
 where toChrOct d = chr (ord_0   + fromIntegral d)

ord_0 :: Num a => a
ord_0 = fromIntegral (ord '0')
\end{code}

Mapping from a Maybe:

\begin{code}
mapFromMb :: b -> (a -> b) -> Maybe a -> b
mapFromMb d f mb = case mb of  Nothing -> d ; Just v  -> f v
\end{code}

\begin{code}
split :: Eq a => a -> [a] -> [[a]]
split _ [] = []
split a as = 
 case break (==a) as of
   (xs,[])   -> [xs]
   (xs,_:ys) -> xs:split a ys

\end{code}

Split at last occurrence of substring.

\begin{code}
splitLast :: Eq a => [a] -> [a] -> ([a],[a])
splitLast []         ls = (ls,[])
splitLast sep@(_:ss) ls = splitLastBy (sep `isPrefixOf`) (drop (length ss)) ls

splitLastBy :: ([a] -> Bool) -- True => current suffix satisifies 
	    -> ([a] -> [a])  -- for the last match, transform the result coming back.
	    -> [a]
	    -> ([a],[a])
splitLastBy predic munge ls = 
   case (chomp (-1) (0::Int) ls) of
     (_,bef,aft) -> (bef,aft)
 where
  chomp lst _ []        = (lst, [], [])
  chomp lst n as@(x:xs) =
       case chomp new_last_pos (n+1) xs of
         (last_found, bef, aft) ->
	    case (compare last_found n) of
	      GT -> (last_found, x:bef,   aft)
	      LT -> (last_found, bef  , x:aft)
	      EQ -> (last_found, bef  ,   munge aft)
   where
    new_last_pos
     | predic as = n
     | otherwise = lst

\end{code}


\begin{code}
prefix :: Eq a => [a] -> [a] -> Maybe [a] -- what's left
prefix [] ls = Just ls
prefix _  [] = Nothing
prefix (x:xs) (y:ys)
 | x == y    = prefix xs ys
 | otherwise = Nothing
\end{code}

\begin{code}
traceIf :: Bool -> String -> a -> a
traceIf True str v = trace str v
traceIf _ _ v = v

elemBy :: (a -> Bool) -> [a] -> Bool
elemBy _       []	=  False
elemBy isEqual (y:ys)	=  isEqual y || elemBy isEqual ys

mapUnzip :: (a -> (b,c)) -> [a] -> ([b],[c])
mapUnzip _ []     = ([],[])
mapUnzip f (x:xs) =
  let
   (a, b)  = f x
   (as,bs) = mapUnzip f xs
  in
  (a:as,b:bs)
\end{code}

Returns list of deltas, i.e,

@
  diff [x0,x1..xp,xn] = [x0, x1-x0, .., xp - xn]
@

\begin{code}
diff :: Num a => [a] -> [a]
diff ls = snd (mapAccumL ( \ acc v -> (v, v - acc)) 0 ls)
\end{code}

begin{code}
catMapMaybes :: (a -> b) -> [Maybe a] -> [b]
catMapMaybes f ls = [f x | Just x <- ls]
end{code}

Dropping the extension off of a filename:

\begin{code}
dropSuffix :: String -> String
dropSuffix str = 
 case dropWhile (\ch -> ch /= '.' && ch /= '/' && ch /= '\\' ) 
                (reverse str) of
      ('.':rs) -> reverse rs
      _        -> str
      -- give up if we reach a separator (/ or \) or end of list.

{- UNUSED:
dropPrefix :: Eq a => [a] -> [a] -> [a]
dropPrefix []         ys = ys
dropPrefix _          [] = []
dropPrefix (x:xs) (y:ys) 
  | x == y               = dropPrefix xs ys
  | otherwise            = y:ys
-}
\end{code}

Slightly generalised version of code found in GreenCard's front end:

\begin{code}
tryOpen ::   Bool 
	 -> [FilePath] 
	 -> [String] 
	 -> FilePath
	 -> IO (Maybe FilePath)
tryOpen verbose path exts name = 
  doUntil (mbOpenFile verbose) (allFileNames path name exts)

doUntil :: (a -> IO (Maybe b)) -> [a] -> IO (Maybe b)
doUntil _     [] = return Nothing
doUntil f (a:as) = do
  v <- f a
  case v of
   Nothing -> doUntil f as
   _       -> return v

allFileNames :: [String] -> String -> [String] -> [String]
allFileNames path file exts 
  = [addSuffix '/' d ++ file ++ (prefixWith '.' ext) | d <- path, ext <- exts]
    where
     addSuffix _  []  = []
     addSuffix ch ls  = 
        case (decons ls) of
	  (_,x)
	    | x == ch   -> ls
	    | otherwise -> ls++[ch]

     prefixWith _  [] = []
     prefixWith ch ls@(x:_)
       | ch == x   = ls
       | otherwise = ch:ls
\end{code}

Combining <tt>last</tt> and <tt>init</tt> into one (pass
over the list):

\begin{code}
decons :: [a] -> ([a],a)
decons ds = trundle ds
 where
  trundle []     = error "decons: empty list"
  trundle [x]    = ([], x)
  trundle (x:xs) = let (ls, l) = trundle xs in (x:ls, l)
\end{code}

Try reading a file:

\begin{code}

mbOpenFile :: Bool -> FilePath -> IO (Maybe FilePath)
mbOpenFile verbose fpath = do
   -- I seem to remember that Hugs doesn't support Directory...
{- BEGIN_GHC_ONLY
  flg <- doesFileExist fpath
  END_GHC_ONLY -}
{- BEGIN_NOT_FOR_GHC -}
  flg <- (openFile fpath ReadMode >>= \ h -> hClose h >> return True)
            `catch` (\ _ -> return False)
{- END_NOT_FOR_GHC -}
  if not flg 
   then return Nothing
   else do
     when verbose (hPutStrLn stderr ("Reading file: " ++ show fpath))
     return (Just fpath)

\end{code}

\begin{code}
basename :: String -> String
basename str = snd $
    splitLastBy (\ (x:_) -> x == '/' || x == '\\')
    		id
		str
     -- bi-lingual, the upshot of which is that
     -- / isn't allowed in DOS-style paths (and vice
     -- versa \ isn't allowed in POSIX(?) style pathnames).

splitdir :: String -> (String, String)
splitdir = 
  splitLastBy (\ (x:_) -> x == '/' || x == '\\')
              id

prefixDir :: String -> String -> String
prefixDir []    rest = rest
prefixDir ['/'] rest = '/':rest
prefixDir ['\\'] rest = '/':rest
prefixDir [x]    rest = x:'/':rest
prefixDir (x:xs) rest = x : prefixDir xs rest

\end{code}

Removing escape char from double quotes:

\begin{code}
deEscapeString :: String -> String
deEscapeString [] = []
deEscapeString ls@('\\':x:xs) = 
  case x of
    '"' -> x : deEscapeString xs -- "
    _   -> 
	case readLitChar ls of
	  ((ch,rs):_) -> ch : deEscapeString rs
	  _ -> '\\':x: deEscapeString xs
deEscapeString (x:xs) = x: deEscapeString xs
\end{code}

The top of the HaskellDirect Registry tree:

\begin{code}
hdirect_root :: String
hdirect_root        = "Software\\Haskell\\HaskellDirect"

-- sporadically handy in a monadic context.
bailIf :: Bool -> a -> a -> a
bailIf True a _ = a
bailIf _    _ b = b
\end{code}

Avoids Haskell version trouble:

\begin{code}
mapMb :: (a -> b) -> Maybe a -> Maybe b
mapMb _ Nothing  = Nothing
mapMb f (Just c) = Just (f c)

mapMbM :: (Monad m) => (a -> m b) -> Maybe a -> m (Maybe b)
mapMbM _ Nothing  = return Nothing
mapMbM f (Just c) = f c >>= return.Just

concMaybe :: Maybe a -> Maybe a -> Maybe a
concMaybe v@(Just _) _ = v
concMaybe _          v = v

-- If predicate is false, represent it as Nothing.
toMaybe :: (a -> Bool) -> a -> Maybe a
toMaybe predic x | predic x  = Nothing
	         | otherwise = Just x
\end{code}

\begin{code}
safe_init :: [a] -> [a]
safe_init [] = []
safe_init ls = init ls
\end{code}

\begin{code}
snoc :: [a] -> a -> [a]
snoc []     y = [y]
snoc (x:xs) y = x : snoc xs y
\end{code}

\begin{code}
mapAccumLM :: (Monad m)
           => (acc -> x -> m (acc, y)) -- Function of elt of input list
				     -- and accumulator, returning new
				     -- accumulator and elt of result list
   	   -> acc	    -- Initial accumulator 
	   -> [x]	    -- Input list
	   -> m (acc, [y])	    -- Final accumulator and result list
mapAccumLM _ s []     	=  return (s, [])
mapAccumLM f s (x:xs) 	=  do
 (s', y)     <- f s x
 (s'',ys)    <- mapAccumLM f s' xs
 return (s'',y:ys)
 
\end{code}

The simplest of defns; usefule, but not provided as standard:

\begin{code}
notNull :: [a] -> Bool
notNull [] = False
notNull  _ = True
\end{code}

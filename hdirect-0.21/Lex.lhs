%
% (c) 1998-99, sof
%

A lexer for MS & OMG IDLs

\begin{code}
module Lex 
        ( 
 	  lexIDL    -- :: (IDLToken -> LexM a) -> LexM a
	) where

import LexM
import Char
import Numeric
import IDLToken
import List ( isPrefixOf )
import Literal
import BasicTypes
import Utils ( deEscapeString, notNull )
import SrcLoc
import Opts ( optIncludeAsImport, optExcludeSysIncludes )

\end{code}

\begin{code}
lexIDL :: (IDLToken -> LexM a) -> LexM a
lexIDL cont = do
 eof <- isEOF
 if eof
   then cont T_eof
   else do
    c <- getNextChar
    case c of
     ' '  -> lexIDL cont
     '\t' -> lexIDL cont
     '\v' -> lexIDL cont
     '\b' -> lexIDL cont
     '\r' -> lexIDL cont
     '\f' -> lexIDL cont
     '\n' -> incLineNo (lexIDL cont)
     '/'  -> do
       c1 <- getNextChar
       case c1 of
         '/' -> lex_oneline_comment cont
         '*' -> lex_nested_comment cont (1::Int){-one seen-}
	 _ -> do
	   putBackChar c1
	   cont T_div
     '(' -> cont T_oparen
     ')' -> cont T_cparen
     '{' -> cont T_ocurly
     '}' -> cont T_ccurly
     '[' -> cont T_osquare
     ']' -> cont T_csquare
     ',' -> cont T_comma
     '.' -> do
       c1 <- getNextChar
       case c1 of
        '.' -> do
	  c2 <- getNextChar
	  case c2 of
	   '.' -> cont T_dotdotdot
	   _   -> do
             putBackChar c1
             putBackChar c2
	     cont T_dot
	_ -> do
	  putBackChar c1
	  cont T_dot
     ';' -> cont T_semi
     ':' -> do
       c1 <- getNextChar
       case c1 of
          ':' -> cont T_dcolon
	  _   -> putBackChar c1 >> cont T_colon
     'L'  -> do
        c1 <- getNextChar
	case c1 of
	  '\"' -> lex_string (\ (T_string_lit ls) -> cont (T_literal (WStringLit ls)))
	  _    -> putBackChar c1 >> start_lex_id 'L' cont

     '\"' -> lex_string cont -- matching "
     '\'' -> lex_char cont
     '='  -> do
	  c1 <- getNextChar
          case c1 of
	    '=' -> cont T_eqeq
	    _   -> putBackChar c1 >> cont T_equal
     '!'  -> do
	  c1 <- getNextChar
          case c1 of
	    '=' -> cont T_neq
	    _   -> putBackChar c1 >> cont T_negate
     '+'  -> cont T_plus
     '-'  -> cont T_minus
     '?'  -> cont T_question
     '<'  -> do
	  c1 <- getNextChar
          case c1 of
            '<' -> cont (T_shift L)
	    '=' -> cont T_le
	    _   -> putBackChar c1 >> cont T_lt
     '>'  -> do 
	  c1 <- getNextChar
          case c1 of
	    '>' -> cont (T_shift R)
	    '=' -> cont T_ge
            _   -> putBackChar c1 >> cont T_gt
     '|'  -> do
	  c1 <- getNextChar
          case c1 of
	    '|' -> cont T_rel_or
            _   -> putBackChar c1 >> cont T_or
     '^'  -> cont T_xor
     '&'  -> do
	  c1 <- getNextChar
          case c1 of
	    '&' -> cont T_rel_and
            _   -> putBackChar c1 >> cont T_and
     '*'  -> cont T_times
     '%'  -> cont T_mod
     '~'  -> cont T_not
     '#'  -> do 
       -- I'm delaying the decision on whether #pragma lines should 
       -- be parsed or not. For the moment, we'll just chop off everything
       -- after #pragma.
       cs1 <- getStream
       let cs2 = dropWhile isSpace cs1
       if "define" `isPrefixOf` cs2 then
          setStream (drop (6::Int) cs2) >> cont T_hdefine
        else if "pragma" `isPrefixOf` cs2 then
	  case span (/='\n') (drop (6::Int){-length of "pragma"-} cs2) of
	    (prag,[]) -> do
	       setStream []
	       cont (T_pragma prag)
	    (prag,_:cs3) -> do
	       setStream cs3
	       cont (T_pragma prag)
        else if ("line" `isPrefixOf` cs2) || (notNull cs1 && isSpace (head cs1)) then
	  let
	    cs1_no_line = dropWhile (not.isDigit) cs1
	  in
	    -- munge, munge - get at the line and loc.
	  case (reads cs1_no_line) of 
	    ((ln,rs):_) -> 
	       case (reads (dropWhile isSpace rs)) of
	        ((fn,rs1):_) -> do
		     -- drop any trailing info (such as flags)..
		    let (flags, rs2) = break (=='\n') rs1
		    setStream rs2
		    sl  <- getSrcLoc 
		    osl <- getOrigSrcLoc
		    let f   = modSrcLoc sl
		        ofn = modSrcLoc osl
		        in_system   = '3' `elem` flags
			at_end      = '2' `elem` flags

		    flg <- getSystemContextFlag
                    inSystemContext in_system  $ do
		    setSrcLoc (mkSrcLoc fn ln) $
		      if (optIncludeAsImport && at_end) then
		         cont T_include_end
		      else if (optExcludeSysIncludes && at_end && (flg || ofn == fn)) then
			  -- emit end marker if we're leaving a system context
			  -- (or name of source module.)
		         cont T_include_end
		      else if (optExcludeSysIncludes && not at_end && (in_system || ofn == fn) && f /= fn) then
		          -- emit start marker if we're entering a system context
			  -- (or name of source module.)
		         cont (T_include_start fn)
		      else if (not optIncludeAsImport || optExcludeSysIncludes || f == fn) then
		         lexIDL cont -- nothing new.
		      else 
		         cont (T_include_start fn)
	        _ -> do
	            sloc <- getSrcLoc
	            cont (T_unknown (sloc,c:cs1))
	    _ -> do
	       sloc <- getSrcLoc
	       cont (T_unknown (sloc,c:cs1))
         else do
	  sloc <- getSrcLoc
	  cont (T_unknown (sloc,c:cs1))
     x    -> start_lex_id x cont

start_lex_id :: Char -> (IDLToken -> LexM a) -> LexM a
start_lex_id c cont = do
       putBackChar c
       if isDigit c 
          then lex_num cont
          else if isHexDigit c
	       then lex_guid' cont
	       else lex_id cont

{-
spool_on :: String -> [Char] -> [Char]
spool_on s []     = []
spool_on s ('\n':'#':xs) = 
   case dropWhile isSpace (dropWhile isDigit (dropWhile isSpace xs)) of
     ('"':xs1) | s `isPrefixOf` xs1 -> dropWhile (/= '\n') xs1 -- matching '"'
     xs            -> spool_on s xs
spool_on s (x:xs) = spool_on s xs 
-}

lex_oneline_comment :: (IDLToken -> LexM a) -> LexM a
lex_oneline_comment cont = do
  cs <- getStream
  case dropWhile (/='\n') cs of
    []     -> setStream [] >> incLineNo (lexIDL cont)
    (_:xs) -> setStream xs >> incLineNo (lexIDL cont)

lex_nested_comment :: (IDLToken -> LexM a) -> Int -> LexM a
lex_nested_comment cont count = do
  cs <- getStream
  case dropWhile (\ x -> x /='/' && x /= '*' && x /= '\n') cs of
    []           -> do
       setStream []
       lexIDL cont
    ('\n':cs1)   -> do
       setStream cs1
       incLineNo (lex_nested_comment cont count)
    ('/':'*':cs1) -> do
       setStream cs1
       lex_nested_comment cont (count+1)
    ('*':'/':cs1) -> do
       setStream cs1
       if count == 1 
	then lexIDL cont
	else lex_nested_comment cont (count-1)
    (_:cs1)     -> do
       setStream cs1
       lex_nested_comment cont count

lex_num :: (IDLToken -> LexM a) -> LexM a
lex_num cont = do
  cs <- getStream
  case cs of
    '0':'x':cs1 -> 
     case readHex cs1 of
       [(i,cs2)] -> setStream (removeL cs2) >> cont (T_literal (IntegerLit (ILit 16 i)))
       _         -> getSrcLoc >>= \ sc -> cont (T_unknown (sc,cs))
    '0':cs1 ->
      case readOct cs1 of
       [(i,cs2)] -> 
           case cs2 of
  	     '-':cs3              -> try_lex_guid cs3
             c:cs3 | isHexDigit c -> 
                   case dropWhile (isHexDigit) cs3 of
		     '-':cs4 -> try_lex_guid cs4
		     _ -> do
		        -- or should that be an error ?
		       setStream cs2
		       cont (T_literal (IntegerLit (ILit 8 i)))
             _ -> do
	       setStream (removeL cs2)
	       cont (T_literal (IntegerLit (ILit 8 i)))
            where
              -- this may just be the start of a GUID.
	      try_lex_guid cs3 = 
               case lex_guid (takeWhile (isHexDigit) cs) cs3 of
                 Nothing         -> do
		    setStream (removeL cs2)
		    cont (T_literal (IntegerLit (ILit 8 i)))
	         Just (guid,cs4) -> do
		    setStream cs4
		    cont (T_literal (GuidLit guid))
       _ ->
        -- this is a mess.
	let (as, bs) = span (isHexDigit) cs in
	case bs of
	  '-':bs1 -> 
	    case lex_guid as bs1 of
	      Nothing -> do
 	             sloc <- getSrcLoc
	             cont (T_unknown (sloc, cs))
	      Just (guid,cs2) -> do
		     setStream cs2
		     cont (T_literal (GuidLit guid))
	  _ ->
	    case reads cs of
              [(i,cs2)] -> do
	        setStream (removeL cs2)
	        cont (T_literal (IntegerLit (ILit 10 i)))
              _ ->
                case reads cs of
                  [(d,cs2)] -> do
	            setStream cs2 
		    let rs = takeWhile (\ x -> isDigit x || x == '.') cs
		    cont (T_literal (FloatingLit (rs,d)))
                  _ -> do
                    sloc <- getSrcLoc
                    cont (T_unknown (sloc, cs))

    _ ->	     	      
        -- this is a mess.
	let (as, bs) = span (isHexDigit) cs in
	case bs of
	  '-':bs1 -> 
	    case lex_guid as bs1 of
	      Nothing -> do
 	             sloc <- getSrcLoc
	             cont (T_unknown (sloc, cs))
	      Just (guid,cs2) -> do
		     setStream cs2
		     cont (T_literal (GuidLit guid))
          _ ->
            case reads cs of
             [(i,cs1)] -> do
                   setStream (removeL cs1)
		   cont (T_literal (IntegerLit (ILit 10 i)))
             _ ->
               case reads cs of
                 [(d,cs1)] -> do
		    setStream cs1
    		    let rs = takeWhile (\ x -> isDigit x || x == '.') cs
		    cont (T_literal (FloatingLit (rs,d)))
                 _ -> getSrcLoc >>= \ sc -> cont (T_unknown (sc,cs))

 where
  removeL ('L':xs) = xs
  removeL xs       = xs

lex_guid' :: (IDLToken -> LexM a) -> LexM a
lex_guid' cont = do
  cs <- getStream
  case readHex cs of
     [((_::Int),cs1)] -> 
       case cs1 of
        ('-':cs2) -> -- this may just be the start of a GUID.
            case lex_guid (takeWhile (isHexDigit) cs) cs2 of
              Nothing         -> lex_id cont
	      Just (guid,cs3) -> do
	         setStream cs3
		 cont (T_literal (GuidLit guid))
        _ -> lex_id cont
     _ -> lex_id cont

lex_guid :: String -> String -> Maybe ([String],String)
lex_guid d1 cs1 =
 case span (isHexDigit) cs1 of 
  (d2,'-':cs2) ->
   case span (isHexDigit) cs2 of 
    (d3,'-':cs3) ->
     case span (isHexDigit) cs3 of 
      (d4,'-':cs4) ->
       case span (isHexDigit) cs4 of 
        ([],_)   -> Nothing
        (d5,cs5) -> Just ([d1,d2,d3,d4,d5],cs5)
      _            -> Nothing
    _            -> Nothing
  _            -> Nothing

lex_id :: (IDLToken -> LexM a) -> LexM a
lex_id cont = do
 cs <- getStream
 case span is_id_char cs of
   ([],(r:rs)) -> do
      setStream rs
      sloc <- getSrcLoc
      cont (T_unknown (sloc, [r]))
   (is,rs) -> do
     t <- lookupSymbol is
     case t of
       Just tok@T_safearray ->   -- SIGH.
           case rs of
	      ('(':rs2) -> setTok tok >> setStream rs2 >> cont tok
	      _         -> do
	          setTok tok
		  setStream rs
	          res <- lookupType is  -- check to see whether SAFEARRAY
					-- is to be considered an ID or a TYPE..
		  case res of
		    Nothing  -> cont (T_id "SAFEARRAY")
		    Just x   -> cont x

       Just (T_include _) -> do
	    let 
		-- Sigh, trying to integrate CPP's #include syntax
		-- into IDLs will lead to trouble (what's the parse of
		-- "<a>"?) Not unfixable, but let's delay doing so until
		-- we move to using a lexer generator.
	        rs'     = dropWhile isSpace rs

	        ((as,bs), wrapper) = 
		    case rs' of 
		      '(':xs -> -- ("foo.h") or (foo.h)
		          (break (==')') xs, id)
		      '<':xs -> -- <foo.h>
		          (break (=='>') xs , \ x -> '<':x ++ ">") 
		      '"':xs ->  -- "foo.h" Don't even think of using double qoutes
		                 -- in your filenames!
			  (break (=='"') xs, \ x -> '"':x ++ "\"")
		      _      -> (break isSpace rs', id)

		bs' =
		  case bs of
		    []     -> []
		    (_:xs) -> xs

		tok     = T_include (wrapper as)
	    setTok tok
	    setStream bs'
	    cont tok

       Just T_ignore_start -> do
           ls <- getStream
           case (dropUntil "__ignore_end__" ls) of
	      [] -> cont T_eof
	      xs -> setStream xs >> lexIDL cont
       Just tok -> setTok tok >> setStream rs >> cont tok
       Nothing  -> setTok (T_id is) >> setStream rs >> cont (T_id is)

is_id_char :: Char -> Bool
is_id_char ch = isAlpha ch || isDigit ch || ch == '_' || ch == '$' || ch == '.'

dropUntil :: String -> String -> String
dropUntil _pref [] = []
dropUntil pref  ls@(_:xs)
 | pref `isPrefixOf` ls = drop (length pref) ls
 | otherwise = dropUntil pref xs

lex_string :: (IDLToken -> LexM a) -> LexM a
lex_string cont = do
 cs <- getStream
 --assert (head cs /= '\"')
 case loop cs of 
  (str,cs1) -> do
     setStream cs1
     cont (T_string_lit (deEscapeString str))
  where
   not_quote_nor_esc ch = ch /= '\"' && ch /= '\\'

   loop cs =
    case span not_quote_nor_esc cs of
     (ls,'\\':rs) -> 
      case rs of
       ('"':rs1) -> -- escaped quote, just continue.
	  let
	   (as,bs) = loop rs1
	  in
	  (ls++'\\':'\"':as, bs)
       ('\\':rs1) -> -- want \\" to be interpreted as \\ ", not \ \"
	  let
	   (as,bs) = loop rs1
	  in
	  (ls++'\\':'\\':as, bs)
       _ -> -- just continue.
	  let
	   (as,bs) = loop rs
	  in
	  (ls++'\\':as, bs)
     (ls,'\"':rs) -> (ls,rs)
     x -> x
   	  
lex_char :: (IDLToken -> LexM a) -> LexM a
lex_char cont = do
 cs <- getStream
 loop cs
  where
   not_quote_nor_esc ch = ch /= '\'' && ch /= '\\'
   loop cs = 
    case span not_quote_nor_esc cs of
     ([],'\\':rs) ->
        case rs of
          '\\':'\'':rs1 -> setStream rs1  >>          cont (T_literal (CharLit '\\'))
          x:'\'':rs1 -> setStream rs1     >>          cont (T_literal (CharLit (read ['\\',x])))
          _          -> getSrcLoc         >>= \ sl -> cont (T_unknown (sl,cs))
     (ls,'\'':rs) -> setStream rs         >>          cont (T_literal (CharLit (read ls)))
     (ls,rs) -> setStream rs >> getSrcLoc >>= \ sl -> cont (T_unknown (sl,ls))
\end{code}




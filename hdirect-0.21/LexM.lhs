%
% (c) The GRASP/AQUA Project, Glasgow University, 1998
%

The @LexM@ hides the information maintained by the IDL lexer
(and parser.)


\begin{code}
module LexM 

       (
         LexM
	
       , runLexM         -- :: [FilePath] -> String -> LexM a -> IO (a, SymbolTable IDLToken)
       , invokeLexM      -- :: String -> String -> LexM a -> LexM a
       , ioToLexM        -- :: IO a   -> LexM a
       , incLineNo       -- :: LexM a -> LexM a
       , setSrcLoc       -- :: SrcLoc -> LexM a -> LexM a
       , getSrcLoc       -- :: LexM SrcLoc
       , getOrigSrcLoc   -- :: LexM SrcLoc
       , getPath         -- :: LexM [FilePath]
       , isEOF           -- :: LexM Bool
       , getNextChar     -- :: LexM Char
       , putBackChar     -- :: Char -> LexM ()
       , getStream       -- :: LexM String
       , setStream       -- :: String -> LexM ()
       , lookupSymbol    -- :: String -> LexM (Maybe IDLToken)
       , lookupType      -- :: String -> LexM (Maybe IDLToken)
       , addBuiltinType  -- :: String -> LexM ()
       , addTypedef      -- :: String -> LexM ()
       , setTok
       , getTok
       
       , inSystemContext
       , getSystemContextFlag

       , cacheFilePath
       , alreadySeenFile
       

       , thenLexM
       , returnLexM
       ) where

import qualified SymbolTable
import SrcLoc
import IDLToken
import IDLSyn
import PreProc
import Data.IORef  ( IORef, newIORef, readIORef, writeIORef )
import Utils   ( tryOpen, dropSuffix )
import IO      ( hPutStrLn, stderr )
import Monad   ( when )
import Char    ( toLower )

-- components threaded by the monad (apart from
-- the IO token.)
data LexState
 = LexState {
      sym_table  :: SymbolTable.SymbolTable IDLToken,
      cur_tok    :: Maybe IDLToken, {- current token (for error msgs.) -}
      inp_stream :: String     {- input stream -}
  }

data LexEnv = 
  LexEnv {
    env_src_loc     :: SrcLoc,
    env_origsrc_loc :: SrcLoc,
    env_in_system   :: Bool,
    env_file_path   :: [FilePath],      -- search path for imported .idl files.
    env_file_cache  :: IORef [FilePath] -- already imported .idl files.
  }

newtype LexM a = LexM (  LexEnv -> LexState -> IO (a, LexState))

runLexM :: [String]
        -> String
        -> String 
        -> LexM a 
	-> IO a
runLexM path fname str (LexM m) = do
  var <- newIORef []
  let sl = (mkSrcLoc fname 1)
  (v, _) <- m (LexEnv sl sl False path var) 
              (LexState (SymbolTable.mkSymbolTable idlKeywords) Nothing str)
  return v

-- nested invocations of LexM actions share
-- the keyword part of the symbol table + the cache
-- of already seen files.
invokeLexM :: String -> String -> LexM a -> LexM a
invokeLexM fname ls (LexM m) =
 LexM (\ (LexEnv _ _ flg path var) (LexState symt tok cs) -> do
    let -- symt1 = SymbolTable.newContext symt
        sl    = (mkSrcLoc fname 1)
    (v, LexState symt2 _ _) 
      <- m (LexEnv sl sl flg path var)
	   (LexState symt tok ls)
    return (v, LexState symt2{-(SymbolTable.combineSyms symt symt2)-} tok cs))

ioToLexM :: IO a -> LexM a
ioToLexM act =
 LexM (\ _ st -> do
         v <- act
	 return (v, st))

cacheFilePath :: FilePath -> LexM ()
cacheFilePath f =
 LexM (\ (LexEnv{env_file_cache=fp}) st -> do
     ls <- readIORef fp
     writeIORef fp (f:ls)
     return ((), st))

alreadySeenFile :: FilePath -> LexM Bool
alreadySeenFile f =
 LexM (\ (LexEnv{env_file_cache=fp}) st -> do
     ls <- readIORef fp
     return (f `elem` ls, st))

incLineNo :: LexM a -> LexM a
incLineNo (LexM m) = 
 LexM (\ env@(LexEnv{env_src_loc=l}) st -> m (env{env_src_loc=incSrcLineNo l}) st)

setSrcLoc :: SrcLoc -> LexM a -> LexM a
setSrcLoc new_loc (LexM m) = LexM (\ env st -> m (env{env_src_loc=new_loc}) st)

inSystemContext :: Bool -> LexM a -> LexM a
inSystemContext flg (LexM m) = LexM (\ env st -> m (env{env_in_system=flg}) st)

getSystemContextFlag :: LexM Bool
getSystemContextFlag = LexM (\ env st -> return (env_in_system env, st))

getSrcLoc :: LexM SrcLoc
getSrcLoc = LexM (\ env st -> return (env_src_loc env, st))

getOrigSrcLoc :: LexM SrcLoc
getOrigSrcLoc = LexM (\ env st -> return (env_origsrc_loc env, st))

getPath :: LexM [FilePath]
getPath = LexM (\ env st -> return (env_file_path env, st))

isEOF :: LexM Bool
isEOF = LexM (\ _ st -> return (null (inp_stream st), st))

getNextChar :: LexM Char
getNextChar = 
  LexM (\ _ st ->
     case inp_stream st of
       (c:cs) -> return (c, st{inp_stream=cs})
       _      -> return (error "getNextChar: stream is empty", st))

putBackChar :: Char -> LexM ()
putBackChar c = LexM ( \ _ st -> return ((), st{inp_stream=c:inp_stream st}))

getStream :: LexM String
getStream = LexM (\ _ st -> return (inp_stream st, st))

setStream :: String -> LexM ()
setStream cs = LexM (\ _ st -> return ((), st{inp_stream=cs}))

lookupSymbol :: String -> LexM (Maybe IDLToken)
lookupSymbol str = 
  LexM (\ _ st -> return (SymbolTable.lookupSymbol (sym_table st) str, st))

lookupType :: String -> LexM (Maybe IDLToken)
lookupType str = 
  LexM (\ _ st -> return (SymbolTable.lookupType (sym_table st) str, st))

-- back door entry for adding new types after we've installed
-- the default set.
addBuiltinType :: String -> LexM ()
addBuiltinType str =
 LexM (\ _ st ->
     return ( ()
            , st{sym_table=SymbolTable.addKeyword (sym_table st) str (T_type str)}))

addTypedef :: String -> LexM ()
addTypedef str =
 LexM (\ _ st ->
       return (()
              , st{sym_table=SymbolTable.addType (sym_table st) str (T_type str)}))

setTok :: IDLToken -> LexM ()
setTok t = LexM (\ _ st -> return ((),st{cur_tok=Just t}))

getTok :: LexM (Maybe IDLToken)
getTok = LexM (\ _ st -> return (cur_tok st, st))

-----

thenLexM :: LexM a -> (a -> LexM b) -> LexM b
thenLexM (LexM m) n =
 LexM ( \ env st -> do
          (a, st1) <- m env st
	  let (LexM act) = n a
	  act env st1 )

returnLexM :: a -> LexM a
returnLexM v = LexM (\ _ st -> return (v, st) )

{- UNUSED
mapLexM :: (a -> b) -> LexM a -> LexM b
mapLexM f (LexM m) =
 LexM (\ env st -> do
   (x,st1) <- m env st
   return (f x, st1))
-}

instance Monad LexM where
  (>>=)  = thenLexM
  return = returnLexM

\end{code}




-- A program to automatically build a Cabal package consisting of
-- compiled DOM IDL definitions in JSMW flavor.

-- The program creates a Cabal package in the current directory (which has to be empty).

module Main where

import Paths_domconv
import Prelude hiding (putStrLn, readFile)
import System.Environment.UTF8
import System.Process
import System.Directory
import System.FilePath
import System.Exit
import System.Cmd
import System.IO (stdin, stderr, openFile, IOMode (..), hClose)
import System.IO.UTF8
import Control.Monad
import Data.Maybe
import Data.Either
import Data.List
import Data.Char

main :: IO ()

main = do
  p <- getProgName
  putStrLn $ "This is the " ++ p ++ " program which will build a Cabal package"
  putStrLn $ "out of the DOM IDL files provided by the Web Consortium."
  putStrLn $ "The package will be created in the current directory which has to be empty."
  d <- getDataDir
  let idldir = d </> "idl"
      pkgdir = d </> "pkg"
  putStrLn $ "Looking for IDL files in " ++ idldir
  idlfiles <- getDirectoryContents idldir >>= return . filter (\f -> takeExtension f == ".idl")
  when (null idlfiles) $ do
    putStrLn $ "No IDL files found in " ++ idldir
    exitWith (ExitFailure 1)
  putStrLn "The following files found:"
  mapM_ (putStrLn . show) idlfiles
  curr <- getCurrentDirectory
  putStrLn $ "The current directory is " ++ curr
  curfs_all <- getDirectoryContents curr
  let curfs = curfs_all \\ [".", ".."]
  when (not $ null curfs) $ do
    putStrLn $ "Current directory is not empty. Files found:"
    mapM_ (putStrLn . show) curfs
    putStrLn $ "The DOM Cabal package cannot be created: please clean up the directory " ++ curr
    exitWith (ExitFailure 2)
  putStrLn "Copying extra files"
  exfs_all <- getDirectoryContents pkgdir
  let exfs = exfs_all \\ [".", ".."]
  mapM_ (putStrLn . show) exfs
  mapM_ (\f -> copyFile (pkgdir </> f) (curr </> f)) exfs
  putStrLn "Converting the IDL files."
  mapM_ (\f -> do
      let idl = idldir </> f
          base = takeFileName f `replaceExtension` ".hs_unsplit"
          cmd = "domconv-jsmw -I" ++ idldir ++ 
                  " <" ++ idl ++ 
                  " >" ++ base ++ 
                  " && modsplit " ++ base
      putStrLn $ "Running command: " ++ cmd
      ex <- system cmd
      putStrLn (show ex)
    ) (idlfiles \\ ["addtags.idl"])
  let splcmd = "modsplit wbt.hs_unsplit"
  putStrLn $ "Running command:" ++ splcmd
  exs <- system splcmd
  putStrLn (show exs)
  let grepcmd = "grep -- '^-- Split begin/' *.hs_unsplit"
  putStrLn $ "Running command: " ++ grepcmd
  (inp, out, err, pid) <- runInteractiveCommand grepcmd
  mod_raw <- hGetContents out
  waitForProcess pid
  putStrLn $ "Reading package description file"
  let indent s ls = concat (intersperse (s ++ "\n") (map ("   " ++) ls))
  descr <- readFile "descr.txt"
  putStrLn $ "Reading package version file"
  let indent s ls = concat (intersperse (s ++ "\n") (map ("   " ++) ls))
  ver <- readFile "version.txt"
  let modlist = map (drop 1 . snd . break (== '/')) (lines mod_raw)
  putStrLn "Writing Cabal package description file."
  let cabfile = "DOM.cabal"
  cfd <- openFile cabfile WriteMode
  hPutStrLn cfd $ "-- " ++ cabfile ++ " is generated automatically: do not edit"
  hPutStrLn cfd $ "Name: DOM"
  hPutStrLn cfd $ "Build-Type: Simple"
  hPutStrLn cfd $ "Version: " ++ head (lines ver)
  hPutStrLn cfd $ "Synopsis: " ++ head (lines descr)
  hPutStrLn cfd $ "Description:\n" ++ indent "" (tail $ lines descr)
  hPutStrLn cfd $ "License: BSD3"
  hPutStrLn cfd $ "License-File: LICENSE"
  hPutStrLn cfd $ "Category: Web"
  hPutStrLn cfd $ "Extensions: CPP"
  hPutStrLn cfd $ "Maintainer: Dmitry Golubovsky <golubovsky@gmail.com>"
  hPutStrLn cfd $ "Build-depends: base >= 3, mtl >= 1.1.0.0, WebBits == 0.15"
  hPutStrLn cfd $ "Exposed-modules:\n" ++ indent "," modlist
  hClose cfd
  putStrLn "Package created successfully"
  exitWith (ExitSuccess)  


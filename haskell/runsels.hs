{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE LambdaCase #-}
module Main (main, getProgName, absPath) where

import Foreign
import Foreign.C

import System.Environment (getArgs)
import System.FilePath
import System.Directory   (getCurrentDirectory)

import Config
import Apps
import Selections (loadSelections)
import Run     (executeSelections)

main :: IO ()
main = getArgs >>= headTail
  (error "Syntax: runsels [APP | SELECTIONS] [ARGS]")
  \(appOrSels:args) -> do
    config <- getProgName >>= absPath
	        >>= Config.getDefaultConfig
	        .:. dropFileName
    sels <- config >>= lookupApp appOrSels
	      >>= maybe appOrSels (</> "selections.xml")
	      >>= loadSelections
    executeSelections sels args config

infixl 2 (.:.)
(.:.) = flip (.)

-- bash-3.2> ghci
-- Prelude>  :type maybe
-- maybe :: b -> ( a -> b ) -> Maybe a  -> b
headTail :: b -> ([a] -> b) -> a -> [a] -> b
headTail bkp fun list = \case [    ] -> bkp
			                        [x   ] -> bkp
			                        [x:xs] -> fun x xs

-- From http://hackage.haskell.org/trac/ghc/ticket/3199

getProgName :: IO String
getProgName = alloca $ \argc
	         -> alloca $ \argv
           -> getArgv argc argv
           >> peekCString =<< peek =<< peek argv

foreign import ccall unsafe "getFullProgArgv"
  getArgv :: Ptr CInt -> Ptr (Ptr CString) -> IO ()
 
-- From http://hackage.haskell.org/packages/archive/MissingH/1.2.0.0/doc/html/System-Path-NameManip.html

absPath :: String -> IO String
absPath path@('/':_) = return path
absPath path = do
  cwd <- getCurrentDirectory
  return $ cwd++"/"++path

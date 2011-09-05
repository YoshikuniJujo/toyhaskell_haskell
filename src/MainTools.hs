{-# LANGUAGE TupleSections #-}

module MainTools ( mainGen ) where

import ToyHaskell

import System.IO ( hFlush, stdout )
import System.Console.GetOpt (
	getOpt, ArgOrder( .. ), OptDescr( .. ), ArgDescr( .. ) )
import Control.Monad ( foldM )
import Control.Monad.Tools ( doWhile )
import Control.Arrow ( second )
import Data.List ( isPrefixOf )
import Data.Char ( isSpace )

--------------------------------------------------------------------------------

mainGen :: [ String ] -> [ String ] -> IO String
mainGen args _ = do
	let ( expr, fns, errs ) = readOption args
	mapM_ putStr errs
	env0 <- foldM ( \e -> fmap ( load e ) . readFile ) initEnv fns
	( flip . flip maybe )
		( showValue . evalV env0 ) expr $ do
		runLoop "toyhaskell" env0 $ \env inp -> case inp of
			':' : cmd	-> runCmd cmd env
			_		-> case evalV env inp of
				Let ps	-> return $ setPats
					( second ( toyEval env ) `map` ps ) env
				ret	-> showValue ret >>= putStr >> return env
		return ""

runLoop :: String -> a -> ( a -> String -> IO a ) -> IO ()
runLoop name stat0 act = do
	_ <- doWhile stat0 $ \stat -> do
		input <- prompt $ name ++ "> "
		if input `elem` [ ":quit", ":q" ]
			then return ( stat, False )
			else fmap ( , True ) $ act stat input
	putStrLn $ "Leaving " ++ name ++ "."
	where
	prompt p = putStr p >> hFlush stdout >> getLine

data Option = Expr String

options :: [ OptDescr Option ]
options = [
	Option "e" [ ] ( ReqArg Expr "haskell expression" ) "run expression"
 ]

readOption ::
	[ String ] -> ( Maybe String, [ FilePath ], [ String ] )
readOption args = let
	( opts, fns, errs )	= getOpt RequireOrder options args
	expr			= fromOps opts in
	( expr, fns, errs )
	where
	fromOps [ ]		= Nothing
	fromOps ( op : _ )	= case op of
		Expr e		-> Just e

runCmd :: String -> Env -> IO Env
runCmd cmd env
	| "load " `isPrefixOf` cmd	= do
		let fn = dropWhile isSpace $ dropWhile ( not . isSpace ) cmd
		load env `fmap` readFile fn
	| otherwise			= do
		putStrLn $ "unknown command ':" ++ cmd  ++ "'"
		return env

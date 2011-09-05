{-# LANGUAGE TupleSections #-}

module MainTools ( mainGen ) where

import ToyHaskell ( Env, initEnv, load, evalP )

import System.IO ( hFlush, stdout )
import System.Console.GetOpt (
	getOpt, ArgOrder( .. ), OptDescr( .. ), ArgDescr( .. ) )
import Control.Monad ( foldM )
import Control.Monad.Tools ( doWhile )
import Data.List ( isPrefixOf )
import Data.Char ( isSpace )

--------------------------------------------------------------------------------

mainGen :: [ String ] -> [ String ] -> IO String
mainGen args _ = do
	let ( expr, fns, errs ) = readOption args
	mapM_ putStr errs
	env0 <- foldM ( \e -> ( load e `fmap` ) . readFile ) initEnv fns
	( flip . flip maybe ) ( fmap fst . evalP env0 ) expr $ do
		runLoop "toyhaskell" env0 $ \env inp -> case inp of
			':' : cmd	-> runCmd cmd env
			_		-> do
				( ret, nenv ) <- evalP env inp
				putStr ret
				return nenv
		return "Leaving toyhaskell.\n"

runLoop :: String -> a -> ( a -> String -> IO a ) -> IO ()
runLoop name stat0 act = ( >> return () ) $ doWhile stat0 $ \stat -> do
	input <- putStr ( name ++ "> " ) >> hFlush stdout >> getLine
	if input `elem` [ ":quit", ":q" ]
		then return ( stat, False )
		else fmap ( , True ) $ act stat input

runCmd :: String -> Env -> IO Env
runCmd cmd env
	| "load " `isPrefixOf` cmd	= do
		let fn = dropWhile isSpace $ dropWhile ( not . isSpace ) cmd
		load env `fmap` readFile fn
	| otherwise			= do
		putStrLn $ "unknown command ':" ++ cmd  ++ "'"
		return env

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

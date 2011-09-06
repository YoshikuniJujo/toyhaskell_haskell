{-# LANGUAGE TupleSections #-}

module MainTools ( mainGen ) where

import ToyHaskell ( Env, primitives, load, evalP )

import System.IO ( hFlush, stdout )
import System.Console.GetOpt (
	getOpt, ArgOrder( .. ), OptDescr( .. ), ArgDescr( .. ) )
import Control.Monad ( foldM )
import Control.Monad.Tools ( doWhile )

--------------------------------------------------------------------------------

mainGen :: [ String ] -> [ String ] -> IO String
mainGen args _ = do
	let ( expr, fns, errs ) = readOption args
	mapM_ putStr errs
	env0 <- foldM ( \e -> ( load e `fmap` ) . readFile ) primitives fns
	( flip . flip maybe ) ( fmap fst . evalP env0 ) expr $ do
		runLoop "toyhaskell" env0 $ \env inp -> case inp of
			':' : str	-> let	cmd : as = words str in
						runCmd env cmd as
			_		-> do
				( ret, env' ) <- evalP env inp
				putStr ret
				return env'
		return "Leaving toyhaskell.\n"

runLoop :: String -> a -> ( a -> String -> IO a ) -> IO ()
runLoop prompt stat0 act = ( >> return () ) $ doWhile stat0 $ \stat -> do
	input <- putStr ( prompt ++ "> " ) >> hFlush stdout >> getLine
	if input `elem` [ ":quit", ":q" ]
		then return ( stat, False )
		else ( , True ) `fmap` act stat input

runCmd :: Env -> String -> [ String ] -> IO Env
runCmd env "load" args	= load env `fmap` readFile ( head args )
runCmd env cmd _	= do	putStrLn ( "unknown command ':" ++ cmd  ++ "'" )
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
	fromOps ( op : _ )	= case op of Expr e -> Just e

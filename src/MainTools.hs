{-# LANGUAGE TupleSections #-}

module MainTools (
	mainGen
) where

import Primitives ( initEnv )
import Eval ( eval )
import Parser ( toyParse, toyParseModule )
import Types ( Value( .. ), showValue, Env, setPats )

import System.IO ( hFlush, stdout )
import System.Console.GetOpt (
	getOpt, ArgOrder( .. ), OptDescr( .. ), ArgDescr( .. ) )
import Control.Monad ( foldM )
import Control.Monad.Tools ( doWhile )
import Data.List ( isPrefixOf )
import Data.Char ( isSpace )

--------------------------------------------------------------------------------

mainGen :: [ String ] -> [ String ] -> IO ()
mainGen args _ = do
	let	( expr, fns, errs ) = readOption args
	mapM_ putStr errs
	env0	<- foldM loadFile initEnv fns
	( flip . flip maybe ) ( showValue . eval env0 . toyParse ) expr $
		runLoop "toyhaskell" env0 $ \env inp -> case inp of
			':' : cmd	-> runCmd cmd env
			_		-> case eval env $ toyParse inp of
				Let ps	-> return $ setPats ps env
				ret	-> showValue ret >> return env

runLoop :: String -> a -> ( a -> String -> IO a ) -> IO ()
runLoop name stat0 act = do
	_ <- doWhile stat0 $ \stat -> do
		input <- prompt $ name ++ "> "
		if input `notElem` [ ":quit", ":q" ]
			then fmap ( , True ) $ act stat input
			else return ( stat, False )
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
		loadFile env fn
	| otherwise			= do
		putStrLn $ "unknown command ':" ++ cmd  ++ "'"
		return env

loadFile :: Env -> FilePath -> IO Env
loadFile env fn = do
	cnt <- readFile fn
	case eval env $ toyParseModule cnt of
		Let ps	-> return $ setPats ps env
		bad	-> error $ show bad

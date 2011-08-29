module MainTools (
	mainGen
) where

import Interact ( runLoop )
import Primitives ( initEnv )
import Eval ( eval )
import Parser ( toyParse, toyParseModule )
import Types ( Value( .. ), showValue, Env, setPats )

import System.Console.GetOpt (
	getOpt, ArgOrder( .. ), OptDescr( .. ), ArgDescr( .. ) )
import Control.Monad ( foldM )
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
			_		-> case eval env $
				toyParse inp of
				Let ps	-> return $ setPats ps env
				ret	-> showValue ret >> return env

data Option = Expr String

options :: [ OptDescr Option ]
options = [
	Option "e" [ ] ( ReqArg Expr "haskell expression" ) "run expression"
 ]

readOption ::
	[ String ] -> ( Maybe String, [ FilePath ], [ String ] )
readOption args = let
	( opts, fns, errs )	= getOpt RequireOrder options args
	( expr )		= fromOps opts in
	( expr, fns, errs )
	where
	fromOps [ ]		= Nothing
	fromOps ( op : _ )	= case op of
		Expr e		-> Just e

runCmd :: String -> Env -> IO Env
runCmd cmd env
	| "load" `isPrefixOf` cmd	= do
		let fn = dropWhile isSpace $ drop 4 cmd
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

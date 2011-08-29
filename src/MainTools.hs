module MainTools (
	mainGen
) where

import Interact ( runLoop )
import Primitives ( initEnv )
import Eval ( eval )
-- import Parser ( toyParse, toyParseModule, getOpTable )
import NewParser
-- import Lexer ( toyLex, SourceName )
import Types ( Value( .. ), showValue, Env, setPats )

import System.Console.GetOpt (
	getOpt, ArgOrder( .. ), OptDescr( .. ), ArgDescr( .. ) )
import Control.Monad ( foldM )
import Data.List ( isPrefixOf )
import Data.Char ( isSpace )

import Control.Monad.State ( evalState )

--------------------------------------------------------------------------------

parse :: String -> Value
parse input = toyParse `evalState` ( 0, [ ], ( 1, 1 ), input, [ ] )

parseModule :: String -> Value
parseModule input = toyParseModule `evalState` ( 0, [ ], ( 1, 1 ), input, [ ] )

mainGen :: [ String ] -> [ String ] -> IO ()
mainGen args _ = do
	let	( _, expr, fns, errs ) = readOption args
	mapM_ putStr errs
	env0	<- foldM loadFile initEnv fns
	flip ( flip . flip maybe ) expr
		( showValue . eval env0 . parse ) $
		runLoop "toyhaskell" env0 $ \env inp -> case inp of
			':' : cmd	-> runCmd cmd env
			_		-> case eval env $
				parse inp of
				Let ps	-> return $ setPats ps env
				ret	-> showValue ret >> return env

data Option = Expr String | OpTable String

options :: [ OptDescr Option ]
options = [
	Option "e" [ ] ( ReqArg Expr "haskell expression" ) "run expression",
	Option "" [ "op-table" ] ( ReqArg OpTable "operation table path" )
	"set operation table path"
 ]

readOption ::
	[ String ] -> ( Maybe FilePath, Maybe String, [ FilePath ], [ String ] )
readOption args = let
	( opts, fns, errs )	= getOpt RequireOrder options args
	( tbl, expr )		= fromOps opts in
	( tbl, expr, fns, errs )
	where
	fromOps [ ]		= ( Nothing, Nothing )
	fromOps ( op : ops )	= case op of
		Expr e		-> ( path, Just e )
		OpTable p	-> ( Just p, expr )
		where
		( path, expr ) = fromOps ops

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
	case eval env $ parseModule cnt of
		Let ps	-> return $ setPats ps env
		bad	-> error $ show bad

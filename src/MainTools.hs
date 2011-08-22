module MainTools (
	mainGen
) where

import Prelude hiding ( lex )

import Interact ( runLoop )
import Primitives ( initEnv )
import Eval ( eval )
import Parser ( toyParse, getOpTable )
import Preprocessor
import Lexer
import Types ( Value( .. ), showValue, Env, setPatsToEnv, Table )

import System.Console.GetOpt (
	getOpt, ArgOrder( .. ), OptDescr( .. ), ArgDescr( .. ) )
import Control.Monad ( foldM )
import Data.List ( isPrefixOf )
import Data.Char ( isSpace )

import Paths_toyhaskell ( getDataFileName )

import Text.ParserCombinators.Parsec.Expr

--------------------------------------------------------------------------------

getDefaultOpTable :: IO String
getDefaultOpTable = getDataFileName "operator-table.lst"

parse :: Table -> SourceName -> String -> Value
parse opLst fn = toyParse opLst fn . prep 0 [ ] . lex ( initialPos fn )

mainGen :: [ String ] -> [ String ] -> IO ()
mainGen args _ = do
	let	( tbl, expr, fns, errs ) = readOption args
	mapM_ putStr errs
	opLst_	<- maybe ( getDefaultOpTable >>= readFile ) readFile tbl
	let opLst = getOpTable opLst_
	env0	<- foldM ( loadFile opLst ) initEnv fns
	flip ( flip . flip maybe ) expr
		( showValue . eval env0 . parse opLst "" ) $
		runLoop "toyhaskell" env0 $ \env inp -> case inp of
			':' : cmd	-> runCmd opLst cmd env
			_		-> case eval env $
				parse opLst "<interactive>" inp of
				Let ps	-> return $ setPatsToEnv ps env
				ret	-> showValue ret >> return env

data Option = Expr String | OpTable String

options :: [ OptDescr Option ]
options = [
	Option "e" [ ] ( ReqArg Expr "haskell expression" ) "run expression",
	Option "" [ "op-table" ] ( ReqArg OpTable "operation table path" )
	"set operation table"
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

runCmd :: Table -> String -> Env -> IO Env
runCmd opLst cmd env
	| "load" `isPrefixOf` cmd	= do
		let fn = dropWhile isSpace $ drop 4 cmd
		loadFile opLst env fn
	| otherwise			= do
		putStrLn $ "unknown command ':" ++ cmd  ++ "'"
		return env

loadFile :: Table -> Env -> FilePath -> IO Env
loadFile opLst env fn = do
	cnt <- readFile fn
	case eval env $ parse opLst fn ( "let\n" ++ cnt ) of
		Let ps	-> return $ setPatsToEnv ps env
		bad	-> error $ show bad
